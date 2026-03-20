"""Smoke tests for BetfairClient authentication and market listing.

These tests mock the betfairlightweight APIClient so no live network
connection is required. They verify:
  1. login() succeeds when certs exist and API responds normally
  2. login() raises FileNotFoundError when certs are missing
  3. list_uk_horse_racing_markets() returns > 0 results when API returns data
  4. list_uk_horse_racing_markets() shapes the response correctly
  5. Context manager (__enter__/__exit__) calls login/logout correctly
"""

import pytest
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import MagicMock, patch, call


class TestBetfairClientAuth:
    """Tests for SSL certificate login flow."""

    def test_login_succeeds_with_valid_certs(self, mock_api_client):
        """login() calls APIClient.login() and sets internal client."""
        with patch("src.betfair_client.betfairlightweight.APIClient") as MockAPIClient:
            mock_instance = MockAPIClient.return_value
            mock_api_client.login()

            MockAPIClient.assert_called_once_with(
                username="test@example.com",
                password="test_password",
                app_key="test_app_key",
                certs=str(mock_api_client._cert_path),
            )
            mock_instance.login.assert_called_once()
            assert mock_api_client._client is mock_instance

    def test_login_raises_when_cert_missing(self, tmp_path):
        """login() raises FileNotFoundError when certificate files are absent."""
        from src.betfair_client import BetfairClient

        empty_dir = tmp_path / "empty_certs"
        empty_dir.mkdir()

        client = BetfairClient(
            username="user",
            password="pass",
            app_key="key",
            cert_path=str(empty_dir),
        )
        with pytest.raises(FileNotFoundError, match="client-2048.crt"):
            client.login()

    def test_login_raises_when_key_missing(self, tmp_path):
        """login() raises FileNotFoundError when private key file is absent."""
        from src.betfair_client import BetfairClient

        partial_dir = tmp_path / "partial_certs"
        partial_dir.mkdir()
        (partial_dir / "client-2048.crt").write_text("FAKE_CERT")
        # .key file intentionally omitted

        client = BetfairClient(
            username="user",
            password="pass",
            app_key="key",
            cert_path=str(partial_dir),
        )
        with pytest.raises(FileNotFoundError, match="client-2048.key"):
            client.login()

    def test_client_property_raises_before_login(self, mock_api_client):
        """.client raises RuntimeError if login() has not been called."""
        with pytest.raises(RuntimeError, match="Not authenticated"):
            _ = mock_api_client.client

    def test_logout_clears_client(self, mock_api_client):
        """logout() calls APIClient.logout() and clears internal state."""
        mock_inner = MagicMock()
        mock_api_client._client = mock_inner

        mock_api_client.logout()

        mock_inner.logout.assert_called_once()
        assert mock_api_client._client is None

    def test_context_manager_calls_login_logout(self, mock_api_client):
        """Context manager calls login() on enter and logout() on exit."""
        with patch.object(mock_api_client, "login") as mock_login, \
             patch.object(mock_api_client, "logout") as mock_logout:
            with mock_api_client as ctx:
                assert ctx is mock_api_client
                mock_login.assert_called_once()
                mock_logout.assert_not_called()
            mock_logout.assert_called_once()


class TestMarketListing:
    """Tests for list_uk_horse_racing_markets()."""

    def _make_market_catalogue_item(
        self,
        market_id: str = "1.234567890",
        market_name: str = "Next Race Win",
        venue: str = "Ascot",
        start_offset_minutes: int = 60,
    ) -> MagicMock:
        """Build a mock MarketCatalogue object."""
        mc = MagicMock()
        mc.market_id = market_id
        mc.market_name = market_name
        mc.total_matched = 50000.0
        mc.market_start_time = datetime(2025, 6, 1, 14, 30, tzinfo=timezone.utc)
        mc.event = MagicMock()
        mc.event.venue = venue
        mc.event.country_code = "GB"
        return mc

    def test_returns_non_empty_list(self, mock_api_client):
        """list_uk_horse_racing_markets() returns > 0 results when API responds."""
        mock_inner = MagicMock()
        mock_api_client._client = mock_inner

        fake_markets = [
            self._make_market_catalogue_item(market_id=f"1.{i}") for i in range(5)
        ]
        mock_inner.betting.list_market_catalogue.return_value = fake_markets

        result = mock_api_client.list_uk_horse_racing_markets(max_results=20)

        assert len(result) > 0
        assert len(result) == 5

    def test_result_contains_required_keys(self, mock_api_client):
        """Each returned market dict has market_id, market_name, start_time, venue."""
        mock_inner = MagicMock()
        mock_api_client._client = mock_inner

        mock_inner.betting.list_market_catalogue.return_value = [
            self._make_market_catalogue_item(
                market_id="1.999000001",
                market_name="Test Win Market",
                venue="Newmarket",
            )
        ]

        result = mock_api_client.list_uk_horse_racing_markets()

        assert len(result) == 1
        market = result[0]
        assert market["market_id"] == "1.999000001"
        assert market["market_name"] == "Test Win Market"
        assert market["venue"] == "Newmarket"
        assert market["country_code"] == "GB"
        assert isinstance(market["start_time"], datetime)

    def test_passes_correct_filter_to_api(self, mock_api_client):
        """list_uk_horse_racing_markets() passes max_results to the API call."""
        mock_inner = MagicMock()
        mock_api_client._client = mock_inner
        mock_inner.betting.list_market_catalogue.return_value = []

        mock_api_client.list_uk_horse_racing_markets(max_results=10)

        call_kwargs = mock_inner.betting.list_market_catalogue.call_args.kwargs
        assert call_kwargs["max_results"] == 10

    def test_empty_response_returns_empty_list(self, mock_api_client):
        """Empty API response yields an empty list without error."""
        mock_inner = MagicMock()
        mock_api_client._client = mock_inner
        mock_inner.betting.list_market_catalogue.return_value = []

        result = mock_api_client.list_uk_horse_racing_markets()

        assert result == []
