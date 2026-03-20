"""BetfairClient — thin wrapper around betfairlightweight for authenticated access.

Handles SSL certificate login (non-interactive) and provides convenience methods
used throughout the bot. All network calls are logged via loguru.
"""

from __future__ import annotations

import os
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

import betfairlightweight
from betfairlightweight import APIClient
from betfairlightweight.filters import market_filter, time_range
from loguru import logger


class BetfairClient:
    """Authenticated Betfair API client using SSL certificate login.

    Args:
        username: Betfair account username.
        password: Betfair account password.
        app_key: Betfair application key (delayed or live).
        cert_path: Directory containing ``client-2048.crt`` and ``client-2048.key``.
    """

    def __init__(
        self,
        username: str,
        password: str,
        app_key: str,
        cert_path: str,
    ) -> None:
        self._username = username
        self._password = password
        self._app_key = app_key
        self._cert_path = Path(cert_path)
        self._client: APIClient | None = None

    # ------------------------------------------------------------------
    # Authentication
    # ------------------------------------------------------------------

    def login(self) -> None:
        """Authenticate using SSL certificate (non-interactive).

        Raises:
            FileNotFoundError: If the certificate files are missing.
            betfairlightweight.exceptions.APIError: If authentication fails.
        """
        crt = self._cert_path / "client-2048.crt"
        key = self._cert_path / "client-2048.key"

        for path in (crt, key):
            if not path.exists():
                raise FileNotFoundError(
                    f"Betfair SSL certificate file not found: {path}. "
                    f"Generate via the Betfair developer portal and place in {self._cert_path}."
                )

        logger.info("Authenticating with Betfair API (cert login)...")

        self._client = betfairlightweight.APIClient(
            username=self._username,
            password=self._password,
            app_key=self._app_key,
            certs=str(self._cert_path),
        )
        self._client.login()
        logger.info("Betfair authentication successful. session_token acquired.")

    def logout(self) -> None:
        """Logout and invalidate the current session token."""
        if self._client:
            try:
                self._client.logout()
                logger.info("Logged out of Betfair API.")
            except Exception as exc:
                logger.warning("Error during logout (ignored): {}", exc)
            finally:
                self._client = None

    @property
    def client(self) -> APIClient:
        """Return the authenticated betfairlightweight client.

        Raises:
            RuntimeError: If ``login()`` has not been called.
        """
        if self._client is None:
            raise RuntimeError("Not authenticated. Call login() first.")
        return self._client

    def is_authenticated(self) -> bool:
        """Return True if a session token is held."""
        return self._client is not None and bool(
            getattr(self._client.session_token, "__class__", None)
        )

    # ------------------------------------------------------------------
    # Market listing
    # ------------------------------------------------------------------

    def list_uk_horse_racing_markets(self, max_results: int = 20) -> list[dict[str, Any]]:
        """Return upcoming UK horse racing WIN markets sorted by start time.

        Args:
            max_results: Maximum number of markets to return.

        Returns:
            List of dicts with keys: ``market_id``, ``market_name``,
            ``start_time``, ``venue``, ``country_code``, ``total_matched``.
        """
        logger.info("Fetching up to {} UK horse racing WIN markets...", max_results)

        now = datetime.now(tz=timezone.utc)

        market_catalogue = self.client.betting.list_market_catalogue(
            filter=market_filter(
                event_type_ids=["7"],          # 7 = Horse Racing
                market_countries=["GB"],        # GB = United Kingdom
                market_type_codes=["WIN"],
                market_start_time=time_range(from_=now.isoformat()),
            ),
            market_projection=["MARKET_START_TIME", "RUNNER_DESCRIPTION", "EVENT", "COMPETITION"],
            sort="FIRST_TO_START",
            max_results=max_results,
            locale="en",
        )

        markets = []
        for mc in market_catalogue:
            markets.append({
                "market_id": mc.market_id,
                "market_name": mc.market_name,
                "start_time": mc.market_start_time,
                "venue": mc.event.venue if mc.event else None,
                "country_code": mc.event.country_code if mc.event else "GB",
                "total_matched": mc.total_matched,
            })

        logger.info(
            "Retrieved {} UK horse racing WIN markets. Next start: {}",
            len(markets),
            markets[0]["start_time"] if markets else "N/A",
        )
        return markets

    # ------------------------------------------------------------------
    # Context manager support
    # ------------------------------------------------------------------

    def __enter__(self) -> "BetfairClient":
        self.login()
        return self

    def __exit__(self, *_: Any) -> None:
        self.logout()
