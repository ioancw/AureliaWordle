"""Tests for configuration loading."""

import os
import importlib
import pytest


def test_config_loads_from_env(monkeypatch, tmp_path):
    """Config reads required values from environment variables."""
    cert_dir = tmp_path / "certs"
    cert_dir.mkdir()

    monkeypatch.setenv("BETFAIR_USERNAME", "alice@test.com")
    monkeypatch.setenv("BETFAIR_PASSWORD", "hunter2")
    monkeypatch.setenv("BETFAIR_APP_KEY", "myAppKey")
    monkeypatch.setenv("CERT_PATH", str(cert_dir))

    # Re-import to pick up monkeypatched env vars
    import src.config as config_module
    importlib.reload(config_module)
    Config = config_module.Config

    assert Config.BETFAIR_USERNAME == "alice@test.com"
    assert Config.BETFAIR_PASSWORD == "hunter2"
    assert Config.BETFAIR_APP_KEY == "myAppKey"
    assert Config.CERT_PATH == str(cert_dir)


def test_cert_paths_are_constructed_correctly(monkeypatch, tmp_path):
    """Config.cert_crt() and cert_key() return correct file paths."""
    cert_dir = tmp_path / "certs"
    cert_dir.mkdir()

    monkeypatch.setenv("BETFAIR_USERNAME", "user")
    monkeypatch.setenv("BETFAIR_PASSWORD", "pass")
    monkeypatch.setenv("BETFAIR_APP_KEY", "key")
    monkeypatch.setenv("CERT_PATH", str(cert_dir))

    import src.config as config_module
    importlib.reload(config_module)
    Config = config_module.Config

    assert Config.cert_crt().endswith("client-2048.crt")
    assert Config.cert_key().endswith("client-2048.key")
    assert str(cert_dir) in Config.cert_crt()
