"""Tests for the logging setup module."""

import sys
from pathlib import Path

import pytest
from loguru import logger


def test_logging_creates_log_directory(tmp_path):
    """setup_logging() creates the log directory if it does not exist."""
    from src.logging_setup import setup_logging

    log_dir = tmp_path / "custom_logs"
    assert not log_dir.exists()

    setup_logging(log_dir=str(log_dir), log_level="WARNING")

    assert log_dir.exists()


def test_logging_does_not_crash_with_debug_level(tmp_path):
    """setup_logging() completes without error at DEBUG level."""
    from src.logging_setup import setup_logging

    # Should not raise
    setup_logging(log_dir=str(tmp_path / "logs"), log_level="DEBUG")
