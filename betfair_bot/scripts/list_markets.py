#!/usr/bin/env python3
"""Script: list upcoming UK horse racing WIN markets.

Lists the next 20 UK horse racing WIN markets with market_id and start time.

Usage:
    python scripts/list_markets.py

Required environment variables (see .env.example):
    BETFAIR_USERNAME, BETFAIR_PASSWORD, BETFAIR_APP_KEY, CERT_PATH
"""

import sys
from pathlib import Path

# Allow running from the project root without installing the package
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.config import Config
from src.betfair_client import BetfairClient
from src.logging_setup import setup_logging
from loguru import logger


def main() -> None:
    setup_logging(log_dir=Config.LOG_DIR, log_level=Config.LOG_LEVEL)

    with BetfairClient(
        username=Config.BETFAIR_USERNAME,
        password=Config.BETFAIR_PASSWORD,
        app_key=Config.BETFAIR_APP_KEY,
        cert_path=Config.CERT_PATH,
    ) as client:
        markets = client.list_uk_horse_racing_markets(max_results=20)

    if not markets:
        logger.warning("No upcoming UK horse racing WIN markets found.")
        return

    print(f"\n{'='*72}")
    print(f"  Upcoming UK Horse Racing WIN Markets ({len(markets)} found)")
    print(f"{'='*72}")
    header = f"  {'Market ID':<16}  {'Start Time (UTC)':<22}  {'Venue':<20}  Market Name"
    print(header)
    print(f"  {'-'*14}  {'-'*20}  {'-'*18}  {'-'*20}")

    for m in markets:
        start = m["start_time"].strftime("%Y-%m-%d %H:%M:%S") if m["start_time"] else "Unknown"
        venue = (m["venue"] or "Unknown")[:18]
        print(f"  {m['market_id']:<16}  {start:<22}  {venue:<20}  {m['market_name']}")

    print(f"{'='*72}\n")


if __name__ == "__main__":
    main()
