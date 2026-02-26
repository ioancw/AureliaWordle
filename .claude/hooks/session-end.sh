#!/usr/bin/env bash
# session-end.sh — Runs when a Claude Code session stops.
# Appends a minimal timestamped entry to today's session log so that
# every session is recorded even if /session-summary was not run.

LOG_DIR="$HOME/claude-session-logs"
LOG_FILE="$LOG_DIR/$(date +%Y-%m-%d).md"

mkdir -p "$LOG_DIR"

# Only write the auto-marker if the file doesn't already have an entry
# for this session (i.e., /session-summary was not used).
MARKER="<!-- session-end $(date +%H:%M:%S) -->"

# Append a lightweight record
{
  echo ""
  echo "---"
  echo "<!-- auto-recorded by session-end hook -->"
  echo "**Session ended:** $(date '+%Y-%m-%d %H:%M:%S')"
  echo "**Project:** $(basename "$PWD")"
  echo "$MARKER"
  echo ""
} >> "$LOG_FILE"
