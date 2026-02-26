Generate a concise session summary of everything accomplished in this Claude Code session.

The summary should be structured as follows:

```
## Session Summary — $CURRENT_DATE

**Project:** $CURRENT_PROJECT (from working directory name)
**Duration:** [note start/end if known, otherwise omit]

### What was done
- [bullet points of the main tasks completed]

### Files changed
- [list files that were created, modified, or deleted]

### Issues resolved
- [bugs fixed, errors addressed, or blockers overcome]

### Next steps / TODOs
- [any outstanding items mentioned or obvious follow-ups]
```

After writing the summary, append it to the daily log file at `~/claude-session-logs/YYYY-MM-DD.md` (using today's actual date). Create the file and `~/claude-session-logs/` directory if they don't exist.

Use the Bash tool to:
1. Run `mkdir -p ~/claude-session-logs` to ensure the directory exists
2. Append the formatted summary to `~/claude-session-logs/$(date +%Y-%m-%d).md`
3. Print a confirmation with the file path so the user knows where it was saved
