# Static preview

These HTML files are self-contained snapshots of the app's pages (CSS inlined, forms disabled). They let you view the app's visual design without running Flask.

To see them rendered, open either:

- Raw (GitHub shows source, not rendered): https://github.com/ioancw/aureliawordle/tree/claude/checklist-step-guide-app-XzQHa/checklist-guide/preview
- Rendered via htmlpreview (proxy): prefix the raw file URL with `https://htmlpreview.github.io/?` — for example:

  https://htmlpreview.github.io/?https://raw.githubusercontent.com/ioancw/aureliawordle/claude/checklist-step-guide-app-XzQHa/checklist-guide/preview/index.html

Files:
- `index.html` — landing page / checklist list
- `editor.html` — new-checklist editor
- `run_step.html` — mid-run step view (single step, progress bar)
- `run_complete.html` — retro/finish screen with run log
- `runs_all.html` — all runs history

Forms are disabled in the preview (action="#"); to actually use the app, run Flask locally (see parent README).
