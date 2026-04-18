# Checklist Guide

A one-step-at-a-time checklist runner, shaped by the ideas in
Atul Gawande's *The Checklist Manifesto*.

You pick a checklist, and the app walks you through it one item at a time.
Phases create natural pause points; at the end of each phase you review
what you've done before moving on. After a run, you're prompted for a
short retrospective note so the checklist improves over time.

## Why the design looks like this

- **Phases with pause points.** A checklist isn't a flat list of 40 steps —
  it's grouped into short phases (5–9 killer items). The runner stops at
  each phase boundary so you can pause and confirm.
- **READ-DO vs DO-CONFIRM.** Each phase declares its mode. READ-DO reads
  each item out, one by one, before you do it (for novel/complex work).
  DO-CONFIRM lets you do a familiar phase from memory, then confirm at the
  pause point.
- **Killer items only.** The editor warns if a phase grows past 9 items.
- **Critical flag.** Items can be flagged critical; the runner highlights
  them.
- **Stop-the-line.** Any step can be aborted with a reason, which is logged.
- **Retro note.** End-of-run prompt captures what to improve; notes are
  attached to the checklist so iteration is visible.
- **Versioning.** Editing a checklist creates a new version. Past runs keep
  a snapshot of the version they ran against, so history is stable.

## Run it

```
cd checklist-guide
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
python app.py
```

Open http://127.0.0.1:5000.

## Data on disk

- `checklists/<slug>/v<n>.json` — one file per version.
- `state/runs/<run_id>.json` — one file per run (frozen phase snapshot,
  step-by-step log, abort reason if any, retro note).

Both are plain JSON — easy to version-control, diff, and port.

## Where agents fit later

Each item is a single, concrete action. It's a short jump from there to
"run this step with an agent": hand the item text plus the checklist's
context to an LLM, get a proposed action back, and let the human confirm.
For MVP the runner is manual — the shape of the data is already right for
that next step.
