"""File-based storage for checklists and runs.

Layout:
  checklists/<slug>/v<n>.json   one file per version; latest = max n
  state/runs/<run_id>.json      one file per run
"""

from __future__ import annotations

import json
import re
import uuid
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parent
CHECKLISTS_DIR = ROOT / "checklists"
RUNS_DIR = ROOT / "state" / "runs"

CHECKLISTS_DIR.mkdir(parents=True, exist_ok=True)
RUNS_DIR.mkdir(parents=True, exist_ok=True)


def now_iso() -> str:
    return datetime.now(timezone.utc).isoformat(timespec="seconds")


def slugify(text: str) -> str:
    s = re.sub(r"[^a-zA-Z0-9]+", "-", text.strip().lower()).strip("-")
    return s or "checklist"


def _read_json(path: Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as f:
        return json.load(f)


def _write_json(path: Path, data: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(".tmp")
    with tmp.open("w", encoding="utf-8") as f:
        json.dump(data, f, indent=2, ensure_ascii=False)
    tmp.replace(path)


# ---------- Checklists ----------

def list_checklists() -> list[dict[str, Any]]:
    out = []
    for d in sorted(CHECKLISTS_DIR.iterdir()):
        if not d.is_dir():
            continue
        latest = load_checklist(d.name)
        if latest:
            out.append(latest)
    return out


def _versions(slug: str) -> list[int]:
    d = CHECKLISTS_DIR / slug
    if not d.exists():
        return []
    nums = []
    for p in d.glob("v*.json"):
        m = re.match(r"v(\d+)\.json", p.name)
        if m:
            nums.append(int(m.group(1)))
    return sorted(nums)


def load_checklist(slug: str, version: int | None = None) -> dict[str, Any] | None:
    versions = _versions(slug)
    if not versions:
        return None
    v = version if version is not None else versions[-1]
    if v not in versions:
        return None
    path = CHECKLISTS_DIR / slug / f"v{v}.json"
    data = _read_json(path)
    data["available_versions"] = versions
    return data


def save_new_checklist(title: str, description: str, phases: list[dict[str, Any]]) -> dict[str, Any]:
    slug = slugify(title)
    # disambiguate if slug exists
    base = slug
    i = 2
    while (CHECKLISTS_DIR / slug).exists():
        slug = f"{base}-{i}"
        i += 1
    data = {
        "slug": slug,
        "version": 1,
        "title": title.strip(),
        "description": description.strip(),
        "created_at": now_iso(),
        "updated_at": now_iso(),
        "phases": _normalize_phases(phases),
        "retro_notes": [],
    }
    _write_json(CHECKLISTS_DIR / slug / "v1.json", data)
    return data


def save_new_version(slug: str, title: str, description: str, phases: list[dict[str, Any]]) -> dict[str, Any]:
    versions = _versions(slug)
    if not versions:
        raise FileNotFoundError(slug)
    prev = load_checklist(slug)
    assert prev is not None
    new_version = versions[-1] + 1
    data = {
        "slug": slug,
        "version": new_version,
        "title": title.strip(),
        "description": description.strip(),
        "created_at": prev.get("created_at", now_iso()),
        "updated_at": now_iso(),
        "phases": _normalize_phases(phases),
        "retro_notes": prev.get("retro_notes", []),
    }
    _write_json(CHECKLISTS_DIR / slug / f"v{new_version}.json", data)
    return data


def delete_checklist(slug: str) -> None:
    d = CHECKLISTS_DIR / slug
    if not d.exists():
        return
    for p in d.iterdir():
        p.unlink()
    d.rmdir()


def append_retro_note(slug: str, version: int, run_id: str, note: str) -> None:
    # retro notes are written onto the latest version file so iteration hints accumulate.
    latest = load_checklist(slug)
    if not latest:
        return
    path = CHECKLISTS_DIR / slug / f"v{latest['version']}.json"
    data = _read_json(path)
    data.setdefault("retro_notes", []).append({
        "at": now_iso(),
        "run_id": run_id,
        "ran_version": version,
        "note": note.strip(),
    })
    _write_json(path, data)


def _normalize_phases(phases: list[dict[str, Any]]) -> list[dict[str, Any]]:
    out = []
    for p in phases:
        items = []
        for it in p.get("items", []):
            text = (it.get("text") or "").strip()
            if not text:
                continue
            items.append({
                "id": it.get("id") or uuid.uuid4().hex[:8],
                "text": text,
                "critical": bool(it.get("critical", False)),
            })
        if not items:
            continue
        mode = p.get("mode", "read-do")
        if mode not in ("read-do", "do-confirm"):
            mode = "read-do"
        out.append({
            "name": (p.get("name") or "").strip() or "Phase",
            "mode": mode,
            "items": items,
        })
    return out


# ---------- Runs ----------

@dataclass
class RunCursor:
    phase: int
    item: int  # -1 means "at pause point / phase summary"


def start_run(slug: str) -> dict[str, Any]:
    cl = load_checklist(slug)
    if not cl:
        raise FileNotFoundError(slug)
    run_id = uuid.uuid4().hex[:12]
    data = {
        "id": run_id,
        "checklist_slug": slug,
        "checklist_version": cl["version"],
        "checklist_title": cl["title"],
        "started_at": now_iso(),
        "completed_at": None,
        "status": "in_progress",   # in_progress | completed | aborted
        "cursor": {"phase": 0, "item": 0},
        "phase_snapshot": cl["phases"],  # freeze at run-start for integrity
        "steps": [],
        "abort_reason": None,
        "retro_note": None,
    }
    _write_json(RUNS_DIR / f"{run_id}.json", data)
    return data


def load_run(run_id: str) -> dict[str, Any] | None:
    path = RUNS_DIR / f"{run_id}.json"
    if not path.exists():
        return None
    return _read_json(path)


def list_runs(limit: int = 50) -> list[dict[str, Any]]:
    files = sorted(RUNS_DIR.glob("*.json"), key=lambda p: p.stat().st_mtime, reverse=True)
    return [_read_json(p) for p in files[:limit]]


def _save_run(run: dict[str, Any]) -> None:
    _write_json(RUNS_DIR / f"{run['id']}.json", run)


def advance(run_id: str, note: str | None = None) -> dict[str, Any]:
    run = load_run(run_id)
    if not run or run["status"] != "in_progress":
        raise ValueError("run not active")
    cursor = run["cursor"]
    phase_idx = cursor["phase"]
    item_idx = cursor["item"]
    phases = run["phase_snapshot"]
    phase = phases[phase_idx]

    if item_idx == -1:
        # leaving pause point: advance into next phase
        phase_idx += 1
        if phase_idx >= len(phases):
            run["status"] = "completed"
            run["completed_at"] = now_iso()
            run["cursor"] = {"phase": phase_idx - 1, "item": -1}
        else:
            run["cursor"] = {"phase": phase_idx, "item": 0}
        _save_run(run)
        return run

    # record step completion
    run["steps"].append({
        "phase": phase_idx,
        "item": item_idx,
        "item_id": phase["items"][item_idx]["id"],
        "text": phase["items"][item_idx]["text"],
        "completed_at": now_iso(),
        "note": (note or "").strip() or None,
    })

    if item_idx + 1 < len(phase["items"]):
        run["cursor"] = {"phase": phase_idx, "item": item_idx + 1}
    else:
        # end of phase -> pause point
        run["cursor"] = {"phase": phase_idx, "item": -1}

    _save_run(run)
    return run


def go_back(run_id: str) -> dict[str, Any]:
    run = load_run(run_id)
    if not run or run["status"] != "in_progress":
        raise ValueError("run not active")
    if run["steps"]:
        last = run["steps"].pop()
        run["cursor"] = {"phase": last["phase"], "item": last["item"]}
        _save_run(run)
    return run


def abort(run_id: str, reason: str) -> dict[str, Any]:
    run = load_run(run_id)
    if not run:
        raise FileNotFoundError(run_id)
    if run["status"] != "in_progress":
        return run
    run["status"] = "aborted"
    run["abort_reason"] = reason.strip() or "(no reason given)"
    run["completed_at"] = now_iso()
    _save_run(run)
    return run


def save_retro(run_id: str, note: str) -> dict[str, Any]:
    run = load_run(run_id)
    if not run:
        raise FileNotFoundError(run_id)
    note = note.strip()
    run["retro_note"] = note or None
    _save_run(run)
    if note:
        append_retro_note(run["checklist_slug"], run["checklist_version"], run_id, note)
    return run
