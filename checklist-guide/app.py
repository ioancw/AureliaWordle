"""Checklist guide — a READ-DO / DO-CONFIRM runner inspired by Gawande's
Checklist Manifesto. One step at a time, phased pause points,
stop-the-line, post-run retro."""

from __future__ import annotations

import json

from flask import Flask, abort, redirect, render_template, request, url_for

import storage

app = Flask(__name__)


def _parse_phases_form(form) -> list[dict]:
    """Parse the flat form fields produced by the editor into phases list.

    Fields:
      phase_name[0], phase_mode[0]
      item_text[0][0], item_critical[0][0], item_text[0][1], ...
    """
    phases: list[dict] = []
    p = 0
    while f"phase_name[{p}]" in form:
        name = form.get(f"phase_name[{p}]", "").strip()
        mode = form.get(f"phase_mode[{p}]", "read-do")
        items = []
        i = 0
        while f"item_text[{p}][{i}]" in form:
            text = form.get(f"item_text[{p}][{i}]", "").strip()
            critical = form.get(f"item_critical[{p}][{i}]") == "on"
            if text:
                items.append({"text": text, "critical": critical})
            i += 1
        if name or items:
            phases.append({"name": name, "mode": mode, "items": items})
        p += 1
    return phases


@app.route("/")
def index():
    checklists = storage.list_checklists()
    runs = storage.list_runs(limit=10)
    return render_template("index.html", checklists=checklists, runs=runs)


@app.route("/checklists/new", methods=["GET", "POST"])
def checklist_new():
    if request.method == "POST":
        title = request.form.get("title", "").strip()
        description = request.form.get("description", "").strip()
        phases = _parse_phases_form(request.form)
        if not title or not phases:
            return render_template(
                "checklist_form.html",
                mode="new",
                checklist={"title": title, "description": description, "phases": phases},
                error="Give your checklist a title and at least one item.",
            )
        cl = storage.save_new_checklist(title, description, phases)
        return redirect(url_for("checklist_view", slug=cl["slug"]))
    blank = {"title": "", "description": "", "phases": [
        {"name": "Phase 1", "mode": "read-do", "items": [{"text": "", "critical": False}]}
    ]}
    return render_template("checklist_form.html", mode="new", checklist=blank, error=None)


@app.route("/checklists/<slug>")
def checklist_view(slug):
    version_str = request.args.get("v")
    version = int(version_str) if version_str and version_str.isdigit() else None
    cl = storage.load_checklist(slug, version=version)
    if not cl:
        abort(404)
    return render_template("checklist_view.html", checklist=cl)


@app.route("/checklists/<slug>/edit", methods=["GET", "POST"])
def checklist_edit(slug):
    cl = storage.load_checklist(slug)
    if not cl:
        abort(404)
    if request.method == "POST":
        title = request.form.get("title", "").strip()
        description = request.form.get("description", "").strip()
        phases = _parse_phases_form(request.form)
        if not title or not phases:
            return render_template(
                "checklist_form.html",
                mode="edit",
                checklist={"slug": slug, "title": title, "description": description, "phases": phases},
                error="Give your checklist a title and at least one item.",
            )
        cl = storage.save_new_version(slug, title, description, phases)
        return redirect(url_for("checklist_view", slug=cl["slug"]))
    return render_template("checklist_form.html", mode="edit", checklist=cl, error=None)


@app.route("/checklists/<slug>/delete", methods=["POST"])
def checklist_delete(slug):
    storage.delete_checklist(slug)
    return redirect(url_for("index"))


@app.route("/runs", methods=["POST"])
def run_start():
    slug = request.form.get("slug", "").strip()
    if not slug:
        abort(400)
    run = storage.start_run(slug)
    return redirect(url_for("run_view", run_id=run["id"]))


@app.route("/runs/<run_id>")
def run_view(run_id):
    run = storage.load_run(run_id)
    if not run:
        abort(404)
    cursor = run["cursor"]
    phases = run["phase_snapshot"]
    state = _run_state(run)
    context = {"run": run, "phases": phases, "cursor": cursor, "state": state}
    if state == "retro_prompt":
        return render_template("run_complete.html", **context)
    if state == "aborted":
        return render_template("run_aborted.html", **context)
    if state == "pause":
        phase = phases[cursor["phase"]]
        is_last = cursor["phase"] + 1 >= len(phases)
        return render_template("run_pause.html", phase=phase, is_last=is_last, **context)
    # step
    phase = phases[cursor["phase"]]
    item = phase["items"][cursor["item"]]
    progress = _progress(run)
    return render_template("run_step.html", phase=phase, item=item, progress=progress, **context)


def _run_state(run) -> str:
    if run["status"] == "aborted":
        return "aborted"
    if run["status"] == "completed":
        return "retro_prompt"
    if run["cursor"]["item"] == -1:
        return "pause"
    return "step"


def _progress(run) -> dict:
    total = sum(len(p["items"]) for p in run["phase_snapshot"])
    done = len(run["steps"])
    return {"done": done, "total": total, "pct": int(100 * done / total) if total else 0}


@app.route("/runs/<run_id>/advance", methods=["POST"])
def run_advance(run_id):
    note = request.form.get("note", "")
    storage.advance(run_id, note=note or None)
    return redirect(url_for("run_view", run_id=run_id))


@app.route("/runs/<run_id>/back", methods=["POST"])
def run_back(run_id):
    storage.go_back(run_id)
    return redirect(url_for("run_view", run_id=run_id))


@app.route("/runs/<run_id>/abort", methods=["POST"])
def run_abort(run_id):
    reason = request.form.get("reason", "")
    storage.abort(run_id, reason)
    return redirect(url_for("run_view", run_id=run_id))


@app.route("/runs/<run_id>/retro", methods=["POST"])
def run_retro(run_id):
    note = request.form.get("note", "")
    storage.save_retro(run_id, note)
    return redirect(url_for("run_view", run_id=run_id))


@app.route("/runs-all")
def runs_all():
    runs = storage.list_runs(limit=200)
    return render_template("runs_list.html", runs=runs)


@app.template_filter("datetimefmt")
def datetimefmt(value):
    if not value:
        return ""
    return value.replace("T", " ").replace("+00:00", "Z")


@app.template_filter("tojson_pretty")
def tojson_pretty(value):
    return json.dumps(value, indent=2)


if __name__ == "__main__":
    app.run(host="127.0.0.1", port=5000, debug=True)
