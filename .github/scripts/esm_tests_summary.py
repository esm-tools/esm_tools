#!/usr/bin/env python3
"""
Writes a Markdown summary of the ``esm_tests`` results to stdout, from the JSON files
written with ``esm_tests --results-file`` (searched recursively in ``results_dir``):

- Problems: every test with errors or a failed job (not explained by differences),
  with the reason and a link to the job log. Always expanded.
- Differences: one collapsible section per computer, one per file with differences.

``--jobs`` is the output of ``gh api .../actions/runs/<id>/jobs`` (list of jobs with
``name``, ``conclusion`` and ``html_url``). It is used to link the job logs and to
report failed jobs that did not write any results (e.g. ``esm_tests`` crashed).

Usage: esm_tests_summary.py <results_dir> [--jobs jobs.json] >> "$GITHUB_STEP_SUMMARY"
"""

import argparse
import html
import json
import pathlib

# GitHub limits the step summary to 1 MiB
MAX_LINES_PER_FILE = 300
MAX_CHARS = 900_000


def md_escape(text):
    # So that e.g. ``_`` in file names is not rendered as italics
    return "".join(f"\\{c}" if c in "\\`*_[]<>#|" else c for c in text)


def code_block(text, lang="", indent=""):
    # Fence longer than any run of backticks in the text
    fence = "```"
    while fence in text:
        fence += "`"
    lines = [f"{fence}{lang}", *text.splitlines(), fence]
    return "\n".join(f"{indent}{line}" for line in lines)


def file_block(path, diff):
    lines = diff.rstrip("\n").splitlines()
    if len(lines) > MAX_LINES_PER_FILE:
        extra = len(lines) - MAX_LINES_PER_FILE
        lines = lines[:MAX_LINES_PER_FILE] + [f"... {extra} more lines, see the job log"]
    return (
        f"<details open><summary><code>{html.escape(path)}</code></summary>\n\n"
        f"{code_block(chr(10).join(lines), 'diff')}\n\n</details>\n"
    )


def load_jobs(jobs_file):
    """
    Returns ``{(computer, model, runscript): job}`` for the test jobs. Their names end
    with ``<computer> / <model> / <runscript>`` (see ``esm_tests_computer_workflow.yml``).
    """
    if not jobs_file:
        return {}
    jobs = {}
    for job in json.loads(pathlib.Path(jobs_file).read_text()):
        parts = job["name"].split(" / ")
        if len(parts) >= 4:
            jobs[tuple(parts[-3:])] = job
    return jobs


def problems_section(tests, jobs):
    out = []
    keys = sorted(set(tests) | set(jobs))
    for key in keys:
        test = tests.get(key)
        job = jobs.get(key, {})
        conclusion = job.get("conclusion")
        reasons = []
        if test is None:
            if conclusion in ("success", "skipped", None):
                continue
            reasons.append(
                f"Job {conclusion} without esm_tests results (esm_tests crashed or the "
                "job failed before), see the job log"
            )
        else:
            for error in test["errors"]:
                reason = md_escape(error["message"])
                if error.get("excerpt"):
                    reason += "\n\n" + code_block(error["excerpt"], indent="    ")
                reasons.append(reason)
            # Differences are only reported in the *Differences* section. With ``-e``
            # they also fail the job, so a failed job is only a problem here if there
            # are no differences explaining it
            if (
                not reasons
                and not test["differences"]
                and conclusion not in ("success", "skipped", None)
            ):
                reasons.append(f"Job {conclusion}, see the job log")
        if not reasons:
            continue
        link = f" · [job log]({job['html_url']})" if job.get("html_url") else ""
        out.append(f"- **{md_escape(' / '.join(key))}**{link}")
        out.extend(f"  - {reason}" for reason in reasons)

    n = sum(1 for line in out if line.startswith("- "))
    header = f"### Problems ({n} test{'s' if n != 1 else ''})\n"
    if not out:
        return [header, "None.\n"]
    return [header, "\n".join(out) + "\n"]


def differences_section(tests, size):
    out = ["### Differences\n", "`-` last_tested, `+` this run.\n"]
    truncated = False

    by_computer = {}
    for key, test in sorted(tests.items()):
        by_computer.setdefault(key[0], []).append(test)

    for computer, computer_tests in sorted(by_computer.items()):
        differ = [test for test in computer_tests if test["differences"]]
        out.append(
            f"<details><summary><b>{html.escape(computer)}</b>: {len(differ)} of "
            f"{len(computer_tests)} tests differ</summary>\n"
        )
        # Compilation files are shared by runscripts of the same model version, show
        # them only once
        shown = set()
        for test in differ:
            files = {p: d for p, d in test["differences"].items() if p not in shown}
            if not files:
                continue
            out.append(
                f"\n#### {md_escape(test['model'])} / {md_escape(test['runscript'])}\n"
            )
            for path in sorted(files):
                shown.add(path)
                block = file_block(path, files[path])
                if size + len(block) > MAX_CHARS:
                    truncated = True
                    break
                size += len(block)
                out.append(block)
            if truncated:
                break
        out.append("\n</details>\n")
        if truncated:
            break

    if truncated:
        out.append(
            "\n**Differences truncated (GitHub size limit), see the job logs for the "
            "rest.**\n"
        )
    return out


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("results_dir")
    parser.add_argument("--jobs", default=None)
    args = parser.parse_args()

    tests = {}
    for json_file in sorted(pathlib.Path(args.results_dir).rglob("*.json")):
        for test in json.loads(json_file.read_text()):
            tests[(test["computer"], test["model"], test["runscript"])] = test
    jobs = load_jobs(args.jobs)

    out = ["## esm_tests results\n"]
    out += problems_section(tests, jobs)
    out += differences_section(tests, sum(len(s) for s in out))
    print("\n".join(out))


if __name__ == "__main__":
    main()
