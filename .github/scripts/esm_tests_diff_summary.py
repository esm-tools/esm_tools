#!/usr/bin/env python3
"""
Writes a Markdown summary of the ``esm_tests`` differences to stdout, from the JSON
files written with ``esm_tests --diff-file`` (searched recursively in the given
directory). One collapsible section per computer, one per file with differences.

Usage: esm_tests_diff_summary.py <dir> >> "$GITHUB_STEP_SUMMARY"
"""

import html
import json
import pathlib
import sys

# GitHub limits the step summary to 1 MiB
MAX_LINES_PER_FILE = 300
MAX_CHARS = 900_000


def code_block(text):
    # Fence longer than any run of backticks in the text
    fence = "```"
    while fence in text:
        fence += "`"
    return f"{fence}diff\n{text}\n{fence}"


def file_block(path, diff):
    lines = diff.rstrip("\n").splitlines()
    if len(lines) > MAX_LINES_PER_FILE:
        extra = len(lines) - MAX_LINES_PER_FILE
        lines = lines[:MAX_LINES_PER_FILE] + [f"... {extra} more lines, see the job log"]
    return (
        f"<details open><summary><code>{html.escape(path)}</code></summary>\n\n"
        f"{code_block(chr(10).join(lines))}\n\n</details>\n"
    )


def main():
    tests = []
    for json_file in sorted(pathlib.Path(sys.argv[1]).rglob("*.json")):
        tests.extend(json.loads(json_file.read_text()))

    by_computer = {}
    for test in tests:
        by_computer.setdefault(test["computer"], []).append(test)

    n_differ = sum(1 for test in tests if test["differences"])
    out = [
        "## esm_tests differences\n",
        f"{n_differ} of {len(tests)} tests have differences "
        "(`-` last_tested, `+` this run).\n",
    ]
    size = sum(len(s) for s in out)
    truncated = False

    for computer in sorted(by_computer):
        computer_tests = sorted(
            by_computer[computer], key=lambda t: (t["model"], t["runscript"])
        )
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
            out.append(f"\n#### {test['model']} / {test['runscript']}\n")
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
            "\n**Summary truncated (GitHub size limit), see the job logs for the "
            "remaining differences.**\n"
        )

    print("\n".join(out))


if __name__ == "__main__":
    main()
