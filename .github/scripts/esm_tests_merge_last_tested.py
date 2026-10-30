#!/usr/bin/env python3
"""
Merges the ``last_tested-*`` artifacts of an ``#approve-changes`` run into a checkout
of esm_tests_info, for a single commit.

The artifacts come from jobs that ran the code of the pull-request, so they are checked
before anything is written:

- Only regular files (no symlinks) under ``last_tested/<computer>/``, where
  ``<computer>`` is a directory that already exists in esm_tests_info.
- The same path in several artifacts (e.g. the compilation files of a model version,
  saved by every runscript of that version) must have the same content.

Nothing is written if any check fails (``ValueError`` listing all the problems).

Usage: esm_tests_merge_last_tested.py <artifacts_dir> <esm_tests_info_dir>
"""

import argparse
import pathlib
import shutil


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("artifacts_dir", type=pathlib.Path)
    parser.add_argument("esm_tests_info_dir", type=pathlib.Path)
    args = parser.parse_args()

    computers = {
        p.name for p in (args.esm_tests_info_dir / "last_tested").iterdir() if p.is_dir()
    }

    errors = []
    # ``{relative path: (source file, artifact name)}``
    files = {}
    # No directory if no job changed any file
    artifacts = args.artifacts_dir.iterdir() if args.artifacts_dir.is_dir() else []
    for artifact in sorted(p for p in artifacts if p.is_dir()):
        for path in sorted(artifact.rglob("*")):
            rel = path.relative_to(artifact)
            if path.is_symlink():
                errors.append(f"{artifact.name}: '{rel}' is a symlink")
                continue
            if path.is_dir():
                continue
            if not path.is_file():
                errors.append(f"{artifact.name}: '{rel}' is not a regular file")
                continue
            parts = rel.parts
            if len(parts) < 3 or parts[0] != "last_tested" or parts[1] not in computers:
                errors.append(
                    f"{artifact.name}: '{rel}' is not under 'last_tested/<computer>/' "
                    f"(computers: {', '.join(sorted(computers))})"
                )
                continue
            if rel in files:
                other_path, other_artifact = files[rel]
                if other_path.read_bytes() != path.read_bytes():
                    errors.append(
                        f"'{rel}' differs between {other_artifact} and {artifact.name}"
                    )
                continue
            files[rel] = (path, artifact.name)

    if errors:
        raise ValueError(
            "Not merging, problems found in the artifacts:\n"
            + "\n".join(f"  - {error}" for error in errors)
        )

    for rel, (path, _) in sorted(files.items()):
        target = args.esm_tests_info_dir / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(path, target)
    print(f"{len(files)} files merged into '{args.esm_tests_info_dir}'")


if __name__ == "__main__":
    main()
