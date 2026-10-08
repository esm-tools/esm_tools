#!/bin/bash
#
# Install esm_tools from the mounted checkout into the venv at /opt/venv.
# /opt/venv is a named Docker volume, so this only does real work the first
# time (or after `--force`); the plugins esm_master pip-installs later persist
# there too.
#
# Three things differ from a plain `pip install -e .` on Python 3.12:
#   - ruamel.yaml.clib is pinned to 0.2.7 in setup.py, which does not build on
#     3.12; 0.2.8 is used instead
#   - setuptools is kept below 81, because esm_tools imports pkg_resources
#   - esm_tools locates its configs through a legacy esm-tools.egg-link file
#     that modern pip no longer writes

set -e

ESM_TOOLS="${ESM_TOOLS:-$HOME/esm_tools}"
VENV=/opt/venv

if [[ "$1" != "--force" && -x "$VENV/bin/esm_master" ]]; then
    exit 0
fi

if [[ ! -f "$ESM_TOOLS/setup.py" ]]; then
    echo "esm_tools checkout not found at $ESM_TOOLS" >&2
    exit 1
fi

echo "Installing esm_tools from $ESM_TOOLS into $VENV ..."
python3 -m venv "$VENV"
"$VENV/bin/pip" install -q --upgrade pip wheel "setuptools<81"

"$VENV/bin/python" - "$ESM_TOOLS/setup.py" > /tmp/esm_tools_requirements.txt <<'EOF'
import ast, sys
tree = ast.parse(open(sys.argv[1]).read())
for node in ast.walk(tree):
    if isinstance(node, ast.Assign) and getattr(node.targets[0], "id", "") == "requirements":
        for req in ast.literal_eval(node.value):
            print("ruamel.yaml.clib==0.2.8" if req.startswith("ruamel.yaml.clib") else req)
EOF
"$VENV/bin/pip" install -q -r /tmp/esm_tools_requirements.txt
"$VENV/bin/pip" install -q --no-deps --no-build-isolation -e "$ESM_TOOLS"

SITE=$("$VENV/bin/python" -c "import site; print(site.getsitepackages()[0])")
printf '%s/src\n../\n' "$ESM_TOOLS" > "$SITE/esm-tools.egg-link"

"$VENV/bin/esm_tools" --version
