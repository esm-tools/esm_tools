#!/usr/bin/env bash
#
# Read a boolean key out of an esm_tools finished_config YAML.
#
#   yaml_bool <file> <section> <key>
#
# Prints 1 for true/.true./yes/on/t/1, 0 for false/.false./no/off/f/0 (any case).
# Exit: 0 ok, 1 key absent, 2 value not a recognised boolean, 3 file missing.
# Diagnostics go to stderr, so $( ) capture stays clean.
#
# Only *direct* children of <section> are considered (two-space indent), so
# namelist_changes blocks that repeat the same key deeper down are ignored.
# Trailing "# provenance" comments and surrounding quotes are stripped.
#
# Portability: `function NAME {` + `typeset` is the one form that gives real
# local scope in BOTH bash and ksh93 -- `local` is not a ksh builtin, and
# `typeset` inside a POSIX-style `name()` function does not scope in ksh.
# `printf` rather than `echo` so no shell can mangle backslashes in the value.
#
function yaml_bool {
    typeset file=$1 section=$2 key=$3 raw lower

    [ -f "$file" ] || { printf 'yaml_bool: no such file: %s\n' "$file" >&2; return 3; }

    raw=$(awk -v sec="$section" -v key="$key" '
        BEGIN { sec_re = "^" sec ":[[:space:]]*$"; child_re = "^  " key ":" }
        # track the current top-level section (a key at column 0)
        /^[A-Za-z_][A-Za-z0-9_]*:/ { in_sec = ($0 ~ sec_re) }
        in_sec && $0 ~ child_re {
            sub(/^  [^:]*:[[:space:]]*/, "")      # strip "  key:"
            sub(/[[:space:]]*#.*$/, "")           # strip trailing comment
            gsub(/^[[:space:]]+|[[:space:]]+$/, "")
            gsub(/^["\047]|["\047]$/, "")        # strip surrounding quotes (\047 = squote)
            print
            exit
        }
    ' "$file")

    if [ -z "$raw" ]; then
        printf 'yaml_bool: %s.%s not found in %s\n' "$section" "$key" "$file" >&2
        return 1
    fi

    lower=$(printf '%s' "$raw" | tr '[:upper:]' '[:lower:]')
    case "$lower" in
        true|.true.|.t.|t|yes|on|1)   printf '1\n' ;;
        false|.false.|.f.|f|no|off|0) printf '0\n' ;;
        *) printf "yaml_bool: unrecognised boolean '%s' for %s.%s\n" \
               "$raw" "$section" "$key" >&2; return 2 ;;
    esac
}

# Allow running the file directly:  ./yaml_bool.sh <file> <section> <key>
# (BASH_SOURCE is unset under ksh, so this is simply skipped there.)
if [ -n "${BASH_SOURCE:-}" ] && [ "${BASH_SOURCE:-}" = "$0" ]; then
    yaml_bool "$@"
fi
