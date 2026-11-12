#!/usr/bin/env bash
# Regenerate index.json (format 2) from builds/*.json.
# Usage: build-index.sh <data-index dir>
# Schema: README.md on the data-index branch.
set -euo pipefail

dir="${1:?usage: build-index.sh <data-index dir>}"

shopt -s nullglob
files=("$dir"/builds/*.json)
if [ "${#files[@]}" -eq 0 ]; then
  echo "No builds found in $dir/builds." >&2
  exit 1
fi

jq -s '
  (map(.packages | keys) | add | unique) as $pkgs
  | {
      format: 2,
      packages: $pkgs,
      fingerprints: (
        group_by(.fingerprint)
        | map({
            key: .[0].fingerprint,
            value: {
              tag: (sort_by(.built_at) | .[0].tag),
              first_built_at: (map(.built_at) | min)
            }
          })
        | from_entries
      ),
      builds: (
        map(select(.source != "override"))
        | sort_by(.built_at, .inputs_hash) | reverse
        | map(. as $b | {
            inputs_hash, fingerprint, tag, source, built_at,
            packages: [$pkgs[] | {
              package: .,
              version: ($b.packages[.].version // null),
              remote_sha: ($b.packages[.].remote_sha // null)
            }]
          })
      )
    }
' "${files[@]}" > "$dir/index.json"
