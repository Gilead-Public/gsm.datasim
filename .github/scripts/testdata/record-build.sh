#!/usr/bin/env bash
# Record a build on a data-index checkout: write builds/<inputs_hash>.json,
# regenerate index.json, then commit and push (fetch/rebase/retry).
# Usage: record-build.sh <snapshot.json> <data-index checkout>
# Env: TAG, SOURCE, TRIGGERED_BY; optional INDEX_BRANCH (default data-index).
# Fails if builds/<inputs_hash>.json exists with a different fingerprint.
set -euo pipefail

snapshot="${1:?usage: record-build.sh <snapshot.json> <data-index checkout>}"
index_dir="${2:?usage: record-build.sh <snapshot.json> <data-index checkout>}"
: "${TAG:?}" "${SOURCE:?}" "${TRIGGERED_BY:?}"
branch="${INDEX_BRANCH:-data-index}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

inputs_hash="$(jq -er '.metadata.inputs_hash' "$snapshot")"
fingerprint="$(jq -er '.metadata.fingerprint' "$snapshot")"
rel="builds/$inputs_hash.json"
entry="$(mktemp)"

jq -S \
  --arg tag "$TAG" --arg source "$SOURCE" --arg triggered_by "$TRIGGERED_BY" '
  {
    format: 1,
    inputs_hash: .metadata.inputs_hash,
    fingerprint: .metadata.fingerprint,
    tag: $tag,
    source: $source,
    built_at: .created,
    triggered_by: $triggered_by,
    packages: .packages
  }' "$snapshot" > "$entry"

# Returns 0 if there is something to push, 1 if the index is already current.
apply() {
  local path="$index_dir/$rel"
  if [ -f "$path" ]; then
    local existing
    existing="$(jq -r '.fingerprint' "$path")"
    if [ "$existing" != "$fingerprint" ]; then
      echo "::error::$rel already records fingerprint $existing, but this build produced $fingerprint. The build is not reproducible." >&2
      exit 1
    fi
    echo "$rel already records this fingerprint; keeping the original entry."
  else
    mkdir -p "$index_dir/builds"
    cp "$entry" "$path"
  fi
  bash "$here/build-index.sh" "$index_dir"
  git -C "$index_dir" add builds index.json
  if git -C "$index_dir" diff --cached --quiet; then
    return 1
  fi
  git -C "$index_dir" commit -q -m "Record build $inputs_hash ($TAG)"
}

for attempt in 1 2 3 4 5; do
  git -C "$index_dir" fetch -q origin "$branch"
  git -C "$index_dir" reset -q --hard "origin/$branch"
  if ! apply; then
    echo "Nothing to commit; data-index is already up to date."
    exit 0
  fi
  if git -C "$index_dir" push -q origin "HEAD:$branch"; then
    echo "Recorded $rel on $branch."
    exit 0
  fi
  echo "Push failed (attempt $attempt); retrying."
  sleep $((attempt * 5))
done
echo "::error::Could not push to $branch after 5 attempts." >&2
exit 1
