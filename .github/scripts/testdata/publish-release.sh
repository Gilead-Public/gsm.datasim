#!/usr/bin/env bash
# Create the data-<fp12> pre-release for a built bundle, or reuse it if it
# already exists.
# Usage: publish-release.sh <bundle dir> <work dir>
# Env: GH_TOKEN, GITHUB_REPOSITORY, TARGET_SHA, TRIGGERED_BY
# Writes `tag` and `created` to $GITHUB_OUTPUT when set.
# An existing release is reused only if it is published (not a draft) and has
# every expected asset; otherwise the run fails for manual recovery.
set -euo pipefail

bundle="${1:?usage: publish-release.sh <bundle dir> <work dir>}"
work="${2:?usage: publish-release.sh <bundle dir> <work dir>}"
: "${GITHUB_REPOSITORY:?}" "${TARGET_SHA:?}" "${TRIGGERED_BY:?}"

snapshot="$bundle/snapshot.json"
fingerprint="$(jq -er '.metadata.fingerprint' "$snapshot")"
inputs_hash="$(jq -er '.metadata.inputs_hash' "$snapshot")"
tag="data-${fingerprint:0:12}"
zip_name="$tag.zip"

# Per-object assets: named exactly objects$<name>$file and taken from the
# bundle directory, so they are byte-identical to the copies in the zip.
object_files=()
while IFS= read -r file; do
  case "$file" in
    */* | "" | snapshot.json)
      echo "Unexpected object file name: '$file'" >&2
      exit 1
      ;;
  esac
  object_files+=("$file")
done < <(jq -er '.objects[].file' "$snapshot")
expected_names=("$zip_name" "$zip_name.sha256" snapshot.json "${object_files[@]}")

emit() {
  if [ -n "${GITHUB_OUTPUT:-}" ]; then
    printf 'tag=%s\ncreated=%s\n' "$tag" "$1" >> "$GITHUB_OUTPUT"
  fi
}

# Prints nothing if the release doesn't exist. Exits non-zero if it exists but
# is a draft or is missing assets.
check_existing() {
  local info
  if ! info="$(gh release view "$tag" -R "$GITHUB_REPOSITORY" \
    --json isDraft,assets 2> /dev/null)"; then
    return 0
  fi
  if [ "$(jq -r '.isDraft' <<< "$info")" != "false" ]; then
    echo "::error::Release $tag exists but is a draft (likely a failed upload). Delete it and re-run." >&2
    return 1
  fi
  local missing
  missing="$(jq -r --args '[.assets[].name] as $have
    | $ARGS.positional - $have | .[]' "${expected_names[@]}" <<< "$info")"
  if [ -n "$missing" ]; then
    echo "::error::Release $tag exists but is missing assets: $(tr '\n' ' ' <<< "$missing")Delete it and re-run." >&2
    return 1
  fi
  echo "exists"
}

state="$(check_existing)"
if [ "$state" = "exists" ]; then
  echo "Release $tag already exists with all assets; reusing it unchanged."
  emit false
  exit 0
fi

mkdir -p "$work"
(cd "$bundle" && zip -X -q -r "$work/$zip_name" .)
(cd "$work" && sha256sum "$zip_name" > "$zip_name.sha256")

assets=("$work/$zip_name" "$work/$zip_name.sha256" "$snapshot")
for file in "${object_files[@]}"; do
  assets+=("$bundle/$file")
done

notes="$work/notes.md"
{
  echo "Test data bundle \`$tag\` (data fingerprint \`$fingerprint\`)."
  echo
  echo "The tag is derived from the data fingerprint, so this release holds exactly this data. Builds from other package versions that produce identical data reuse it; see the \`data-index\` branch for every package set that maps here."
  echo
  echo "Triggered by: $TRIGGERED_BY"
  echo
  echo "First built from (inputs hash \`$inputs_hash\`):"
  echo
  echo "| Package | Version | remote_sha |"
  echo "|---|---|---|"
  jq -r '.packages | to_entries | sort_by(.key)[]
    | "| \(.key) | \(.value.version) | `\(.value.remote_sha // "NA")` |"' \
    "$snapshot"
  echo
  echo "Assets: \`$zip_name\` (bundle), \`$zip_name.sha256\`, a standalone \`snapshot.json\`, and one file per \`snapshot.json\` object."
} > "$notes"

if ! gh release create "$tag" "${assets[@]}" \
  -R "$GITHUB_REPOSITORY" \
  --target "$TARGET_SHA" \
  --title "$tag" \
  --notes-file "$notes" \
  --prerelease \
  --latest=false; then
  # Another branch's build may have created the same release meanwhile; reuse
  # it only if it is complete.
  sleep 10
  state="$(check_existing)"
  if [ "$state" = "exists" ]; then
    echo "Release $tag was created concurrently; reusing it."
    emit false
    exit 0
  fi
  echo "::error::Could not create release $tag." >&2
  exit 1
fi

# gh uploads assets to a draft, then publishes; confirm the result.
state="$(check_existing)"
if [ "$state" != "exists" ]; then
  echo "::error::Release $tag was not published with all assets." >&2
  exit 1
fi
emit true
