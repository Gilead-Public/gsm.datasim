# data-index

Index of gsm.datasim test data bundles. This branch contains data only (no
code or workflows) and is updated by automation; don't edit it by hand.

- `builds/<inputs_hash>.json`: one file per set of building packages. It
  records which data (`fingerprint`) those exact package versions produce,
  and the release (`tag`) that holds it. These files never change once
  written. Includes `source == "override"` builds.
- `index.json`: a summary of `builds/`, regenerated on each update, for
  falling back when no exact `inputs_hash` match exists.

`inputs_hash` and `fingerprint` are defined in
`?gsm.datasim::build_testdata_bundle`. See Gilead-Public/gsm.datasim#153.

## Releases

Bundles are attached to pre-releases tagged `data-<first 12 hex of
fingerprint>`, one per fingerprint. Each release has:

- `data-<fp12>.zip` and `data-<fp12>.zip.sha256`: the whole bundle.
- `snapshot.json`: the bundle's refresh snapshot.
- One asset per entry in `snapshot.json` `objects`, named exactly
  `objects$<name>$file` and byte-identical to the copy in the zip, so
  `objects$<name>$sha256` verifies either. Download URL:
  `https://github.com/Gilead-Public/gsm.datasim/releases/download/<tag>/<file>`.

## `index.json` schema (format 2)

| Field | Type | Description |
|---|---|---|
| `format` | integer | Schema version, `2`. |
| `packages` | array of string | Every package recorded per build, sorted. |
| `fingerprints` | object | Map of fingerprint to `{tag, first_built_at}`. One release per fingerprint. |
| `builds` | array of object | Sorted by `built_at`, newest first. Excludes `source == "override"`. |
| `builds[].inputs_hash` | string | SHA-256 hex; name of the file in `builds/`. |
| `builds[].fingerprint` | string | SHA-256 hex of the bundle data. |
| `builds[].tag` | string | Release tag holding the data. |
| `builds[].source` | string | `dev`, `main`, etc. |
| `builds[].built_at` | string | UTC ISO 8601 timestamp. |
| `builds[].packages` | array of object | One record per entry in top-level `packages`, in the same order: `{package, version, remote_sha}`. `version` and `remote_sha` are `null` if the package was not installed for that build. |

Timestamps are UTC (`YYYY-MM-DDTHH:MM:SSZ`), so they sort as strings.
