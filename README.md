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
- `snapshot.json`: the bundle's refresh snapshot (format 2).
- One asset per entry in `snapshot.json` `objects` (the RDS files), named
  exactly `objects$<name>$file` and byte-identical to the copy in the zip.
  After `readRDS()`, the object verifies against `objects$<name>$digest`
  (`gsm.core::fresh_digest()`). Download URL:
  `https://github.com/Gilead-Public/gsm.datasim/releases/download/<tag>/<file>`.

The zip also contains CSV copies of the tables. They are not in `objects` or
the fingerprint, so they are not separate release assets.

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

## When data is published

The `testdata-build` workflow (`.github/workflows/testdata-build.yaml` on
`dev`/`main`) builds the data. It runs only when dispatched, normally by the
`testdata.yaml` caller installed in each gsm repo (see the `actions` branch).
When a repo that feeds the data (listed in `packages` in `index.json`) merges
to `dev` or `main`, its caller dispatches `testdata-build` in gsm.datasim at
the same branch, passing `triggered_by` (`<owner>/<repo>@<sha>`). gsm.datasim's
own merges go through the same caller. The build runs in gsm.datasim and
installs gsm.datasim at the dispatched branch together with its `Remotes`, runs
`build_testdata_bundle()` (failing if any package lacks a `RemoteSha`), then:

1. Creates the `data-<fp12>` pre-release if it doesn't exist yet. Releases are
   never marked "Latest" and are never modified once created.
2. Writes `builds/<inputs_hash>.json` and regenerates `index.json` here. A
   build whose data is unchanged adds only a `builds/` entry pointing at the
   existing release. Rebuilding identical package versions changes nothing. If
   the same `inputs_hash` produces a different fingerprint, the run fails
   because the build isn't reproducible.

Builds run one at a time per branch, whichever repo dispatched them.
`triggered_by` in `builds/` records which commit started each build.

To build manually, run `testdata-build` from the Actions tab ("Run workflow",
choosing `dev` or `main`), or:

```sh
gh workflow run testdata-build.yaml -R Gilead-Public/gsm.datasim --ref dev \r
  -f triggered_by="manual build by @<you>"
```

This branch is protected against deletion and force-push.
