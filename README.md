# gsm.datasim actions

This branch contains only the shared action(s) for {gsm.datasim}. Do not
develop other features on this branch.

## Test data caller

[`testdata.yaml`](testdata.yaml) is a workflow template. Copy it to
`.github/workflows/testdata.yaml` in any gsm repo. On a push to `dev` or
`main`, it runs [`trigger-build`](trigger-build/action.yml), which:

1. Skips unless the repo is a test data input: by default, it must be listed
   in `packages.json` on gsm.datasim's `data-index` branch. Set
   `enabled` to `'true'` or `'false'` to override.
2. Generates a token (`gsm.utils/generate-token@actions-v1`) and runs
   `gh workflow run testdata-build.yaml -R Gilead-Public/gsm.datasim --ref
   <branch> -f triggered_by=<repo>@<sha>`.
3. Finds the dispatched run by its `run-name` and links it in the job summary.
4. With `wait: true`, waits for the build and fails if it fails.

Builds, logs, and releases all live in gsm.datasim. See the
README on `data-index` for what a build publishes.

Callers need the `CM_BOT_APP_ID` and `CM_BOT_PRIVATE_KEY` secrets for an app
with `actions: write` on gsm.datasim. Without them, only gsm.datasim itself
can dispatch (using `GITHUB_TOKEN`).

## Actions

| Action | Purpose |
|---|---|
| [`trigger-build`](trigger-build/action.yml) | Used by `testdata.yaml`. Dispatches `testdata-build.yaml`, links the run, and optionally waits. |
| [`build-bundle`](build-bundle/action.yml) | Installs gsm.datasim at a commit (plus optional `extra-packages`, e.g. `local::.`) and runs `build_testdata_bundle()`. Publishes nothing; outputs `bundle-dir`, `fingerprint`, `inputs-hash`. Needs R already set up. |
| [`publish-bundle`](publish-bundle/action.yml) | Creates or reuses the `data-<fp12>` release for a built bundle and records it on `data-index`. Needs `contents: write`. |
| [`needs-testdata`](needs-testdata/action.yml) | Outputs `needed` = `true` if a package is listed in `packages.json` on `data-index` (published from `.bundle_packages` by gsm.datasim's `testdata-packages` workflow on `dev`). |
| [`find-run`](find-run/action.yml) | Polls for a workflow run by exact `run-name`, e.g. one just dispatched. |
| [`wait-run`](wait-run/action.yml) | Waits for a run and fails if it didn't succeed. |
| [`wait-for-index`](wait-for-index/action.yml) | After a merge: polls `data-index` `index.json` until a build records `package` at `sha`. Outputs `tag` and `fingerprint`; with `download: true`, downloads the release zip and exports `GSM_TESTDATA_SNAPSHOT`. On `timeout`, fails with a link to the build runs. |
| [`build-pr-bundle.yaml`](.github/workflows/build-pr-bundle.yaml) | Reusable workflow for PRs: builds the bundle with the caller's package (`local::.`) and uploads the artifact `testdata-pr<number>-<sha>`, deleting the PR's older bundles once it succeeds. Call it from a separate build-only workflow that sets `run-name: build-pr-bundle <sha>` (see the workflow header). |
| [`wait-for-bundle`](wait-for-bundle/action.yml) | Finds the `build-pr-bundle` run for a SHA, waits for it, downloads its artifact, and exports `GSM_TESTDATA_SNAPSHOT`. Needs `actions: read`. |
| [`delete-pr-bundles`](delete-pr-bundles/action.yml) | Deletes a PR's `testdata-pr<number>-*` artifacts, optionally keeping one. `build-pr-bundle` uses it to drop superseded bundles; the [`testdata-pr-cleanup.yaml`](testdata-pr-cleanup.yaml) template (copy it into every gsm repo) uses it when a PR closes. Needs `actions: write`. |

Actions on this branch reference each other with `uses: $/<action>`.

## Code of Conduct

Please note that the `{gsm.datasim}` project is released with a [Contributor
Code of Conduct](https://gilead-public.github.io/gsm.datasim/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
