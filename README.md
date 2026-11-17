# gsm.datasim actions

This branch contains only the shared action(s) for {gsm.datasim}. Do not
develop other features on this branch.

## Test data caller

[`testdata.yaml`](testdata.yaml) is a workflow template. Copy it to
`.github/workflows/testdata.yaml` in any gsm repo. On a push to `dev` or
`main`, it runs [`trigger-build`](trigger-build/action.yml), which:

1. Skips unless the repo is a test data input: by default, it must be listed
   in `packages` of `index.json` on gsm.datasim's `data-index` branch. Set
   `enabled` to `'true'` or `'false'` to override.
2. Generates a token (`gsm.utils/generate-token@actions-v1`) and runs
   `gh workflow run testdata-build.yaml -R Gilead-Public/gsm.datasim --ref
   <branch> -f triggered_by=<repo>@<sha>`.
3. Finds the dispatched run by its `run-name` and links it in the job summary.
4. With `wait: true`, waits for the build and fails if it fails.

Builds, logs, releases, and the build queue all live in gsm.datasim. See the
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
| [`needs-testdata`](needs-testdata/action.yml) | Outputs `needed` = `true` if a package is listed in `packages` of `data-index` `index.json`. |
| [`find-run`](find-run/action.yml) | Polls for a workflow run by exact `run-name`, e.g. one just dispatched. |
| [`wait-run`](wait-run/action.yml) | Waits for a run and fails if it didn't succeed. |

Actions on this branch reference each other with `uses: $/<action>`.

## Code of Conduct

Please note that the `{gsm.datasim}` project is released with a [Contributor
Code of Conduct](https://gilead-public.github.io/gsm.datasim/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
