# Vendored front-end sources

| File | Source | Commit | Modifications |
|---|---|---|---|
| `datatable.js` | `Gilead-BioStats/open.gismo` `site/src/datatable.js` (+ `esc()` from `site/src/utils.js`) | `1072c620955a` (2026-06-24) | ES module imports/exports replaced by an IIFE that publishes `window.gsmTestdataExplorer.datatable`; `buildEnhancedTable(csvText)` refactored into `buildTable({headers, rows})` + `buildTableFromRows(headers, rows)` so embedded JSON rows can be rendered without a CSV round-trip; cells are stringified and `null` renders as empty. Behaviour (sort, search, pagination, type detection, stats tooltip) is unchanged. |
| `explorer.js` | Adapted from `Gilead-BioStats/open.gismo` `site/src/explorer.js` | `1072c620955a` | Tree levels are snapshot → layer → table instead of phase → workflow → artifact; artifacts are embedded (no `loadArtifact()` fetch); adds the bundle header, schema tab and overview charts (via `gsm.viz`). |
| `explorer.css` | Adapted from `Gilead-BioStats/open.gismo` `site/src/style.css` (`.explorer-*`, `.dt-*` rules) | `1072c620955a` | Scoped under `.gte-root`, light theme variables, layout sized to the widget container instead of the viewport. |
| `../gsm.viz-2.4.0/index.js` | `Gilead-Public/gsm.viz` `index.js` (built bundle, `gsmViz` global) | `2d13c9fc9920` (v2.4.0) | None. |

These copies exist because `open.gismo` does not publish its front-end modules
as a package yet; see Gilead-Public/gsm.datasim#155 for the proposal to replace
them with a shared dependency.
