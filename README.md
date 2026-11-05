# data-index

Index of gsm.datasim test data bundles. This branch contains data only (no
code or workflows) and is updated by automation; don't edit it by hand.

- `builds/<inputs_hash>.json`: one file per set of building packages. It
  records which data (`fingerprint`) those exact package versions produce,
  and the release (`tag`) that holds it. These files never change once
  written.
- `index.json`: a summary of every file in `builds/`, regenerated on each
  update, for falling back when no exact `inputs_hash` match exists.

Bundles are attached to releases tagged `data-<first 12 hex of fingerprint>`.
`inputs_hash` and `fingerprint` are defined in `?gsm.datasim::build_testdata_bundle`.
See Gilead-Public/gsm.datasim#153.
