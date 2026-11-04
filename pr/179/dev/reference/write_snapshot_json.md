# Write snapshot.json for files already in a folder

Checksums are computed from the files on disk.

## Usage

``` r
write_snapshot_json(dir, objects, key, packages = NULL, metadata = NULL)
```

## Arguments

- dir:

  Snapshot folder containing the files.

- objects:

  Named list or character vector: object name -\> file name.

- key:

  Version key (a string).

- packages:

  Optional named list; each element has `version` and `remote_sha`.

- metadata:

  Optional named list stored as-is.

## Value

The snapshot description, invisibly.
