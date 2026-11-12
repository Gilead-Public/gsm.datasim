# Namespace-qualify workflow step names

Workflow YAML may reference step functions without a namespace (e.g.
`Analyze_NormalApprox`), which workr resolves from the search path.
Rather than attaching packages, each bare name is resolved through the
namespaces of `packages` and rewritten as `owner::fn`.

## Usage

``` r
.qualify_workflow_steps(workflows, packages)
```

## Arguments

- workflows:

  List of workflows.

- packages:

  Packages whose namespaces are searched for step functions.

## Value

`workflows` with qualified step names.
