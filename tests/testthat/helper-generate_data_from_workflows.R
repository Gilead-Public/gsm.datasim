# Fixture shared by the column_overrides integration tests in
# test-generate_data_from_workflows.R: a minimal fake workflow exposing a
# single Raw_CUSTOM domain with a numeric and a character column.
make_override_workflows <- function() {
  list(
    wf1 = list(
      meta = list(),
      spec = list(
        Raw_CUSTOM = list(
          base_val = list(type = "numeric"),
          label = list(type = "character")
        )
      ),
      steps = list()
    )
  )
}
