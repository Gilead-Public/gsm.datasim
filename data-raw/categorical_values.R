## data-raw/categorical_values.R
##
## Generates `categorical_lookup` — an internal package list that stores
## realistic categorical values for domain generator functions.
##
## Source: clindata package (rawplus_ae, rawplus_studcomp).
## Run this script once (or whenever clindata is updated) via:
##   source("data-raw/categorical_values.R")
##
## Requires: clindata (dev dependency only; listed in Suggests)

library(clindata)

# ---------------------------------------------------------------------------
# AE: MedDRA preferred terms (mdrpt_nsv)
# Use the 50 most-frequently occurring terms in clindata::rawplus_ae to keep
# the lookup small while still producing realistic plots.
# ---------------------------------------------------------------------------
ae_term_freq <- sort(table(rawplus_ae[["mdrpt_nsv"]]), decreasing = TRUE)
mdrpt_nsv_lookup <- sort(names(head(ae_term_freq, 50)))

# ---------------------------------------------------------------------------
# AE: MedDRA system organ class (mdrsoc_nsv)
# All 26 SOCs present in clindata::rawplus_ae.
# ---------------------------------------------------------------------------
mdrsoc_nsv_lookup <- sort(unique(rawplus_ae[["mdrsoc_nsv"]]))

# ---------------------------------------------------------------------------
# STUDCOMP: study-completion reason (compreas)
# All valid values in clindata::rawplus_studcomp (blank and out-of-bound
# entries excluded).
# ---------------------------------------------------------------------------
compreas_lookup <- sort(
  rawplus_studcomp[["compreas"]][
    !grepl("^$|out of bound", rawplus_studcomp[["compreas"]])
  ] |> unique()
)

# ---------------------------------------------------------------------------
# Death: death classification/reason (deathcls)
# ---------------------------------------------------------------------------
deathcls_lookup <- c(
  "Progressive Disease",
  "Adverse Event",
  "Disease Recurrence",
  "Not related to disease",
  "Related to long-term follow-up and not related to study drug"
)

# ---------------------------------------------------------------------------
# Bundle into a single internal list and save
# ---------------------------------------------------------------------------
categorical_lookup <- list(
  mdrpt_nsv = mdrpt_nsv_lookup,
  mdrsoc_nsv = mdrsoc_nsv_lookup,
  compreas = compreas_lookup,
  deathcls = deathcls_lookup
)

save(
  categorical_lookup,
  file = "R/sysdata.rda",
  compress = "xz"
)

message("categorical_lookup saved to R/sysdata.rda")
message("  mdrpt_nsv : ", length(categorical_lookup$mdrpt_nsv), " values")
message("  mdrsoc_nsv: ", length(categorical_lookup$mdrsoc_nsv), " values")
message("  compreas  : ", length(categorical_lookup$compreas), " values")
message("  deathcls  : ", length(categorical_lookup$deathcls), " values")
