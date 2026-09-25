# Fixture: `n_sites` sites with `per_site` subjects each, and `n_ae` AE rows
# spread over those subjects.
make_grading_fixture <- function(n_sites = 60, per_site = 8, n_ae = 12000) {
  subj <- data.frame(
    subjid = sprintf("S%05d", seq_len(n_sites * per_site)),
    invid = rep(sprintf("0X%03d", seq_len(n_sites)), each = per_site),
    stringsAsFactors = FALSE
  )
  ae <- data.frame(
    subjid = sample(subj$subjid, n_ae, replace = TRUE),
    aeser = NA_character_,
    mdrpt_nsv = NA_character_,
    mdrsoc_nsv = NA_character_,
    aetoxgr = NA_integer_,
    stringsAsFactors = FALSE
  )
  list(subj = subj, ae = ae)
}

test_that("ae_term_catalog has valid, term-specific grade distributions", {
  catalog <- ae_term_catalog()
  probs <- as.matrix(catalog[paste0("g", 1:5)])

  expect_gte(nrow(catalog), 20)
  expect_false(anyDuplicated(catalog$mdrpt_nsv) > 0)
  expect_gte(length(unique(catalog$mdrsoc_nsv)), 8)
  expect_true(all(catalog$weight > 0))
  expect_true(all(probs >= 0))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(catalog)), tolerance = 1e-9)

  # Mild terms rarely reach Grade 3; serious terms start there.
  grade3plus <- setNames(rowSums(probs[, 3:5]), catalog$mdrpt_nsv)
  expect_lt(grade3plus[["Headache"]], 0.1)
  expect_equal(grade3plus[["Sepsis"]], 1)
})

test_that("draw_ae_grades gives valid grades and ~20-25% Grade 3+ study-wide", {
  set.seed(149)
  catalog <- ae_term_catalog()
  terms <- catalog$mdrpt_nsv[sample_ae_term_idx(20000)]
  grades <- draw_ae_grades(terms)

  expect_type(grades, "integer")
  expect_true(all(grades %in% 1:5))
  expect_gte(mean(grades >= 3), 0.18)
  expect_lte(mean(grades >= 3), 0.25)
  expect_true(all(grades[terms == "Sepsis"] >= 3))
  expect_length(draw_ae_grades(character(0)), 0)
  # Unknown terms fall back to the study-wide mix rather than failing.
  expect_true(all(draw_ae_grades(rep("Not a term", 50)) %in% 1:5))
})

test_that("draw_ae_grades site_shift moves grades in the intended direction", {
  set.seed(149)
  terms <- rep(c("Nausea", "Anaemia", "Fatigue"), length.out = 6000)

  base <- draw_ae_grades(terms)
  over <- draw_ae_grades(terms, site_shift = 2)
  under <- draw_ae_grades(terms, site_shift = -2)

  expect_gt(mean(over >= 3), mean(base >= 3) + 0.15)
  expect_lt(mean(under >= 3), mean(base >= 3))
  expect_gt(mean(under == 1), mean(base == 1) + 0.15)
  expect_lt(mean(over == 1), mean(base == 1))
})

test_that("ae_site_grading_profile is deterministic and seeds both directions", {
  sites <- sprintf("0X%03d", 1:1000)

  seed_before <- .Random.seed
  p1 <- ae_site_grading_profile(sites, "STUDY-1")
  # Site tendencies are hash-based and leave the RNG stream alone.
  expect_identical(.Random.seed, seed_before)

  expect_identical(p1, ae_site_grading_profile(rev(sites), "STUDY-1"))
  expect_setequal(p1$grading, c("over", "under", "typical"))
  expect_gt(mean(p1$grading == "over"), 0.02)
  expect_lt(mean(p1$grading == "over"), 0.09)
  expect_gt(mean(p1$grading == "under"), 0.02)
  expect_lt(mean(p1$grading == "under"), 0.09)
  expect_true(all(p1$shift[p1$grading == "over"] > 1))
  expect_true(all(p1$shift[p1$grading == "under"] < -1))
  expect_true(all(abs(p1$shift[p1$grading == "typical"]) < 1.5))

  # A site keeps its tendency as more sites enroll (i.e. across snapshots).
  p_sub <- ae_site_grading_profile(sites[1:500], "STUDY-1")
  expect_equal(p_sub$grading, p1$grading[match(p_sub$invid, p1$invid)])

  # Different studies seed different sites.
  p2 <- ae_site_grading_profile(sites, "STUDY-2")
  expect_false(identical(p1$grading, p2$grading))
})

test_that("ae_site_grading_profile guarantees one site per direction in small studies", {
  for (n_sites in 2:6) {
    p <- ae_site_grading_profile(sprintf("SITE%02d", seq_len(n_sites)), "PROT-001")
    expect_gte(sum(p$grading == "over"), 1)
    expect_gte(sum(p$grading == "under"), 1)
  }
  expect_equal(ae_site_grading_profile("SITE01")$grading, "typical")
})

test_that("ae_site_grading_profile anchors both directions among the largest sites", {
  sites <- sprintf("0X%03d", 1:200)
  size <- setNames(rep(c(40, 5), c(15, 185)), sites)

  for (study in paste0("STUDY-", 1:5)) {
    p <- ae_site_grading_profile(sites, study, site_size = size)
    big <- p$grading[p$invid %in% sites[1:15]]
    expect_true(any(big == "over"))
    expect_true(any(big == "under"))
  }
})

test_that("ae_site_grading_profile turns site effects off at intensity 0", {
  p <- ae_site_grading_profile(sprintf("0X%03d", 1:100), "S", intensity = 0)
  expect_true(all(p$shift == 0))
  expect_true(all(p$grading == "typical"))
})

test_that("simulate_ae_grading keys grades on each record's actual site", {
  set.seed(149)
  fx <- make_grading_fixture()
  res <- simulate_ae_grading(fx$ae, seq_len(nrow(fx$ae)), fx$subj, studyid = "PROT-149")

  catalog <- ae_term_catalog()
  expect_true(all(res$aetoxgr %in% 1:5))
  expect_true(all(res$mdrpt_nsv %in% catalog$mdrpt_nsv))
  expect_equal(res$mdrsoc_nsv, catalog$mdrsoc_nsv[match(res$mdrpt_nsv, catalog$mdrpt_nsv)])
  expect_setequal(unique(res$aeser), c("Y", "N"))

  res$invid <- fx$subj$invid[match(res$subjid, fx$subj$subjid)]
  profile <- ae_site_grading_profile(fx$subj$invid, "PROT-149", site_size = table(fx$subj$invid))
  res$grading <- profile$grading[match(res$invid, profile$invid)]
  expect_true(any(res$grading == "over"))
  expect_true(any(res$grading == "under"))

  typical <- res[res$grading == "typical", ]
  over <- res[res$grading == "over", ]
  under <- res[res$grading == "under", ]

  expect_gt(mean(over$aetoxgr >= 3), mean(typical$aetoxgr >= 3) + 0.15)
  expect_lt(mean(under$aetoxgr >= 3), mean(typical$aetoxgr >= 3) - 0.08)
  expect_gt(mean(under$aetoxgr == 1), mean(typical$aetoxgr == 1) + 0.15)
  expect_lt(mean(over$aetoxgr == 1), mean(typical$aetoxgr == 1) - 0.15)

  # Grade mix now differs by site (it was independent of site before #149).
  tab <- table(res$invid, res$aetoxgr >= 3)
  expect_lt(suppressWarnings(stats::chisq.test(tab))$p.value, 1e-6)
})

test_that("simulate_ae_grading leaves earlier rows alone and honors source_col", {
  set.seed(149)
  fx <- make_grading_fixture(n_sites = 5, per_site = 4, n_ae = 20)
  ae <- fx$ae
  ae$mdrpt_nsv[1:10] <- "Existing term"
  ae$aetoxgr[1:10] <- 9L
  names(ae)[names(ae) == "aetoxgr"] <- "AETOXGR"
  spec <- list(aetoxgr = list(source_col = "AETOXGR"))

  res <- simulate_ae_grading(ae, 11:20, fx$subj, studyid = "P", spec = spec)

  expect_equal(res[1:10, ], ae[1:10, ])
  expect_true(all(res$AETOXGR[11:20] %in% 1:5))
  expect_true(all(res$mdrpt_nsv[11:20] %in% ae_term_catalog()$mdrpt_nsv))
  expect_identical(simulate_ae_grading(ae, integer(0), fx$subj), ae)
})

test_that("registry Raw_AE generation applies site-keyed grading", {
  set.seed(149)
  fx <- make_grading_fixture(n_sites = 40, per_site = 5)
  data <- list(Raw_SUBJ = fx$subj, Raw_STUDY = data.frame(protocol_number = "PROT-149"))
  spec <- make_ae_spec()
  spec$Raw_AE$aetoxgr$type <- "integer"

  res <- generate_domain_from_registry("Raw_AE", list(
    data = data,
    previous_data = list(),
    combined_specs = spec,
    n = 4000,
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-12-31")
  ))

  expect_equal(nrow(res), 4000)
  expect_true(all(res$mdrpt_nsv %in% ae_term_catalog()$mdrpt_nsv))
  expect_true(all(res$aetoxgr %in% 1:5))

  res$invid <- fx$subj$invid[match(res$subjid, fx$subj$subjid)]
  profile <- ae_site_grading_profile(fx$subj$invid, "PROT-149", site_size = table(fx$subj$invid))
  res$grading <- profile$grading[match(res$invid, profile$invid)]
  g3 <- tapply(res$aetoxgr >= 3, res$grading, mean)
  expect_gt(g3[["over"]], g3[["typical"]] + 0.15)
  expect_lt(g3[["under"]], g3[["typical"]])
})
