test_that("prediction of exacerbation probability for sample patients works", {
  expect_equal(accept(samplePatients, version="accept2")$predicted_exac_probability, c(0.8327888, 0.4366622), tolerance = 1e-4)
})

test_that("prediction of severe exacerbation probability for sample patients works", {
  expect_equal(accept(samplePatients, version="accept2")$predicted_severe_exac_probability, c(0.6026383, 0.1085515), tolerance = 1e-4)
})

test_that("prediction of exacerbation rate for sample patients works", {
  expect_equal(accept(samplePatients, version="accept2")$predicted_exac_rate, c(1.7884977, 0.5738758), tolerance = 1e-4)
})

test_that("prediction of severe exacerbation rate for sample patients works", {
  expect_equal(accept(samplePatients, version="accept2")$predicted_severe_exac_rate, c(0.9229084, 0.1149076), tolerance = 1e-4)
})

test_that("accept3 default prediction works", {
  result <- accept(samplePatients, country = "CAN")
  expect_true("predicted_exac_probability" %in% names(result))
  expect_true("predicted_exac_rate" %in% names(result))
  expect_true("predicted_severe_exac_probability" %in% names(result))
  expect_true("predicted_severe_exac_rate" %in% names(result))
  expect_equal(nrow(result), 2)
  
  # Verify rate calculation: rate = -log(1-p)
  expected_rate <- -log(1 - result$predicted_exac_probability)
  expect_equal(result$predicted_exac_rate, expected_rate, tolerance = 1e-6)
  
  expected_sev_rate <- -log(1 - result$predicted_severe_exac_probability)
  expect_equal(result$predicted_severe_exac_rate, expected_sev_rate, tolerance = 1e-6)
})

test_that("accept3 requires country parameter", {
  expect_error(accept(samplePatients), "country.*required")
})

test_that("accept1 and accept2 warn when country parameter is provided", {
  expect_warning(accept(samplePatients[1,], version="accept1", country="CAN"), 
                 "not used by accept1.*ignored")
  expect_warning(accept(samplePatients[1,], version="accept2", country="USA"), 
                 "not used by accept2.*ignored")
})

test_that("accept3 TEST 1: CHN with obs_modsev_risk=0.6", {
  test_data <- tibble::tibble(
    ID = "TEST1",
    age = 42,
    male = FALSE,
    BMI = 21,
    smoker = FALSE,
    mMRC = 4,
    statin = FALSE,  # CVD
    ICS = FALSE,
    LABA = FALSE,
    LAMA = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    FEV1 = 30,
    oxygen = FALSE
  )
  
  result <- accept(test_data, version = "accept3", country = "CHN", obs_modsev_risk = 0.6)
  
  expect_equal(result$predicted_exac_probability, 0.7649, tolerance = 1e-4)
  expect_equal(result$predicted_severe_exac_probability, 0.1118, tolerance = 1e-4)
})

test_that("accept3 TEST 2: GBR (supported country, obs_modsev_risk ignored)", {
  test_data <- tibble::tibble(
    ID = "TEST2",
    age = 42,
    male = FALSE,
    BMI = 21,
    smoker = FALSE,
    mMRC = 4,
    statin = FALSE,  # CVD
    ICS = FALSE,
    LABA = FALSE,
    LAMA = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    FEV1 = 30,
    oxygen = FALSE
  )
  
  result <- accept(test_data, version = "accept3", country = "GBR", obs_modsev_risk = 0.6)
  
  expect_equal(result$predicted_exac_probability, 0.5214, tolerance = 1e-4)
  expect_equal(result$predicted_severe_exac_probability, 0.0888, tolerance = 1e-4)
})

test_that("accept3 TEST 3: NOR with different age and exacerbation history", {
  test_data <- tibble::tibble(
    ID = "TEST3",
    age = 78,
    male = FALSE,
    BMI = 21,
    smoker = FALSE,
    mMRC = 4,
    statin = FALSE,  # CVD
    ICS = FALSE,
    LABA = FALSE,
    LAMA = FALSE,
    LastYrExacCount = 1,
    LastYrSevExacCount = 1,
    FEV1 = 30,
    oxygen = FALSE
  )

  result <- accept(test_data, version = "accept3", country = "NOR", obs_modsev_risk = 0.6)

  expect_equal(result$predicted_exac_probability, 0.3555, tolerance = 1e-4)
  expect_equal(result$predicted_severe_exac_probability, 0.0794, tolerance = 1e-4)
})

# Tests for SGRQ/CAT/mMRC priority and usage
test_that("SGRQ is used when provided directly (accept3)", {
  patient_sgrq <- tibble::tibble(
    ID = "SGRQ_TEST",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    SGRQ = 45,
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  # Should not produce warning or message about SGRQ not found
  expect_message(accept(patient_sgrq, country = "CAN"), "ACCEPT v3 is recalibrated")
  result <- suppressMessages(accept(patient_sgrq, country = "CAN"))
  expect_true(!is.na(result$predicted_exac_probability))
})

test_that("CAT is used when SGRQ is not provided (accept2)", {
  patient_cat <- tibble::tibble(
    ID = "CAT_TEST",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    CAT = 20,  # CAT score instead of SGRQ
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  # Should produce warning about using CAT instead of SGRQ
  expect_warning(accept(patient_cat, version = "accept2"), "SGRQ score not found.*CAT")
  result <- suppressWarnings(accept(patient_cat, version = "accept2"))
  expect_true(!is.na(result$predicted_exac_probability))
})

test_that("mMRC is used when SGRQ and CAT are not provided (accept2)", {
  patient_mmrc <- tibble::tibble(
    ID = "mMRC_TEST",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    mMRC = 2,  # mMRC score instead of SGRQ or CAT
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  # Should produce message about using mMRC instead of SGRQ
  expect_message(accept(patient_mmrc, version = "accept2"), "SGRQ score not found.*mMRC")
  result <- suppressMessages(accept(patient_mmrc, version = "accept2"))
  expect_true(!is.na(result$predicted_exac_probability))
})

test_that("SGRQ takes priority over CAT when both provided (accept2)", {
  patient_sgrq <- tibble::tibble(
    ID = "PRIORITY_TEST1",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    SGRQ = 45,
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  patient_both <- tibble::tibble(
    ID = "PRIORITY_TEST2",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    SGRQ = 45,
    CAT = 99,  # Different CAT score - should be ignored
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  result_sgrq <- accept(patient_sgrq, version = "accept2")
  result_both <- accept(patient_both, version = "accept2")

  # Results should be identical because SGRQ is used in both cases
  expect_equal(result_sgrq$predicted_exac_probability, result_both$predicted_exac_probability)
})

test_that("SGRQ takes priority over mMRC when both provided (accept3)", {
  patient_sgrq <- tibble::tibble(
    ID = "PRIORITY_TEST3",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    SGRQ = 45,
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  patient_both <- tibble::tibble(
    ID = "PRIORITY_TEST4",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    SGRQ = 45,
    mMRC = 4,  # Different mMRC score - should be ignored
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  result_sgrq <- suppressMessages(accept(patient_sgrq, country = "CAN"))
  result_both <- suppressMessages(accept(patient_both, country = "CAN"))

  # Results should be identical because SGRQ is used in both cases
  expect_equal(result_sgrq$predicted_exac_probability, result_both$predicted_exac_probability)
})

test_that("CAT takes priority over mMRC when both provided (accept2)", {
  # CAT = 20 converts to SGRQ = 18.87 + 1.53 * 20 = 49.47
  patient_cat <- tibble::tibble(
    ID = "PRIORITY_TEST5",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    CAT = 20,
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  # mMRC = 2 converts to SGRQ = 20.43 + 14.77 * 2 = 49.97
  patient_mmrc <- tibble::tibble(
    ID = "PRIORITY_TEST6",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    mMRC = 2,
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  patient_both <- tibble::tibble(
    ID = "PRIORITY_TEST7",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    CAT = 20,
    mMRC = 2,
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  result_cat <- suppressWarnings(accept(patient_cat, version = "accept2"))
  result_mmrc <- suppressMessages(accept(patient_mmrc, version = "accept2"))
  result_both <- suppressWarnings(accept(patient_both, version = "accept2"))

  # Result with both should match CAT result (CAT has priority)
  expect_equal(result_cat$predicted_exac_probability, result_both$predicted_exac_probability)

  # Result with both should NOT match mMRC result (different SGRQ values)
  expect_false(isTRUE(all.equal(result_mmrc$predicted_exac_probability, result_both$predicted_exac_probability)))
})

test_that("Error when no SGRQ, CAT, or mMRC provided (accept3)", {
  patient_no_quality_of_life <- tibble::tibble(
    ID = "ERROR_TEST",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  expect_error(accept(patient_no_quality_of_life, country = "CAN"),
               "Either mMRC or SGRQ must be provided")
})

test_that("Different SGRQ values produce different results (no conversion)", {
  patient_sgrq_45 <- tibble::tibble(
    ID = "DIFF_TEST1",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    SGRQ = 45,
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  patient_sgrq_50 <- tibble::tibble(
    ID = "DIFF_TEST2",
    age = 65,
    male = TRUE,
    BMI = 25,
    smoker = TRUE,
    FEV1 = 45,
    SGRQ = 50,
    oxygen = FALSE,
    LastYrExacCount = 2,
    LastYrSevExacCount = 1,
    statin = TRUE,
    LAMA = TRUE,
    LABA = TRUE,
    ICS = TRUE
  )

  result_45 <- suppressMessages(accept(patient_sgrq_45, country = "CAN"))
  result_50 <- suppressMessages(accept(patient_sgrq_50, country = "CAN"))

  # Different SGRQ values should produce different results
  expect_false(isTRUE(all.equal(result_45$predicted_exac_probability, result_50$predicted_exac_probability)))
})
