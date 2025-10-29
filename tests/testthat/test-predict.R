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
