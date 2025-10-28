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
