test_that("prediction of exacerbation probability for sample patients works", {
  expect_equal(accept(samplePatients)$predicted_exac_probability, c(0.8327888, 0.4366622), tolerance = 1e-4)
})

test_that("prediction of severe exacerbation probability for sample patients works", {
  expect_equal(accept(samplePatients)$predicted_severe_exac_probability, c(0.6026383, 0.1085515), tolerance = 1e-4)
})

test_that("prediction of exacerbation rate for sample patients works", {
  expect_equal(accept(samplePatients)$predicted_exac_rate, c(1.7884977, 0.5738758), tolerance = 1e-4)
})

test_that("prediction of severe exacerbation rate for sample patients works", {
  expect_equal(accept(samplePatients)$predicted_severe_exac_rate, c(0.9229084, 0.1149076), tolerance = 1e-4)
})
