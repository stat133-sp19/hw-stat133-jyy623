
context("Test for check functions")

test_that("check_prob works as expected", {
  expect_equal(check_prob(0.5), TRUE)
  expect_error(check_prob(1.1))
  expect_error(check_prob("na"))
})

test_that("check_trials works as expected", {
  expect_equal(check_trials(10), TRUE)
  expect_error(check_trials(0.5))
  expect_error(check_trials("la"))
})

test_that("check_success works as expected",{
  expect_equal(check_success(10, 5), TRUE)
  expect_error(check_success(10.5,5))
  expect_error(check_success(10, 20))
  expect_error(check_success("a", 5))
})

context("Test for aux functions")

test_that("aux_mean works as expected", {
  expect_equal(aux_mean(10, 0.5), 5)
  expect_length(aux_mean(10, 0.5), 1)
  expect_type(aux_mean(10, 0.5), "double")
})

test_that("aux_variance works as expected", {
  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_length(aux_variance(10, 0.5), 1)
  expect_type(aux_variance(10, 0.5), "double")
})

test_that("aux_mode works as expected", {
  expect_equal(aux_mode(10, 0.3), 3)
  expect_length(aux_mode(10, 0.5), 1)
  expect_type(aux_mode(10, 0.5), "double")
})

test_that("aux_skewness works as expected", {
  expect_equal(aux_skewness(10, 0.3), (1 - 2*0.3)/sqrt(aux_variance(10, 0.3)))
  expect_length(aux_skewness(10, 0.5), 1)
  expect_type(aux_skewness(10, 0.5), "double")
})

test_that("aux_kurtosis works as expected", {
  expect_equal(aux_kurtosis(10, 0.3), (1 - 6 * 0.3 * (1 - 0.3))/aux_variance(10, 0.3))
  expect_length(aux_kurtosis(10, 0.5), 1)
  expect_type(aux_kurtosis(10, 0.5), "double")
})

context("Test for bin functions")

test_that("bin_choose works as expected", {
  expect_equal(bin_choose(5, 0), 1)
  expect_error(bin_choose(5, 10))
  expect_equal(bin_choose(5, 0:1), c(1,5))
})

test_that("bin_probability works as expected", {
  expect_equal(bin_probability(2, 5, 0.5), 0.3125)
  expect_equal(bin_probability(0:2, 5, 0.5), c(0.03125, 0.15625, 0.3125))
  expect_error(bin_probability(2.5, 10, 0.5))
})

test_that("bin_distribution works as expected", {
  expect_is(bin_distribution(5, 0.5), c("bindis", "data.frame"))
  expect_length(bin_distribution(5, 0.5), 2)
  expect_named(bin_distribution(5, 0.5),c("success", "probability"))
})

test_that("bin_cumulative works as expected", {
  expect_is(bin_cumulative(5, 0.5), c("bindis", "data.frame"))
  expect_length(bin_cumulative(5, 0.5), 3)
  expect_named(bin_cumulative(5, 0.5),c("success", "probability", "cumulative"))
})









