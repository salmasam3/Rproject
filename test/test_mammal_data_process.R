source("../mammal_data_process.R")
library(testthat)

test_that("Table produced", expect_equal(dim(all_of_it())[1], 7))
test_that("Length 1 vector", {
  expect_length(mean_mass(4), 1)
  expect_length(mean_mass(1:100), 1)
  })

test_that("Numeric from NA", {
  expect_true(!is.na(mean_mass(c(1:100, NA))))
})

test_that("Correct mean", {
  expect_equal(mean_mass(c(3,4,2,9,4.5)), mean(c(3, 4, 2, 9, 4.5)))
})
