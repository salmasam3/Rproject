source("cleaner_code_2.R")
library(testthat)

good <- read.csv("good.csv", na.strings = "-999", stringsAsFactors = FALSE)
df <- read_mass_data("MOMv3.3.txt", header =FALSE, column_names = c("continent", "status", "order", "family","genus", "species", "log10mass", "mass", "ref"), na.strings = "-999")
df <- data_error_correct(df)
final_df <- calculate_mean_masses(df)


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


test_that("Regression",{
    expect_equal(sum(good$continent == final_df$continent),7)
    expect_equal(sum(abs(good$avg_extant_mass- final_df$avg_extant_mass) < 1e-6), 7)
})
