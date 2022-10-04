library(testthat)
source('score_questionnaire.R')
context('Score questionnaire tests')

test_that('Function checks input to be of expected type', {
  expect_error(score_questionnaire(x = list()))
  expect_error(score_questionnaire(x = c()))
  expect_error(score_questionnaire(x = 'Nothing_burger'))
})

test_that('Function checks input to be non-zero', {
  expect_error(score_questionnaire(x = data.frame()))
})

test_that('Function checks that cols and rev_cols is of the correct type', {
  input_data <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))
  expect_error(score_questionnaire(input_data, cols = 123))
  expect_error(score_questionnaire(input_data, cols = c('x', 'y', 'z'), rev_cols = 123))
  expect_error(score_questionnaire(input_data, cols = NULL, rev_cols = c('x', 'y', 'z')))
})

test_that('Function checks that cols exist in names of x', {
  input_data <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))
  expect_error(score_questionnaire(input_data, cols = c('x')))
  expect_error(score_questionnaire(input_data, cols = c('a', 'b', 'x')))
  expect_error(score_questionnaire(input_data, cols = c('a', 'b', 'c', 'd')))
})

test_that('data to be scored has at least two dimensions', {
  input_data <- data.frame(y = rnorm(10))
  expect_error(score_questionnaire(x = input_data, cols = 'y'))
})

test_that('return object has correct type and length', {
  input_data <- data.frame(x = rnorm(10), y = rnorm(10))
  expect_type(score_questionnaire(x = input_data, cols = c('x', 'y')), 'double')
  expect_length(score_questionnaire(x = input_data, cols = c('x', 'y')), dim(input_data)[[1]])
})

test_that('Scored data is as expected', {
  set.seed(123)
  input_data <- data.frame(a = sample(1:5, 10, replace = TRUE),
                           b = sample(1:5, 10, replace = TRUE),
                           c = sample(1:5, 10, replace = TRUE),
                           d = sample(1:5, 10, replace = TRUE),
                           e = sample(1:5, 10, replace = TRUE),
                           i = sample(c(1:5, NA), 10, replace = TRUE),
                           j = sample(c(1:5, NA), 10, replace = TRUE),
                           k = sample(c(1:5, NA), 10, replace = TRUE))
  
  abc_sum <- rowSums(input_data[, c('a', 'b', 'c')])
  de_sum <- rowSums(input_data[, c('d', 'e')])
  ijk_sum <- rowMeans(input_data[, c('i', 'j', 'k')], na.rm = TRUE)*3
  
  expect_equal(score_questionnaire(input_data, cols = c('a', 'b', 'c')), expected = abc_sum)
  expect_equal(score_questionnaire(input_data, cols = c('d', 'e')), expected = de_sum)
  expect_equal(score_questionnaire(input_data, cols = c('i', 'j', 'k')), expected = ijk_sum)
  
  rev_de_sum <- rowSums( (input_data[, c('d', 'e')] - 5) * -1 + 1 )
  abc_rev_de_sum <- abc_sum + rev_de_sum
  
  expect_equal(score_questionnaire(input_data, cols = c('a', 'b', 'c', 'd', 'e'),
                                   rev_score_cols = c('d', 'e'),
                                   min = 1, max = 5), expected = abc_rev_de_sum)
  expect_equal(score_questionnaire(input_data, cols = c('a', 'b', 'c'),
                                   rev_score_cols = c('d', 'e'),
                                   min = 1, max = 5), expected = abc_rev_de_sum)
  
})

