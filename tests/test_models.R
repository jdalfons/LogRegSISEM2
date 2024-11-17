library(testthat)
source("../R/models.R")

test_that("MultinomialLogisticRegression initializes correctly", {
  data <- data.frame(
    Category = factor(sample(c("A", "B", "C"), size = 100, replace = TRUE)),
    Predictor = rnorm(100),
    Response = factor(sample(c("Class1", "Class2", "Class3"), size = 100, replace = TRUE))
  )
  model <- MultinomialLogisticRegression$new(Response ~ Predictor + Category, data)
  expect_equal(model$formula, Response ~ Predictor + Category)
  expect_equal(nrow(model$data), 100)
  expect_equal(model$num_classes, 3)
})

test_that("MultinomialLogisticRegression fits the model", {
  data <- data.frame(
    Category = factor(sample(c("A", "B", "C"), size = 100, replace = TRUE)),
    Predictor = rnorm(100),
    Response = factor(sample(c("Class1", "Class2", "Class3"), size = 100, replace = TRUE))
  )
  model <- MultinomialLogisticRegression$new(Response ~ Predictor + Category, data)
  model$fit()
  expect_false(is.null(model$fitted_model))
  expect_equal(length(model$coefficients), ncol(model$X) * (model$num_classes - 1))
})

test_that("MultinomialLogisticRegression predicts probabilities", {
  data <- data.frame(
    Category = factor(sample(c("A", "B", "C"), size = 100, replace = TRUE)),
    Predictor = rnorm(100),
    Response = factor(sample(c("Class1", "Class2", "Class3"), size = 100, replace = TRUE))
  )
  model <- MultinomialLogisticRegression$new(Response ~ Predictor + Category, data)
  model$fit()
  new_data <- data.frame(
    Predictor = c(0.5, -1.2),
    Category = factor(c("A", "B"))
  )
  predictions <- model$predict(new_data)
  expect_equal(nrow(predictions), 2)
  expect_equal(ncol(predictions), 3)
})

test_that("MultinomialLogisticRegression summarizes the model", {
  data <- data.frame(
    Category = factor(sample(c("A", "B", "C"), size = 100, replace = TRUE)),
    Predictor = rnorm(100),
    Response = factor(sample(c("Class1", "Class2", "Class3"), size = 100, replace = TRUE))
  )
  model <- MultinomialLogisticRegression$new(Response ~ Predictor + Category, data)
  model$fit()
  summary <- model$summary()
  expect_false(is.null(summary$coefficients))
  expect_true(is.numeric(summary$log_likelihood))
  expect_true(is.numeric(summary$num_iterations))
})