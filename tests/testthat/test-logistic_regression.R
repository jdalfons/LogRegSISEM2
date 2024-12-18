library(testthat)
library(LogRegSISEM2)

test_that("LogisticRegression class works correctly", {
  # Generate synthetic dataset
  data(iris)
  iris_binary <- iris[iris$Species != "versicolor", ]
  iris_binary$Species <- ifelse(iris_binary$Species == "setosa", 1, 0)
  X <- iris_binary[, c("Sepal.Length", "Sepal.Width")]
  y <- iris_binary$Species

  # Split data
  set.seed(123)
  train_indices <- sample(seq_len(nrow(X)), size = 0.8 * nrow(X))
  X_train <- X[train_indices, ]
  y_train <- y[train_indices]
  X_test <- X[-train_indices, ]
  y_test <- y[-train_indices]

  # Initialize and train the model
  reglog_model <- LogisticRegression$new(learning_rate = 0.01, num_iterations = 1000)
  reglog_model$fit(X_train, y_train)

  # Predictions
  reglog_pred_proba <- reglog_model$predict_proba(X_test)
  reglog_pred_class <- reglog_model$predict(X_test)

  # Check that predictions are of the correct length
  # expect_equal(length(reglog_pred_proba), nrow(X_test))
  expect_equal(length(reglog_pred_class), nrow(X_test))

  # Check that predictions are within the expected range
  expect_true(all(reglog_pred_proba >= 0 & reglog_pred_proba <= 1))
  # expect_true(all(reglog_pred_class %in% c("setosa", "virginica")))

  # Check accuracy
  reglog_accuracy <- mean(reglog_pred_class == y_test)
  expect_true(reglog_accuracy > 0.5) # Expect accuracy to be better than random guessing

  # Check coefficients
  coefficients <- reglog_model$weights
  expect_equal(dim(coefficients), c(ncol(X_train), 2)) # 2 classes

  # Check log-likelihood
  log_likelihood <- reglog_model$compute_cost(y_train, reglog_model$predict_proba(X_train))
  expect_true(is.numeric(log_likelihood))

  # Check summary output
  expect_output(reglog_model$summary(), "MLR Model Summary")
  expect_output(reglog_model$summary(), "Hyperparameters:")
  expect_output(reglog_model$summary(), "Model Details:")
  expect_output(reglog_model$summary(), "Weights and Bias:")
})