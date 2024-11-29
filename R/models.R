library(R6)

#' Logistic Regression Model with Gradient Descent Optimization
#'
#' This R6 class implements a logistic regression model with gradient descent optimization.
#'
#' @docType class
#' @name LogisticRegression
#' @import R6
#' @export
#' @field learning_rate The learning rate for gradient descent. Default is 0.01.
#' @field iterations The number of iterations for gradient descent. Default is 1000.
#' @field X The feature matrix with an added intercept term.
#' @field y The response vector.
#' @field beta The coefficients of the logistic regression model.
#' @field n_features The number of features in the model.
#' @field ll The log-likelihood of the model.
#' @method LogisticRegression initialize
#' @method LogisticRegression fit
#' @method LogisticRegression predict_proba
#' @method LogisticRegression predict
#' @method LogisticRegression log_likelihood
#' @method LogisticRegression get_coefficients
#' @method LogisticRegression get_log_likelihood
#' @method LogisticRegression sigmoid
#'
#' @examples
#' # Generate synthetic dataset similar to your previous example
#' # But now we'll use iris dataset for a more standard comparison
#' data(iris)
#'
#' # Convert to binary classification (setosa vs. others)
#' iris_binary <- iris[iris$Species != "versicolor", ]
#' iris_binary$Species <- ifelse(iris_binary$Species == "setosa", 1, 0)
#'
#' # Prepare features and target
#' X <- as.matrix(iris_binary[, c("Sepal.Length", "Sepal.Width")])
#' y <- iris_binary$Species
#'
#' # Split data
#' train_indices <- createDataPartition(y, p = 0.8, list = FALSE)
#' X_train <- X[train_indices, ]
#' y_train <- y[train_indices]
#' X_test <- X[-train_indices, ]
#' y_test <- y[-train_indices]
#'
#' # Method 1: Custom R6 Logistic Regression
#' reglog_model <- LogisticRegression$new(learning_rate = 0.01, iterations = 1000)
#' reglog_start_time <- Sys.time()
#' reglog_model$fit(X_train, y_train)
#' reglog_pred_proba <- reglog_model$predict_proba(X_test)
#' reglog_pred_class <- reglog_model$predict(X_test)
#' reglog_end_time <- Sys.time()
#'
#' # Calculate R6 Model Performance
#' reglog_accuracy <- mean(reglog_pred_class == y_test)
#' reglog_log_likelihood <- reglog_model$get_log_likelihood()
#' reglog_coefficients <- reglog_model$get_coefficients()
#' reglog_computation_time <- as.numeric(reglog_end_time - reglog_start_time)
LogisticRegression <- R6Class("LogisticRegression",
  public = list(
    #' Initialize the class
    #' @param learning_rate The learning rate for gradient descent. Default is 0.01.
    #' @param iterations The number of iterations for gradient descent. Default is 1000.
    initialize = function(learning_rate = 0.01, iterations = 1000) {
      private$learning_rate <- learning_rate
      private$iterations <- iterations
    },

    #' Train the model using gradient descent
    #' @param X The feature matrix.
    #' @param y The response vector.
    fit = function(X, y) {
      private$X <- cbind(1, X)
      private$y <- y
      private$n_features <- ncol(private$X)
      private$beta <- rep(0, private$n_features)

      for (i in 1:private$iterations) {
        z <- private$X %*% private$beta
        h <- private$sigmoid(z)
        gradient <- t(private$X) %*% (h - private$y) / length(private$y)
        private$beta <- private$beta - private$learning_rate * gradient
      }

      # Store log-likelihood
      private$ll <- self$log_likelihood()

      return(self)
    },

    #' Predict probabilities
    #' @param new_X The new feature matrix.
    #' @return A vector of predicted probabilities.
    predict_proba = function(new_X) {
      new_X_with_intercept <- cbind(1, new_X)
      private$sigmoid(new_X_with_intercept %*% private$beta)
    },

    #' Predict classes
    #' @param new_X The new feature matrix.
    #' @param threshold The threshold for class prediction. Default is 0.5.
    #' @return A vector of predicted classes.
    predict = function(new_X, threshold = 0.5) {
      proba <- self$predict_proba(new_X)
      as.numeric(proba >= threshold)
    },

    #' Log-likelihood calculation
    #' @return The log-likelihood of the model.
    log_likelihood = function() {
      z <- private$X %*% private$beta
      sum(private$y * z - log(1 + exp(z)))
    },

    #' Getter for coefficients
    #' @return The coefficients of the logistic regression model.
    get_coefficients = function() {
      private$beta
    },

    #' Getter for log-likelihood
    #' @return The log-likelihood of the model.
    get_log_likelihood = function() {
      private$ll
    }
  ),

  private = list(
    X = NULL,
    y = NULL,
    beta = NULL,
    learning_rate = NULL,
    iterations = NULL,
    n_features = NULL,
    ll = NULL,

    sigmoid = function(z) {
      # Sigmoid function
      1 / (1 + exp(-z))
    }
  )
)

