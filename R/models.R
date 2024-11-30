library(R6)

#' Logistic Regression Model with Advanced Gradient Descent Optimization
#'
#' This R6 class implements a flexible logistic regression model with gradient descent optimization,
#' supporting various regularization techniques and binary classification.
#'
#' @docType class
#' @name LogisticRegression
#' @import R6
#' @export
#' 
#' @details 
#' The LogisticRegression class provides a comprehensive implementation of logistic regression with the following features:
#' \itemize{
#'   \item Gradient descent optimization
#'   \item Optional L1 (Lasso) and L2 (Ridge) regularization
#'   \item Flexible binary classification with custom threshold
#'   \item Built-in model performance and diagnostic methods
#' }
#'
#' @examples
#' # Generate synthetic dataset using iris
#' data(iris)
#' 
#' # Convert to binary classification (setosa vs. others)
#' iris_binary <- iris[iris$Species != "versicolor", ]
#' iris_binary$Species <- ifelse(iris_binary$Species == "setosa", 1, 0)
#'
#' # Prepare features and target
#' X <- iris_binary[, c("Sepal.Length", "Sepal.Width")]
#' y <- iris_binary$Species
#'
#' # Split data
#' set.seed(123)
#' train_indices <- sample(seq_len(nrow(X)), size = 0.8 * nrow(X))
#' X_train <- X[train_indices, ]
#' # Model Get Y as factor
#' y_train <- as.factor(y[train_indices])
#' X_test <- X[-train_indices, ]
#' y_test <- y[-train_indices]
#'
#' # Create and train logistic regression model
#' reglog_model <- LogisticRegression$new(
#'   learning_rate = 0.01, 
#'   iterations = 1000, 
#'   lambda = 0.1
#' )
#' reglog_model$fit(X_train, y_train)
#' 
#' # Make predictions
#' predictions <- reglog_model$predict(X_test)
#' probabilities <- reglog_model$predict_proba(X_test)
#'
#' # Inspect model
#' reglog_model$summary()
#' 
LogisticRegression <- R6Class("LogisticRegression",
  public = list(
    #' # Constructor for Logistic Regression Model
    #'
    #' @param learning_rate Numeric learning rate for gradient descent (default: 0.01)
    #' @param iterations Number of gradient descent iterations (default: 1000)
    #' @param penalty Regularization type: 'none', 'l1', or 'l2' (default: 'none')
    #' @param lambda Regularization strength (default: 0)
    #'
    #' @return A new LogisticRegression object instance
    initialize = function(learning_rate = 0.01,
                          iterations = 1000,
                          penalty = 'none',
                          lambda = 0) {
      private$learning_rate <- learning_rate
      private$iterations <- iterations
      private$penalty <- match.arg(penalty, c('none', 'l1', 'l2'))
      private$lambda <- lambda

      # Validate inputs
      if (learning_rate <= 0) stop("Learning rate must be positive")
      if (iterations <= 0) stop("Iterations must be positive")
      if (lambda < 0) stop("Lambda must be non-negative")
    },

    #' Fit the Logistic Regression Model
    #'
    #' @param X Input feature matrix or data frame
    #' @param y Target variable vector
    #'
    #' @return The fitted model object (self)
    fit = function(X, y) {
      # Validate inputs
      if (!is.data.frame(X)) stop("X must be a data frame")
      if (!(is.factor(y) || is.character(y))) stop("y must be a factor or character")

      # Convert to binary classification if needed
      private$levels <- unique(y)
      if (length(private$levels) > 2) {
        warning("Multiple classes detected. Converting to binary classification")
        y <- as.factor(y == private$levels[1])
      }

      # Convert y to numeric
      y_numeric <- as.numeric(y) - 1

      # Add intercept term
      private$X <- as.matrix(cbind(1, X))
      private$y <- y_numeric
      private$n_features <- ncol(private$X)

      # Initialize coefficients
      private$beta <- rep(0, private$n_features)

      # Gradient descent with optional regularization
      for (i in 1:private$iterations) {
        z <- private$X %*% private$beta
        h <- private$sigmoid(z)

        # Calculate gradient
        gradient <- t(private$X) %*% (h - private$y) / length(private$y)

        # Add regularization
        if (private$penalty == 'l2') {
          # L2 regularization (ridge)
          gradient <- gradient + private$lambda * private$beta
        } else if (private$penalty == 'l1') {
          # L1 regularization (lasso)
          gradient <- gradient + private$lambda * sign(private$beta)
        }

        # Update coefficients
        private$beta <- private$beta - private$learning_rate * gradient
      }

      # Store log-likelihood
      private$ll <- self$log_likelihood()

      # Store feature names
      private$feature_names <- colnames(X)

      # Calculate class frequencies
      private$class_frequencies <- table(y) / length(y)

      return(self)
    },

    #' Predict Probabilities
    #'
    #' @param new_X New data for prediction
    #'
    #' @return Vector of predicted probabilities
    predict_proba = function(new_X) {
      # Validate input
      if (!is.data.frame(new_X)) stop("new_X must be a data frame")

      # Ensure same columns as training data
      if (!all(colnames(new_X) %in% private$feature_names)) {
        stop("new_X must have same columns as training data")
      }

      # Reorder columns to match training data
      new_X <- new_X[, private$feature_names, drop = FALSE]

      # Add intercept term
      new_X_with_intercept <- as.matrix(cbind(1, new_X))
      proba <- private$sigmoid(new_X_with_intercept %*% private$beta)

      return(proba)
    },

    #' Predict Classes
    #'
    #' @param new_X New data for prediction
    #' @param threshold Probability threshold for classification (default: 0.5)
    #'
    #' @return Vector of predicted classes
    predict = function(new_X, threshold = 0.5) {
      proba <- self$predict_proba(new_X)

      # Convert probabilities to classes
      predicted_classes <- ifelse(proba >= threshold,
                                  as.character(private$levels[1]),
                                  as.character(private$levels[2]))

      return(predicted_classes)
    },

    #' Print Model Information
    #'
    #' @return NULL (prints to console)
    print = function() {
      cat("Logistic Regression Model\n")
      cat("----------------------\n")
      cat("Regularization:", private$penalty, "\n")
      cat("Regularization Strength (lambda):", private$lambda, "\n")
      cat("Number of Features:", private$n_features - 1, "\n")
      cat("Number of Iterations:", private$iterations, "\n")
      cat("Learning Rate:", private$learning_rate, "\n")
      cat("\nClass Frequencies:\n")
      print(private$class_frequencies)
    },

    #' Summarize Model
    #'
    #' @return NULL (prints to console)
    summary = function() {
      # Basic print information
      self$print()

      # Detailed coefficient information
      cat("\nModel Coefficients:\n")
      coef <- self$get_coefficients()
      coef_df <- data.frame(
        Feature = names(coef),
        Coefficient = unname(coef)
      )
      print(coef_df)

      # Additional model statistics
      cat("\nModel Statistics:\n")
      cat("Log-Likelihood:", self$get_log_likelihood(), "\n")
    },

    #' Calculate Log-Likelihood
    #'
    #' @return Numeric log-likelihood value
    log_likelihood = function() {
      z <- private$X %*% private$beta
      sum(private$y * z - log(1 + exp(z)))
    },

    #' Get Model Coefficients
    #'
    #' @return Named vector of coefficients
    get_coefficients = function() {
      names(private$beta) <- c('(Intercept)', private$feature_names)
      private$beta
    },

    #' Get Log-Likelihood
    #'
    #' @return Stored log-likelihood value
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
    penalty = NULL,
    lambda = NULL,
    feature_names = NULL,
    levels = NULL,
    class_frequencies = NULL,

    #' Sigmoid activation function
    #'
    #' z Input values
    #' return Sigmoid transformed values
    sigmoid = function(z) {
      1 / (1 + exp(-z))
    }
  )
)
