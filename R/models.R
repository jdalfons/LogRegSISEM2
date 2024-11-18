#' @title Logistic Regression Class
#' @description
#' This class implements logistic regression for both binary and multinomial cases using base R functions.
#' It supports fitting the model, predicting probabilities, and summarizing the fitted model.
#'
#' @examples
#' # Binary Logistic Regression Example
#' set.seed(123)
#' data <- data.frame(
#'   x1 = rnorm(100),
#'   x2 = rbinom(100, 1, 0.5),
#'   y = factor(rbinom(100, 1, 0.5), levels = c(0, 1))
#' )
#' 
#' # Instantiate and fit binary logistic regression
#' binary_model <- LogisticRegression$new(y ~ x1 + x2, data)
#' binary_model$fit()
#' binary_model$summary()
#' 
#' # Multinomial Logistic Regression Example
#' data$y <- factor(sample(c("class1", "class2", "class3"), size = 100, replace = TRUE))
#' 
#' # Instantiate and fit multinomial logistic regression
#' multi_model <- LogisticRegression$new(y ~ x1 + x2, data)
#' multi_model$fit()
#' multi_model$summary()
#' 
#' @export
LogisticRegression <- R6Class("LogisticRegression",
  public = list(
    formula = NULL,
    data = NULL,
    X = NULL,
    y = NULL,
    coefficients = NULL,
    fitted_model = NULL,
    num_classes = NULL,
    is_binary = NULL,
    
    #' @description
    #' Initialize the LogisticRegression class.
    #' @param formula A formula specifying the response and predictors.
    #' @param data A data.frame containing the dataset.
    initialize = function(formula, data) {
      self$formula <- formula
      self$data <- data
      
      # Parse formula and create model frame
      mf <- model.frame(formula, data)
      self$y <- model.response(mf)
      self$X <- model.matrix(~ ., mf[, -1]) # Include intercept
      
      # Determine if binary or multinomial
      self$num_classes <- length(unique(self$y))
      self$is_binary <- self$num_classes == 2
    },
    
    #' @description
    #' Fit the logistic regression model.
    fit = function() {
      if (self$is_binary) {
        self$fitted_model <- glm(self$formula, data = self$data, family = binomial())
        self$coefficients <- coef(self$fitted_model)
      } else {
        # Multinomial case
        initial_params <- rep(0, ncol(self$X) * (self$num_classes - 1))
        self$fitted_model <- optim(
          par = initial_params,
          fn = private$multinomial_log_likelihood,
          X = self$X,
          y = self$y,
          num_classes = self$num_classes,
          method = "BFGS",
          control = list(maxit = 1000)
        )
        self$coefficients <- self$fitted_model$par
      }
    },
    
    #' @description
    #' Predict probabilities for new data using the fitted model.
    #' @param new_data A data.frame containing the predictors for new observations.
    #' @return A matrix of predicted probabilities.
    predict = function(new_data) {
      if (is.null(self$fitted_model)) {
        stop("Model has not been fitted yet. Call `fit()` first.")
      }
      
      new_X <- model.matrix(~ ., new_data)
      
      if (self$is_binary) {
        # Binary logistic regression
        eta <- new_X %*% self$coefficients
        probs <- 1 / (1 + exp(-eta))
        return(probs)
      } else {
        # Multinomial logistic regression
        beta <- matrix(self$coefficients, ncol = self$num_classes - 1)
        beta <- cbind(0, beta)
        eta <- new_X %*% beta
        exp_eta <- exp(eta)
        probs <- exp_eta / rowSums(exp_eta)
        return(probs)
      }
    },
    
    #' @description
    #' Summarize the fitted logistic regression model.
    #' @return A summary of the fitted model, including coefficients and performance metrics.
    summary = function() {
      if (is.null(self$fitted_model)) {
        stop("Model has not been fitted yet. Call `fit()` first.")
      }
      
      if (self$is_binary) {
        return(summary(self$fitted_model))
      } else {
        return(list(
          coefficients = self$coefficients,
          log_likelihood = -self$fitted_model$value,
          num_iterations = self$fitted_model$counts
        ))
      }
    }
  ),
  
  private = list(
    #' @description
    #' Multinomial logistic regression likelihood function.
    #' @param params A numeric vector of parameters to optimize.
    #' @param X A matrix of predictors.
    #' @param y A response vector.
    #' @param num_classes The number of unique response classes.
    multinomial_log_likelihood = function(params, X, y, num_classes) {
      beta <- matrix(params, ncol = num_classes - 1)
      beta <- cbind(0, beta) # Reference class
      
      eta <- X %*% beta
      exp_eta <- exp(eta)
      probs <- exp_eta / rowSums(exp_eta)
      
      y_matrix <- model.matrix(~ y - 1) # One-hot encoding
      -sum(y_matrix * log(probs))
    }
  )
)
