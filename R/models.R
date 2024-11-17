#' Multinomial Logistic Regression Class
#'
#' This class implements multinomial logistic regression from scratch using base R functions.
#' It provides methods to fit the model, make predictions, and summarize the results.
#'
#' @section Public Methods:
#' \describe{
#'   \item{\code{initialize(formula, data)}}{
#'     Initializes the model with a formula and dataset. Prepares the design matrix and response vector.
#'   }
#'   \item{\code{fit()}}{
#'     Fits the multinomial logistic regression model by maximizing the likelihood function using \code{optim}.
#'   }
#'   \item{\code{predict(new_data)}}{
#'     Predicts probabilities for new data using the fitted model.
#'   }
#'   \item{\code{summary()}}{
#'     Summarizes the fitted model, including coefficients and log-likelihood.
#'   }
#' }
#'
#' @examples
#' # Sample dataset
#' set.seed(123)
#' data <- data.frame(
#'   Category = factor(sample(c("A", "B", "C"), size = 100, replace = TRUE)),
#'   Predictor = rnorm(100),
#'   Response = factor(sample(c("Class1", "Class2", "Class3"), size = 100, replace = TRUE))
#' )
#'
#' # Instantiate the model
#' model <- MultinomialLogisticRegression$new(Response ~ Predictor + Category, data)
#'
#' # Fit the model
#' model$fit()
#'
#' # Predict probabilities for new data
#' new_data <- data.frame(
#'   Predictor = c(0.5, -1.2),
#'   Category = factor(c("A", "B"))
#' )
#' predictions <- model$predict(new_data)
#' print(predictions)
#'
#' # Summarize the fitted model
#' summary <- model$summary()
#' print(summary)
#'
#' @export
MultinomialLogisticRegression <- R6Class("MultinomialLogisticRegression",
  public = list(
    #' @field formula The formula specifying the model.
    formula = NULL,
    
    #' @field data The dataset used for fitting the model.
    data = NULL,
    
    #' @field X The design matrix of predictors.
    X = NULL,
    
    #' @field y The response vector.
    y = NULL,
    
    #' @field num_classes The number of unique response classes.
    num_classes = NULL,
    
    #' @field coefficients The estimated coefficients of the model.
    coefficients = NULL,
    
    #' @field fitted_model The output of the optimization procedure.
    fitted_model = NULL,

    #' @description
    #' Initialize the multinomial logistic regression model.
    #' @param formula A formula specifying the response and predictors.
    #' @param data A data.frame containing the dataset.
    initialize = function(formula, data) {
      self$formula <- formula
      self$data <- data

      # Extract response and predictor matrices from the formula
      mf <- model.frame(formula, data)
      self$y <- model.response(mf)
      self$X <- model.matrix(~ . - 1, mf[, -1]) # Drop the intercept as we add it manually later

      # Determine the number of classes
      self$num_classes <- length(unique(self$y))
    },

    #' @description
    #' Fit the multinomial logistic regression model.
    fit = function() {
      # Initialize parameters
      initial_params <- rep(0, ncol(self$X) * (self$num_classes - 1))

      # Fit the model using optim
      self$fitted_model <- optim(
        par = initial_params,
        fn = private$multinomial_log_likelihood,
        X = self$X,
        y = self$y,
        num_classes = self$num_classes,
        method = "BFGS",
        control = list(maxit = 1000)
      )

      # Store the fitted coefficients
      self$coefficients <- self$fitted_model$par
    },

    #' @description
    #' Predict probabilities for new data using the fitted model.
    #' @param new_data A data.frame containing the predictors for the new observations.
    #' @return A matrix of probabilities, where each row corresponds to a new observation
    #'   and each column corresponds to a class.
    predict = function(new_data) {
      if (is.null(self$fitted_model)) {
        stop("The model has not been fitted yet. Use the fit() method first.")
      }

      # Preprocess the new data
      new_X <- model.matrix(~ . - 1, new_data)

      # Add intercept
      new_X <- cbind(1, new_X)

      # Reshape coefficients into a matrix
      beta <- matrix(self$coefficients, ncol = self$num_classes - 1)
      beta <- cbind(0, beta)

      # Calculate linear predictors
      eta <- new_X %*% beta

      # Calculate probabilities using the softmax function
      exp_eta <- exp(eta)
      probs <- exp_eta / rowSums(exp_eta)
      return(probs)
    },

    #' @description
    #' Summarize the fitted model.
    #' @return A list containing the model coefficients, log-likelihood, and number of iterations.
    summary = function() {
      if (is.null(self$fitted_model)) {
        stop("The model has not been fitted yet. Use the fit() method first.")
      }

      return(list(
        coefficients = self$coefficients,
        log_likelihood = -self$fitted_model$value,
        num_iterations = self$fitted_model$counts
      ))
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
      # Reshape coefficients into a matrix
      beta <- matrix(params, ncol = num_classes - 1)
      beta <- cbind(0, beta) # Add intercept for the reference class

      # Linear predictors
      eta <- X %*% beta

      # Softmax probabilities
      exp_eta <- exp(eta)
      probs <- exp_eta / rowSums(exp_eta)

      # Negative log-likelihood
      y_matrix <- model.matrix(~ y - 1)  # One-hot encoding of response variable
      -sum(y_matrix * log(probs))
    }
  )
)
