#' Logistic Regression Constructor
#'
#' Creates an object to perform logistic regression on a given dataset.
#'
#' @param formula A formula specifying the model (e.g., `y ~ x1 + x2`).
#' @param data A data frame containing the variables in the model.
#' @return A logistic regression model object with methods for fitting, predicting, and summarizing.
#' @export
logistic_regression <- function(formula, data) {
  # Initialize an empty list to store model information
  model <- list(
    formula = formula,
    data = data,
    coefficients = NULL,
    fitted_model = NULL
  )
  
  # Set the class of the model object to "logistic_regression"
  class(model) <- "logistic_regression"
  
  return(model)
}

#' Fit Logistic Regression Model
#'
#' Fits the logistic regression model to the data.
#'
#' @param model A logistic_regression object.
#' @return The fitted model with coefficients.
#' @export
fit <- function(model) {
  if (class(model) != "logistic_regression") {
    stop("The object is not of class 'logistic_regression'")
  }
  
  # Fit the logistic regression model using glm with binomial family
  model$fitted_model <- glm(model$formula, data = model$data, family = binomial())
  
  # Extract coefficients
  model$coefficients <- coef(model$fitted_model)
  
  return(model)
}

#' Predict Using Logistic Regression Model
#'
#' Makes predictions using the fitted logistic regression model.
#'
#' @param model A fitted logistic_regression object.
#' @param new_data A data frame containing new data for prediction.
#' @return A vector of predicted probabilities.
#' @export
predict.logistic_regression <- function(model, new_data) {
  if (is.null(model$fitted_model)) {
    stop("The model has not been fitted yet. Use the fit() function first.")
  }
  
  # Generate predicted probabilities
  predictions <- predict(model$fitted_model, new_data, type = "response")
  
  return(predictions)
}

#' Summarize Logistic Regression Model
#'
#' Provides a summary of the fitted logistic regression model.
#'
#' @param model A fitted logistic_regression object.
#' @return A summary of the model.
#' @export
summary.logistic_regression <- function(model) {
  if (is.null(model$fitted_model)) {
    stop("The model has not been fitted yet. Use the fit() function first.")
  }
  
  # Generate and return model summary
  return(summary(model$fitted_model))
}
