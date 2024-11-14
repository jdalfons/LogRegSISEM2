
library(R6)

LogisticRegression <- R6Class("LogisticRegression",
    public = list(
        formula = NULL,
        data = NULL,
        coefficients = NULL,
        fitted_model = NULL,

        initialize = function(formula, data) {
            self$formula <- formula
            self$data <- data
        },

        fit = function() {
            self$fitted_model <- glm(self$formula, data = self$data, family = binomial())
            self$coefficients <- coef(self$fitted_model)
        },

        predict = function(new_data) {
            if (is.null(self$fitted_model)) {
                stop("The model has not been fitted yet. Use the fit() method first.")
            }
            predictions <- predict(self$fitted_model, new_data, type = "response")
            return(predictions)
        },

        summary = function() {
            if (is.null(self$fitted_model)) {
                stop("The model has not been fitted yet. Use the fit() method first.")
            }
            return(summary(self$fitted_model))
        }
    )
)
