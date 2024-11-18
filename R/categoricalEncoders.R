#' @title CategoricalVerifier Class
#' @description A class to handle encoding of categorical variables in a dataset.
#'
#' This class provides various encoding methods such as label encoding, one-hot encoding,
#' frequency encoding, and binary encoding. It can dynamically apply these encodings based
#' on user-defined preferences or defaults.
#'
#' @section Public Methods:
#' \describe{
#'   \item{\code{initialize(dataset, encoding_dict = NULL)}}{
#'     Initializes the CategoricalVerifier with a dataset and an optional encoding dictionary.
#'   }
#'   \item{\code{verify_categorical_vars()}}{
#'     Identifies and returns the names of categorical variables in the dataset.
#'   }
#'   \item{\code{label_encoding(var)}}{
#'     Encodes a categorical variable using label encoding.
#'   }
#'   \item{\code{one_hot_encoding(var)}}{
#'     Encodes a categorical variable using one-hot encoding.
#'   }
#'   \item{\code{frequency_encoding(var)}}{
#'     Encodes a categorical variable using frequency encoding.
#'   }
#'   \item{\code{binary_encoding(var)}}{
#'     Encodes a categorical variable using binary encoding.
#'   }
#'   \item{\code{apply_encoding()}}{
#'     Applies the specified encoding methods to all categorical variables in the dataset.
#'   }
#'   \item{\code{get_dataset()}}{
#'     Returns the processed dataset after applying the encodings.
#'   }
#' }
#'
#' @section Fields:
#' \describe{
#'   \item{\code{dataset}}{A \code{data.frame} containing the dataset to be processed.}
#'   \item{\code{categorical_vars}}{A character vector of the names of categorical variables.}
#'   \item{\code{encoded_vars}}{A list to store information about the encoded variables.}
#'   \item{\code{encoding_dict}}{A named list specifying the encoding methods for each categorical variable.}
#' }
#'
#' @examples
#' # Sample dataset
#' data <- data.frame(
#'   Category1 = factor(c("A", "B", "A", "C")),
#'   Category2 = c("X", "Y", "X", "Z"),
#'   Value = c(10, 20, 30, 40)
#' )
#'
#' # Create an instance of CategoricalVerifier
#' verifier <- CategoricalVerifier$new(data, encoding_dict = list(Category1 = "one_hot"))
#'
#' # Apply encodings
#' verifier$apply_encoding()
#'
#' # Get the processed dataset
#' processed_data <- verifier$get_dataset()
#' print(processed_data)
#'
#' @name CategoricalVerifier
#' @export
library(R6)
CategoricalVerifier <- R6Class("CategoricalVerifier",
  public = list(
    #' @field dataset The dataset to be processed.
    dataset = NULL,

    #' @field categorical_vars Names of categorical variables in the dataset.
    categorical_vars = NULL,

    #' @field encoded_vars A list of encoded variables.
    encoded_vars = list(),

    #' @field encoding_dict A dictionary specifying encoding methods for each variable.
    encoding_dict = NULL,

    #' @description
    #' Initialize the CategoricalVerifier class.
    #' @param dataset A \code{data.frame} containing the dataset to be processed.
    #' @param encoding_dict A named list specifying encoding methods for categorical variables.
    #'   Defaults to NULL, which uses label encoding for all variables.
    initialize = function(dataset, encoding_dict = NULL) {
      self$dataset <- dataset
      self$categorical_vars <- self$verify_categorical_vars()
      self$encoding_dict <- encoding_dict
    },

    #' @description
    #' Identify categorical variables in the dataset.
    #' @return A character vector containing the names of categorical variables.
    verify_categorical_vars = function() {
      cat_vars <- sapply(self$dataset, function(x) is.factor(x) || is.character(x))
      names(cat_vars[cat_vars])
    },

    #' @description
    #' Apply label encoding to a variable.
    #' @param var A character string representing the variable name.
    label_encoding = function(var) {
      self$dataset[[var]] <- as.integer(as.factor(self$dataset[[var]]))
    },

    #' @description
    #' Apply one-hot encoding to a variable.
    #' @param var A character string representing the variable name.
    one_hot_encoding = function(var) {
      library(dummies)
      dummies <- dummy(self$dataset[[var]], sep = "_")
      self$dataset <- cbind(self$dataset, dummies)
      self$dataset[[var]] <- NULL
    },

    #' @description
    #' Apply frequency encoding to a variable.
    #' @param var A character string representing the variable name.
    frequency_encoding = function(var) {
      freq_table <- table(self$dataset[[var]]) / nrow(self$dataset)
      self$dataset[[var]] <- as.numeric(freq_table[self$dataset[[var]]])
    },

    #' @description
    #' Apply binary encoding to a variable.
    #' @param var A character string representing the variable name.
    binary_encoding = function(var) {
      library(mltools)
      binary_encoded <- mltools::binary_encoding(self$dataset[[var]])
      colnames(binary_encoded) <- paste(var, colnames(binary_encoded), sep = "_")
      self$dataset <- cbind(self$dataset, binary_encoded)
      self$dataset[[var]] <- NULL
    },

    #' @description
    #' Apply specified encodings to all categorical variables in the dataset.
    #' If \code{encoding_dict} is NULL, label encoding is applied to all variables.
    #' @return A \code{data.frame} containing the dataset with encoded variables.
    apply_encoding = function() {
      if (is.null(self$encoding_dict)) {
        for (var in self$categorical_vars) {
          self$label_encoding(var)
        }
      } else {
        for (var in self$categorical_vars) {
          encoding_method <- self$encoding_dict[[var]]
          
          if (is.null(encoding_method)) {
            self$label_encoding(var)
          } else if (encoding_method == "label") {
            self$label_encoding(var)
          } else if (encoding_method == "one_hot") {
            self$one_hot_encoding(var)
          } else if (encoding_method == "frequency") {
            self$frequency_encoding(var)
          } else if (encoding_method == "binary") {
            self$binary_encoding(var)
          } else {
            stop(paste("Unknown encoding method for variable:", var))
          }
        }
      }
      return(self$dataset)
    },

    #' @description
    #' Retrieve the processed dataset.
    #' @return A \code{data.frame} containing the processed dataset.
    get_dataset = function() {
      return(self$dataset)
    }
  )
)
