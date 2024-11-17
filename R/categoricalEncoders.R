#' CategoricalVerifier Class
#'
#' A class for verifying and encoding categorical variables in a dataset.
#'
#' @section Fields:
#' \describe{
#'   \item{dataset}{A `data.frame` containing the dataset.}
#'   \item{categorical_vars}{A `character` vector of categorical variable names in the dataset.}
#'   \item{encoded_vars}{A `list` to store encoded variables.}
#'   \item{encoding_dict}{A `list` mapping variable names to their desired encoding methods.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(dataset, encoding_dict = NULL)}}{
#'     Initializes the class with a dataset and an optional encoding dictionary.
#'   }
#'   \item{\code{verify_categorical_vars()}}{
#'     Identifies categorical variables in the dataset.
#'   }
#'   \item{\code{label_encoding(var)}}{
#'     Encodes a variable using label encoding.
#'   }
#'   \item{\code{one_hot_encoding(var)}}{
#'     Encodes a variable using one-hot encoding.
#'   }
#'   \item{\code{frequency_encoding(var)}}{
#'     Encodes a variable using frequency encoding.
#'   }
#'   \item{\code{binary_encoding(var)}}{
#'     Encodes a variable using binary encoding.
#'   }
#'   \item{\code{apply_encoding()}}{
#'     Applies the specified encodings to categorical variables.
#'   }
#'   \item{\code{get_dataset()}}{
#'     Returns the modified dataset.
#'   }
#' }
#'
#' @examples
#' # Create a sample dataset
#' data <- data.frame(
#'   Category1 = factor(c("A", "B", "A", "C")),
#'   Category2 = c("X", "Y", "X", "Z"),
#'   Value = c(10, 20, 30, 40)
#' )
#'
#' # Define encoding preferences
#' encoding_dict <- list(
#'   Category1 = "one_hot",
#'   Category2 = "frequency"
#' )
#'
#' # Instantiate the class
#' verifier <- CategoricalVerifier$new(data, encoding_dict)
#'
#' # Apply the encodings
#' verifier$apply_encoding()
#'
#' # View the modified dataset
#' verifier$get_dataset()
#'
#' @export

library(R6)

CategoricalVerifier <- R6Class("CategoricalVerifier",
  public = list(
    dataset = NULL,
    categorical_vars = NULL,
    encoded_vars = list(),
    encoding_dict = NULL,
    
    initialize = function(dataset, encoding_dict = NULL) {
      self$dataset <- dataset
      self$categorical_vars <- self$verify_categorical_vars()
      self$encoding_dict <- encoding_dict
    },
    
    verify_categorical_vars = function() {
      cat_vars <- sapply(self$dataset, function(x) is.factor(x) || is.character(x))
      names(cat_vars[cat_vars])
    },
    
    label_encoding = function(var) {
      self$dataset[[var]] <- as.integer(as.factor(self$dataset[[var]]))
    },
    
    one_hot_encoding = function(var) {
      library(dummies)
      dummies <- dummy(self$dataset[[var]], sep = "_")
      self$dataset <- cbind(self$dataset, dummies)
      self$dataset[[var]] <- NULL
    },
    
    frequency_encoding = function(var) {
      freq_table <- table(self$dataset[[var]]) / nrow(self$dataset)
      self$dataset[[var]] <- as.numeric(freq_table[self$dataset[[var]]])
    },
    
    binary_encoding = function(var) {
      library(mltools)
      binary_encoded <- mltools::binary_encoding(self$dataset[[var]])
      colnames(binary_encoded) <- paste(var, colnames(binary_encoded), sep = "_")
      self$dataset <- cbind(self$dataset, binary_encoded)
      self$dataset[[var]] <- NULL
    },
    
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
    },
    
    get_dataset = function() {
      return(self$dataset)
    }
  )
)
