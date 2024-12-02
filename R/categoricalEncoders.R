library(R6)

#' @title CategoricalVerifier Class
#' 
#' @description
#' A class to handle encoding of categorical variables in a dataset.
#'
#' This class provides various encoding methods such as label encoding, one-hot encoding,
#' frequency encoding, and binary encoding. It can dynamically apply these encodings based
#' on user-defined preferences or defaults.
#'
#' @docType class
#' @name CategoricalVerifier
#' @import R6
#' @export
#' @field dataset The dataset to be processed.
#' @field categorical_vars Names of categorical variables in the dataset.
#' @field encoded_vars A list of encoded variables.
#' @field encoding_dict A dictionary specifying encoding methods for each variable.
#' @method CategoricalVerifier initialize
#' @method CategoricalVerifier verify_categorical_vars
#' @method CategoricalVerifier label_encoding
#' @method CategoricalVerifier one_hot_encoding
#' @method CategoricalVerifier frequency_encoding
#' @method CategoricalVerifier binary_encoding
#' @method CategoricalVerifier apply_encoding
#' @method CategoricalVerifier get_dataset
#'
#' @examples
#' # Load Titanic dataset
#' 
#' set.seed(123)
#' 
#' # Generate a sample dataset
#' data <- data.frame(
#'   Sex = sample(c("Male", "Female"), 100, replace = TRUE),
#'   Age = sample(c("Child", "Adult"), 100, replace = TRUE),
#'   Survived = sample(c("No", "Yes"), 100, replace = TRUE),
#'   Class = sample(c("1st", "2nd", "3rd", "Crew"), 100, replace = TRUE)
#' )
#' 
#' data$Sex <- factor(data$Sex)
#' data$Age <- factor(data$Age)
#' data$Survived <- factor(data$Survived, levels = c("No", "Yes"))
#' data$Class <- factor(data$Class)
#' 
#' # Choose any of the categories available to encode: 
#' # label = label encoding (Default), 
#' # one_hot = One-Hot Encoding, 
#' # frequency = frequency encoding, 
#' # binary = binary encoding
#' 
#' # Create a Hash table of encoding for variables we want a different encoding than 
#' # the default one (Label encoding)
#' encoding_dict = list(Sex = "frequency", Age = "binary")
#' 
#' # Create the categorical verifier
#' verifier <- CategoricalVerifier$new(data, encoding_dict = encoding_dict)
#' 
#' # Apply the encoding
#' verifier$apply_encoding()
#' 
#' 
#' # Note:
#' # If you don't specify any encoding method, it will use default encoding (Label Encoding).
#' 
#' # Get the processed dataset
#' processed_data <- verifier$get_dataset()
#' print(processed_data)
#'

CategoricalVerifier <- R6Class("CategoricalVerifier",
  public = list(
    dataset = NULL,
    target_var = NULL,
    categorical_vars = NULL,
    encoded_vars = list(),
    encoding_dict = NULL,

    #' @description
    #' Initialize the CategoricalVerifier class.
    #' @param dataset A \code{data.frame} containing the dataset to be processed.
    #' @param encoding_dict A named list specifying encoding methods for categorical variables.
    #'   Defaults to NULL, which uses label encoding for all variables.
    #' @param target_var A character string representing the name of the target variable.
    initialize = function(dataset, encoding_dict = NULL, target_var = NULL) {
      if (is.null(target_var)) {
        stop("Error: No target variable specified for logistic regression.")
      }
      self$dataset <- dataset
      self$categorical_vars <- self$verify_categorical_vars()
      self$encoding_dict <- encoding_dict
      self$target_var <- target_var
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
      data_column <- self$dataset[[var]]
      unique_categories <- unique(data_column)
      n <- length(data_column)
      k <- length(unique_categories)
      one_hot_matrix <- matrix(0, nrow = n, ncol = k)
      for (i in seq_along(data_column)) {
        category <- data_column[i]
        category_index <- which(unique_categories == category)
        one_hot_matrix[i, category_index] <- 1
      }
      colnames(one_hot_matrix) <- paste0(var, "_", unique_categories)
      self$dataset <- cbind(self$dataset, one_hot_matrix)
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
      data_column <- self$dataset[[var]]
      unique_categories <- unique(data_column)
      category_map <- setNames(seq_along(unique_categories), unique_categories)
      max_bits <- ceiling(log2(length(unique_categories)))
      category_indices <- category_map[data_column]
      binary_strings <- sapply(category_indices, function(x) {
        paste0(rev(as.integer(intToBits(x))[1:max_bits]), collapse = "")
      })
      self$dataset[[paste0(var)]] <- binary_strings
    },

    #' @description
    #' Apply specified encodings to all categorical variables in the dataset.
    #' If \code{encoding_dict} is NULL, label encoding is applied to all variables.
    #' @return A \code{data.frame} containing the dataset with encoded variables.
    apply_encoding = function() {
      if (is.null(self$encoding_dict)) {
        for (var in self$categorical_vars) {
            if (var != self$target_var) {
              self$label_encoding(var)
            }
          }
        } else {
        for (var in self$categorical_vars) {
          if (var != self$target_var) {
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
