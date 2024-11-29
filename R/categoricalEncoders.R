#' @title Categorical Encoding Functions
#' 
#' @description
#' Functions to handle encoding of categorical variables in a dataset.
#'
#' These functions provide various encoding methods such as label encoding, one-hot encoding,
#' frequency encoding, and binary encoding. They can dynamically apply these encodings based
#' on user-defined preferences.
#'
#' @usage 
#' # Sample dataset
#' data <- data.frame(
#'   Category1 = factor(c("A", "B", "A", "C")),
#'   Category2 = c("X", "Y", "X", "Z"),
#'   Value = c(10, 20, 30, 40)
#' )
#' 
#' # Encode 'Category1' using label encoding
#' encoded_data_label <- label_encoding(data, "Category1")
#' print(encoded_data_label)
#' 
#' # Encode 'Category2' using one-hot encoding
#' encoded_data_one_hot <- one_hot_encoding(data, "Category2")
#' print(encoded_data_one_hot)
#' 
#' # Encode 'Category1' using frequency encoding
#' encoded_data_frequency <- frequency_encoding(data, "Category1")
#' print(encoded_data_frequency)
#' 
#' # Encode 'Category2' using binary encoding
#' encoded_data_binary <- binary_encoding(data, "Category2")
#' print(encoded_data_binary)
#' 
#' 
#' # Choose any of the categories available to encode: 
#' # label = label encoding (Default), 
#' # one_hot = One-Hot Encoding, 
#' # frequency = frequency encoding, 
#' # binary = binary encoding
#' 
#' # Note:
#' # If you don't specify any encoding method, it will use default encoding.
#' 
#' 
#' # Get the processed dataset
#' print(processed_data)
#' 

#' @description
#' Identify categorical variables in the dataset.
#' @param dataset A \code{data.frame} containing the dataset to be processed.
#' @return A character vector containing the names of categorical variables.
verify_categorical_vars <- function(dataset) {
  cat_vars <- sapply(dataset, function(x) is.factor(x) || is.character(x))
  names(cat_vars[cat_vars])
}

#' @title Get Variable Summary
#' @description This function returns a summary of the qualitative and quantitative variables in the dataset.
#' @param dataset A \code{data.frame} containing the dataset to be processed.
#' @return A data frame with two columns: 'Qualitatives' and 'Quantitatives', containing the names of the qualitative and quantitative variables, respectively. If one type has fewer variables, the remaining rows are filled with NA.
get_variable_summary <- function(dataset) {
  qualitative_vars <- verify_categorical_vars(dataset)
  quantitative_vars <- colnames(dataset)[sapply(dataset, is.numeric)]
  
  max_length <- max(length(qualitative_vars), length(quantitative_vars))
  data.frame(
    Qualitatives = c(qualitative_vars, rep(NA, max_length - length(qualitative_vars))),
    Quantitatives = c(quantitative_vars, rep(NA, max_length - length(quantitative_vars)))
  )
}

#' @description
#' Apply label encoding to a variable.
#' @param dataset A \code{data.frame} containing the dataset to be processed.
#' @param var A character string representing the variable name.
#' @return A \code{data.frame} with the encoded variable.
label_encoding <- function(dataset, var) {
  dataset[[var]] <- as.integer(as.factor(dataset[[var]]))
  return(dataset)
}

#' @description
#' Apply one-hot encoding to a variable.
#' @param dataset A \code{data.frame} containing the dataset to be processed.
#' @param var A character string representing the variable name.
#' @return A \code{data.frame} with the encoded variable.
one_hot_encoding <- function(dataset, var) {
  data_column <- dataset[[var]]
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
  dataset <- cbind(dataset, one_hot_matrix)
  dataset[[var]] <- NULL
  return(dataset)
}

#' @description
#' Apply frequency encoding to a variable.
#' @param dataset A \code{data.frame} containing the dataset to be processed.
#' @param var A character string representing the variable name.
#' @return A \code{data.frame} with the encoded variable.
frequency_encoding <- function(dataset, var) {
  freq_table <- table(dataset[[var]]) / nrow(dataset)
  dataset[[var]] <- as.numeric(freq_table[dataset[[var]]])
  return(dataset)
}

#' @description
#' Apply binary encoding to a variable.
#' @param dataset A \code{data.frame} containing the dataset to be processed.
#' @param var A character string representing the variable name.
#' @return A \code{data.frame} with the encoded variable.
binary_encoding <- function(dataset, var) {
  data_column <- dataset[[var]]
  unique_categories <- unique(data_column)
  category_map <- setNames(seq_along(unique_categories), unique_categories)
  max_bits <- ceiling(log2(length(unique_categories)))
  category_indices <- category_map[data_column]
  binary_strings <- sapply(category_indices, function(x) {
    paste0(rev(as.integer(intToBits(x))[1:max_bits]), collapse = "")
  })
  dataset[[paste0(var)]] <- binary_strings
  return(dataset)
}

