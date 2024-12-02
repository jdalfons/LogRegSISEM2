library(testthat)
library(LogRegSISEM2)

test_that("CategoricalVerifier encoding works correctly", {
  # Generate a sample dataset
  set.seed(123)
  data <- data.frame(
    Sex = sample(c("Male", "Female"), 100, replace = TRUE),
    Age = sample(c("Child", "Adult"), 100, replace = TRUE),
    Survived = sample(c("No", "Yes"), 100, replace = TRUE),
    Class = sample(c("1st", "2nd", "3rd", "Crew"), 100, replace = TRUE)
  )

  data$Sex <- factor(data$Sex)
  data$Age <- factor(data$Age)
  data$Survived <- factor(data$Survived, levels = c("No", "Yes"))
  data$Class <- factor(data$Class)

  # Create a Hash table of encoding for variables
  encoding_dict <- list(Sex = "frequency", Age = "binary")

  # Create the categorical verifier
  verifier <- CategoricalVerifier$new(data, encoding_dict = encoding_dict, target_var = "Survived")

  # Apply the encoding
  verifier$apply_encoding()
  encoded_data <- verifier$get_dataset()

  # Check that the encoded data has the correct structure
  expect_true("Sex" %in% colnames(encoded_data))
  expect_true("Age" %in% colnames(encoded_data))
  expect_true("Survived" %in% colnames(encoded_data))
  expect_true("Class" %in% colnames(encoded_data))

  # Check that the encoded values are numeric
  expect_true(is.numeric(encoded_data$Sex))
  expect_true(is.character(encoded_data$Age))

  # Check that the encoded values are within the expected range
  expect_true(all(encoded_data$Sex >= 0 & encoded_data$Sex <= 1))
  expect_true(all(encoded_data$Age >= 0 & encoded_data$Age <= 1))

  # Check that the Survived column values are equal to the original data$Survived values
  expect_equal(encoded_data$Survived, data$Survived)

})