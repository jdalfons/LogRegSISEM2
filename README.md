
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LogRegSISEM2

<!-- badges: start -->

[![R-CMD-check](https://github.com/jdalfons/M2-SISE-R-Library/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jdalfons/M2-SISE-R-Library/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

[![Last
Commit](https://img.shields.io/github/last-commit/jdalfons/LogRegSISEM2)](https://github.com/jdalfons/LogRegSISEM2)

LogRegSISEM2 is an R package that provides a custom implementation of
multinomial logistic regression using gradient descent optimization.
Designed for students of the M2 SISE program at Lyon in France, this
package includes various encoding methods for categorical variables and
tools to evaluate model performance.

## Features

- Multinomial logistic regression with gradient descent
- Encoding methods: label encoding, one-hot encoding, frequency
  encoding, and binary encoding
- Performance metrics: accuracy, log-likelihood, and model coefficients

## Installation

You can install the development version of LogRegSISEM2 like so:

Ensure you have `DevTools` installed and to install the package, use the
following command in R:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("jdalfons/LogRegSISEM2")
```

## Usage

### Categorical var encoder

<!-- Here is an example of how to use the categorical variable encoder with the Titanic dataset: -->

``` r
# Load Titanic dataset
library(LogRegSISEM2)
library(ggplot2)

data <- as.data.frame(Titanic)
data <- data[rep(seq_len(nrow(data)), data$Freq), ]
data$Freq <- NULL
data$Survived <- factor(data$Survived, levels = c("No", "Yes"))
data$Class <- factor(data$Class)
data$Sex <- factor(data$Sex)
data$Age <- factor(data$Age)
data
```

\*\* Output before `data`: \*\*

``` r
# A tibble: 2201 × 4
  Class  Sex   Age   Survived
  <fct>  <dbl> <dbl> <fct>   
  3rd      Female   Child   No
  3rd      Female   Child   No
  1st      Male   Adult No
  1st      Male   Adult No
  1st      Male   Adult No
  1st      Male   Adult No
  1st      Male   Adult No  
# ... with 2195 more rows
```

Choose any of the categories available to encode:

- label = label encoding (Default),
- one_hot = One-Hot Encoding,
- frequency = frequency encoding,
- binary = binary encoding

Create a Hash table of encoding for variables we want a different
encoding than the default one (Label encoding) in this case only for
exemple we will encode Sex and Age

``` r
encoding_dict = list(Sex = "frequency", Survived = "binary")

# Create the categorical verifier
verifier <- CategoricalVerifier$new(data, encoding_dict = encoding_dict, target_var = "Class")

# Apply the encoding
verifier$apply_encoding()

# Get the processed dataset
processed_data <- verifier$get_dataset()
print(processed_data)
```

\*\* Output of `verifier$apply_encoding()`: \*\*

``` r
# A tibble: 2201 × 4
  Class  Sex           Age   Survived
  <fct>  <dbl>         <dbl> <fct>   
  3      0.2135393      2     1      
  3      0.2135393      2     1      
  1      0.7864607      1     1      
  1      0.7864607      1     1      
  1      0.7864607      1     1      
  1      0.7864607      1     1      
# ... with 2195 more rows
```

### Logistic Regression model

On the following example we will use the diamonds dataset to

``` r
# Load required libraries
library(LogRegSISEM2)

# Load the diamonds dataset
data("diamonds", package = "ggplot2")
diamond_data <- diamonds

# Categorical Encoding
verifier <- CategoricalVerifier$new(diamond_data)
verifier$apply_encoding()
encoded_data <- verifier$get_dataset()

# Prepare the data
X <- encoded_data[, !names(encoded_data) %in% "cut"]
y <- ifelse(diamonds$cut == "Ideal", 1, 0)

# Initialize and fit the Logistic Regression model
model <- LogisticRegression$new(learning_rate = 0.05, num_iterations = 500)
model$fit(X, y)

# Predictions and Probabilities
predictions <- model$predict(X)
probabilities <- as.numeric(model$predict_proba(X))

# Print Model Metrics
cat("Model Summary:\n")
model$summary()

cat("\nFeature Importance:\n")
print(importance)

cat("\nModel Log-Likelihood:\n")
log_likelihood <- model$compute_cost(y, probabilities)
print(log_likelihood)

# Performance Metrics
accuracy <- mean(predictions == y)
cat("\nModel Accuracy:", accuracy, "\n")
```

## Contribute

To contribute to this project, please refer to the [contribution
guidelines](https://github.com/jdalfons/LogRegSISEM2/blob/main/contribute/contribute.md).

## License

This project is licensed under the MIT License.

## Contributing

| Nom | GitHub Profile |
|----|----|
| ALEXIS DARDELET | [pbrbn](https://github.com/AlexisDardelet) |
| BERTRAND KLEIN | [bertrandklein](https://github.com/bertrandklein) |
| JUAN DIEGO ALFONSO OCAMPO | [jdalfons](https://github.com/jdalfons) |

## Contact

For any questions or inquiries, please contact Juan Diego A. at
<jalfonsooc@univ-lyon2.fr>.
