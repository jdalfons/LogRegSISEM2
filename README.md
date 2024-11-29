
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LogRegSISEM2

<!-- badges: start -->
<!-- badges: end -->

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

### Categorical var encoder

Here is an example of how to use the categorical variable encoder with
the Titanic dataset:

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
encoding_dict = list(Sex = "frequency", Age = "binary")

# Create the categorical verifier
verifier <- CategoricalVerifier$new(data, encoding_dict = encoding_dict)

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

On the following exemple we will use iris dataset to

``` r
library(caret)
library(LogRegSISEM2)

data(iris)
  
# Convert to binary classification (setosa vs. others)
iris_binary <- iris[iris$Species != "versicolor", ]
iris_binary$Species <- ifelse(iris_binary$Species == "setosa", 1, 0)

# Prepare features and target
X <- as.matrix(iris_binary[, c("Sepal.Length", "Sepal.Width")])
y <- iris_binary$Species

# Split data
train_indices <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]
```

Apply logistic regression

``` r
library(LogRegSISEM2)

reglog_model <- LogisticRegression$new(learning_rate = 0.01, iterations = 1000)
reglog_start_time <- Sys.time()
reglog_model$fit(X_train, y_train)
reglog_pred_proba <- reglog_model$predict_proba(X_test)
reglog_pred_class <- reglog_model$predict(X_test)
reglog_end_time <- Sys.time()
```

Get performance mesures

``` r
# Calculate R6 Model Performance
reglog_accuracy <- mean(reglog_pred_class == y_test)
reglog_log_likelihood <- reglog_model$get_log_likelihood()
reglog_coefficients <- reglog_model$get_coefficients()

# Output results
cat("Accuracy:", reglog_accuracy, "\n")
cat("Log-Likelihood:", reglog_log_likelihood, "\n")
cat("Coefficients:", reglog_coefficients, "\n")
```

\*\* Output: \*\*

``` sh
> Accuracy: 1 
> Log-Likelihood: -24.07341 
> Coefficients: 0.3097684 -0.9693679 1.620301 
```

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
