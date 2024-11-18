
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LogRegSISEM2

<!-- badges: start -->
<!-- badges: end -->

The goal of LogRegSISEM2 is to …

## Installation

You can install the development version of LogRegSISEM2 like so:

Ensure you have `DevTools` installed

To install the package, use the following command in R:

``` r
install_github("jdalfons/LogRegSISEM2")
```

## Usage

Here is a basic example of how to use the package:

``` r
library(LogRegSISEM2)

# Load your data
data <- read.csv("path/to/your/data.csv")

# Fit a logistic regression model
model <- log_reg(data, response_variable, predictor_variables)

# Summarize the model
summary(model)
```

## Functions

- `log_reg(data, response, predictors)`: Fits a logistic regression
  model.
- `summary(model)`: Provides a summary of the fitted model.

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
