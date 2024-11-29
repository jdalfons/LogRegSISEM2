
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

## Use

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

```r
data <- data.frame(
  Category1 = factor(c("A", "B", "A", "C", "A", "C")),
  Category2 = c("X", "Y", "X", "Z", "T", "H"),
  Value = c(10, 20, 30, 40, 60, 60)
)

verifier <- CategoricalVerifier$new(data, encoding_dict = list(Category1 = "frequency", Category2 = "binary"))
verifier$get_dataset()

```
# Output of verifier$get_dataset()
# A tibble: 6 × 3
  Category1 Category2 Value
  <fct>     <chr>     <dbl>
1 A         X            10
2 B         Y            20
3 A         X            30
4 C         Z            40
5 A         T            60
6 C         H            60
```

```r
verifier$apply_encoding()
```
Output

```r

```r
# Output of verifier$apply_encoding()
# A tibble: 6 × 3
  Category1 Category2 Value
  <dbl>     <dbl>     <dbl>
1 0.5       1            10
2 0.25      0            20
3 0.5       1            30
4 0.25      0            40
5 0.5       0            60
6 0.25      0            60
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
