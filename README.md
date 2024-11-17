# Logistic Regression Package

This package provides tools for performing logistic regression analysis in R.

## Installation

To install the package, use the following command in R:

```R
install.packages("LogRegSISEM2")
```

## Usage

Here is a basic example of how to use the package:

```R
library(LogRegSISEM2)

# Load your data
data <- read.csv("path/to/your/data.csv")

# Fit a logistic regression model
model <- log_reg(data, response_variable, predictor_variables)

# Summarize the model
summary(model)
```

## Functions

- `log_reg(data, response, predictors)`: Fits a logistic regression model.
- `summary(model)`: Provides a summary of the fitted model.

## License

This project is licensed under the MIT License.

## Contributing

| Nom                        | GitHub Profile                     |
|----------------------------|------------------------------------|
| ALEXIS DARDELET             | [pbrbn](https://github.com/AlexisDardelet)  |
| BERTRAND KLEIN             | [bertrandklein](https://github.com/bertrandklein) |
| JUAN DIEGO ALFONSO OCAMPO  | [jdalfons](https://github.com/jdalfons) |


## Contact

For any questions or inquiries, please contact Juan Diego A. at [jalfonsooc@univ-lyon2.fr](mailto:jalfonsooc@univ-lyon2.fr).
