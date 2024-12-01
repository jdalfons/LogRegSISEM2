# Changelog

All notable changes to this project will be documented in this file.

## [0.3.0] - 2024-11-30

### Added
- Added tests for `LogisticRegression` class.
- Added tests for categorical encoders.
- Implemented `get_coefficients` method in `LogisticRegression` class.
- Implemented `get_log_likelihood` method in `LogisticRegression` class.
- Improved summary output for `LogisticRegression` class.

### Changed
- Updated version number to 0.3.0.
- Improved documentation for `LogisticRegression` class.
- Enhanced the `apply_encoding` method in `CategoricalVerifier` class.

### Fixed
- Fixed issues with encoding methods in `CategoricalVerifier` class.
- Resolved errors in `devtools::check()` related to file paths.

## [0.2.0] - 2024-11-28

### Added
- Initial release of `LogRegSISEM2` package.
- Implemented multinomial logistic regression with gradient descent.
- Added various encoding methods for categorical variables.
- Included performance metrics: accuracy, log-likelihood, and model coefficients.