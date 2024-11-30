# Manual of Modifications and Documenting Code

## Using `renv` for Dependency Management

`renv` is a package that helps manage R dependencies for your project. Here are the steps to use `renv` effectively:

### Testing

We are using `testthat` as a library to create and run tests.

### Creating a New Test

1. To create a new test, add a file in the `tests/testthat/` directory with a name that reflects the purpose of the test, for example, `tests/testthat/test-your-feature.R`.

2. Write your test cases in the new file using the `testthat` functions. Here is an example:

    ```r
    test_that("example test", {
      expect_equal(1 + 1, 2)
    })
    ```

### Running Tests

1. To run the tests in the console, use the following commands:

    ```r
    library(testthat)
    test_dir("tests/testthat")
    ```

    This will execute all the test files in the `tests/testthat/` directory and display the results in the console.


### Initializing `renv`

1. To initialize `renv` in your project, run the following command:

    ```r
    renv::init()
    ```

    This will create a new `renv` environment and a `renv.lock` file that records the state of your project's dependencies.

### Installing New Libraries

1. To install a new library and add it to your `renv` environment, use the `install` function:

    ```r
    renv::install("package_name")
    ```

    Replace `"package_name"` with the name of the package you want to install.

2. After installing the new package, update the `renv.lock` file to reflect the changes:

    ```r
    renv::snapshot()
    ```

### Uninstalling Libraries

1. To uninstall a library from your `renv` environment, use the `remove` function:

    ```r
    renv::remove("package_name")
    ```

    Replace `"package_name"` with the name of the package you want to remove.

2. After removing the package, update the `renv.lock` file:

    ```r
    renv::snapshot()
    ```

### Restoring the Environment

1. If you need to restore the environment from the `renv.lock` file, use the `restore` function:

    ```r
    renv::restore()
    ```

    This will install the packages listed in the `renv.lock` file to ensure your environment matches the recorded state.

### Important Notes

- Always use `renv::snapshot()` after installing or removing packages to keep the `renv.lock` file up-to-date.
- Avoid manually editing the `renv.lock` file to prevent inconsistencies.

By following these steps, you can effectively manage your project's dependencies using `renv`.
## Modifying `Readme.rmd`

1. Open the `Readme.rmd` file located in the root directory of your project.
2. Make the necessary changes to the content. This could include updating text, adding new sections, or modifying existing ones.
3. Save the changes to `Readme.rmd`.

## Documenting Code with Roxygen2

1. Ensure that all your R code files are stored in the `R/` directory.
2. Use the Roxygen2 format to document your functions and code. Here is an example of how to document a function:

    ```r
    #' Title of the Function
    #'
    #' Description of what the function does.
    #'
    #' @param param1 Description of the first parameter.
    #' @param param2 Description of the second parameter.
    #' @return Description of the return value.
    #' @examples
    #' example_function(1, 2)
    example_function <- function(param1, param2) {
      # Function implementation
    }
    ```

3. After making changes to your code or documentation, run the following command to update the documentation:

    ```r
    devtools::document()
    ```

4. This command will generate or update the documentation files in the `man/` directory based on the Roxygen2 comments in your code.

## Building and Installing Your Package Locally

1. To build your package locally, use the following command:

    ```r
    devtools::build()
    ```

    This will create a tarball of your package in the current directory.

2. To install the built package, use the following command:

    ```r
    devtools::install()
    ```

    This will install the package from the tarball created in the previous step.

## Important Notes

- Avoid manually modifying the `NAMESPACE` and `renv.lock` files. These files are managed by Roxygen2 and `renv` respectively, and manual changes can lead to inconsistencies or errors.
- Always use the appropriate tools and commands to update these files to ensure they remain in sync with your code and environment.

## Adding Imports to the `DESCRIPTION` File

1. Open the `DESCRIPTION` file located in the root directory of your project.
2. To manually add an import, locate the `Imports:` section and add the package name. For example:

    ```
    Imports:
        dplyr,
        ggplot2
    ```

3. Alternatively, you can use the `usethis` package to add imports programmatically. Run the following command for each package you want to import:

    ```r
    usethis::use_package("package_name")
    ```

    Replace `"package_name"` with the name of the package you want to import.

4. Save the changes to the `DESCRIPTION` file.

By following these steps, you can ensure that all necessary packages are listed in the `DESCRIPTION` file, which is crucial for package dependencies.

## Summary

- Modify `Readme.rmd` as needed and save the changes.
- Document your code using Roxygen2 format in the `R/` directory.
- Run `devtools::document()` to update the documentation.
- Avoid manually modifying the `NAMESPACE` and `renv.lock` files.
- Use appropriate tools to update these files to maintain consistency.

By following these steps, you ensure that your project's documentation is always up-to-date and properly formatted.

