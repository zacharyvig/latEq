# latEq: Generate LaTeX Equation from a Linear Model

The `latEq` function takes a linear model object and generates a LaTeX equation representation based on the model's coefficients, terms, and other optional parameters.

## Installation

To install the `latEq` package using `devtools`, you can run the following commands in R:

```R
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install latEq from GitHub
devtools::install_github("zacharyvig/latEq")
```
## Details

The `latEq` function is designed to simplify the process of converting linear regression models into LaTeX equation representations. This function takes a linear model object created using R's `lm()` function as input and generates a formatted LaTeX equation based on the model's coefficients, terms, and customizable parameters. Users can control various aspects of the equation presentation, such as adding subscripts, including hats on the dependent variable, using numeric coefficients, and introducing additional elements like error terms and range conditions. Additionally, the function allows users to choose between symbolic variable representation and actual variable names in the output equation. The resulting LaTeX equation is suitable for use in academic papers, reports, or presentations, providing a convenient and customizable tool for researchers and statisticians working with linear models in R.

## Examples

```R
# Example with default parameters
latEq(lm(mpg ~ wt + hp, data = mtcars))

# Example with custom parameters
latEq(lm(mpg ~ wt + hp, data = mtcars), sub.i = FALSE, add.hat = TRUE, num.coef = FALSE, dep.var = "y", ind.var = "NAME", cat.var = "\\beta")

# Example with error term and range condition
latEq(lm(mpg ~ wt + hp, data = mtcars), add = c("error", "range"))

# Example with legend and custom variable names
latEq(lm(mpg ~ wt + hp, data = mtcars), add = "legend", dep.var = "Dependent", ind.var = "Independent", cat.var = "Category")
```

## Author

[Zach Vig](https://github.com/zacharyvig)

## See Also

[LaTeX Project](https://www.latex-project.org/)
