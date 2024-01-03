# latEq: Generate LaTeX Equation from a Linear Model

The `latEq` function takes a linear model object and generates a LaTeX equation representation based on the model's coefficients, terms, and other optional parameters.

## Usage

```R
latEq <- function(.lm,
                  sub.i = T,
                  add.hat = F,
                  num.coef = T,
                  round.to = 3,
                  dep.var = "y",
                  ind.var = "x",
                  cat.var = "\\gamma",
                  add = NULL) {
  # Function implementation...
}
```

## Parameters

# .lm: A linear model object (lm()).
# sub.i: Logical. If TRUE, subscript "i" will be added to the dependent variable. Default is TRUE.
# add.hat: Logical. If TRUE, a hat will be added to the dependent variable. Default is FALSE.
# num.coef: Logical. If TRUE, numeric coefficients will be used in the equation. Default is TRUE.
# round.to: Numeric. The number of decimal places to round coefficients to. Default is 3.
# dep.var: Character. The symbol for the dependent variable. Default is "y".
# ind.var: Character. The symbol for the independent variable. Use "NAME" for variable names. Default is "x".
# cat.var: Character. The symbol for categorical variables. Default is "\(\gamma\)".
# add: Character vector specifying additional elements to add to the equation. Default is NULL.

## Details

# The function supports various options for formatting the equation, including adding subscripts, hats,
# using numeric coefficients, and including additional elements specified by the add parameter.
# The .var parameters (dep.var, ind.var, cat.var) can be set to "NAME" to use the actual variable names
# instead of symbols in the equation.
# The add parameter can take values such as:
# - "error": Adds the error term to the equation. For example, \(y = mx + b + \epsilon_i\).
# - "error2": Similar to "error," but includes additional information about the error term, specifying that it follows a normal distribution. For example, \(y = mx + b + \epsilon_i, \epsilon_i \sim \mathcal{N}(0, \sigma^2)\).
# - "range": Appends a range condition to the equation, indicating that the equation is valid for a specific range of values of "i". For example, \(y = mx + b, 1 \le i \le 30\).
# - "range2": Similar to "range," but the range condition is based on the length of the fitted values of the linear model. For example, \(y = mx + b, 1 \le i \le nn\).
# - "legend": Generates a legend for the equation, providing information about the symbols used in the equation. This is particularly useful when variable names are used instead of symbols. For example, \(y = \code{dep.var} + \beta_1 \cdot \code{ind.var} + \beta_2 \cdot \gamma + \epsilon_i\).
# Note that in order for R to read LaTeX symbols, you must use a double slash, e.g., \\beta.

## Examples

# Example with default parameters
```R
latEq(lm(mpg ~ wt + hp, data = mtcars))
```

# Example with custom parameters
```R
latEq(lm(mpg ~ wt + hp, data = mtcars), sub.i = FALSE, add.hat = TRUE, num.coef = FALSE, dep.var = "y", ind.var = "NAME", cat.var = "\\beta")
```

# Example with error term and range condition
```R
latEq(lm(mpg ~ wt + hp, data = mtcars), add = c("error", "range"))
```

# Example with legend and custom variable names
```R
latEq(lm(mpg ~ wt + hp, data = mtcars), add = "legend", dep.var = "Dependent", ind.var = "Independent", cat.var = "Category")
```

## Author

[Zach Vig](https://github.com/zacharyvig)

## See Also

[LaTeX Project](https://www.latex-project.org/)
