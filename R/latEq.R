#' Generate a LaTeX Equation from a Linear Model
#'
#' This function takes a linear model object and generates a LaTeX equation representation
#' based on the model's coefficients, terms, and other optional parameters.
#'
#' @param .lm A linear model object (\code{lm()}).
#' @param sub_i Logical. If \code{TRUE}, subscript ``i'' will be added to the dependent variable. Default is \code{TRUE}.
#' @param add_hat Logical. If \code{TRUE}, a hat will be added to the dependent variable. Default is \code{FALSE}.
#' @param num_coef Logical. If \code{TRUE}, numeric coefficients will be used in the equation. Default is \code{TRUE}.
#' @param round_to Numeric. The number of decimal places to round coefficients to. Default is 3.
#' @param dep_var Character. The symbol for the dependent variable. Default is ``y''.
#' @param ind_var Character. The symbol for the independent variable. Use \code{"NAME"} for variable names. Default is ``x''.
#' @param cat_var Character. The symbol for categorical variables. Default is ``\eqn{\gamma}''.
#' @param add Character vector specifying additional elements to add to the equation. Default is \code{NULL}.
#'
#' @details
#' The function supports various options for formatting the equation, including adding subscripts, hats,
#' using numeric coefficients, and including additional elements specified by the \code{add} parameter.
#'
#' The \code{_var} parameters (dep_var, ind_var, cat_var) can be set to \code{"NAME"} to use the actual variable names
#' instead of symbols in the equation. For example, setting \code{ind_var = "NAME"} will replace all independent variables with
#' the actual names of the variables in the output equation.
#'
#' The \code{add} parameter can take the following values:
#' \itemize{
#'   \item "error": Adds the error term to the equation. For example, \eqn{y = mx + b + \epsilon_i}.
#'   \item "error2": Similar to "error," but includes additional information about the error term, specifying that it follows a normal distribution. For example, \eqn{y = mx + b + \epsilon_i, \epsilon_i \sim \mathcal{N}(0, \sigma^2)}.
#'   \item "range": Appends a range condition to the equation, indicating that the equation is valid for a range of values of ``i'' (1 to n). For example, \eqn{y = mx + b, 1 \le i \le n}.
#'   \item "range2": Similar to "range," but the range condition is based on the length of the fitted values of the linear model. For example, \eqn{y = mx + b, 1 \le i \le 30}.
#'   \item "legend": Generates a legend for the equation, providing information about the symbols used in the equation. This is particularly useful when variable names are used instead of symbols. For example, \eqn{\code{dep_var} =  \beta_1 \cdot \code{ind_var} + \gamma \cdot \code{cat_var} + \epsilon_i}.
#' }
#'
#' Note that in order for R to read LaTeX symbols, you must use a double slash, e.g., ``\\\beta''.
#'
#' @return A character string representing the LaTeX equation.
#'
#' @examples
#' \code{latEq(lm(mpg ~ wt + hp, data = mtcars))}
#'
#' # Example with custom parameters
#' \code{latEq(lm(mpg ~ wt + hp, data = mtcars), sub_i = FALSE, add_hat = TRUE, num_coef = FALSE, dep_var = "y", ind_var = "NAME", cat_var = "\\beta")}
#'
#' # Example with error term and range condition
#' \code{latEq(lm(mpg ~ wt + hp, data = mtcars), add = c("error", "range"))}
#'
#' # Example with legend and custom variable names
#' \code{latEq(lm(mpg ~ wt + hp, data = mtcars), add = "legend", dep_var = "Dependent", ind_var = "Independent", cat_var = "Category")}
#'
#' @export
#' @author Zach Vig
#' @seealso
#' LaTeX Project: \url{https://www.latex-project.org/}
latEq <- function(.lm,
                  sub_i = T,
                  add_hat = F,
                  num_coef = T,
                  round_to = 3,
                  dep_var = "y",
                  ind_var = "x",
                  cat_var = "\\gamma",
                  add = NULL) {


  all_vars <- as.character(names(.lm$coef))

  terms <- data.frame(vars = all_vars,
                      coefs = unname(round(.lm$coef,round_to)),
                      is_pos = unname(.lm$coef) > 0,
                      is_intercept = isIntercept(all_vars),
                      is_interaction = isInteraction(all_vars))

  fact_vars <- gatherFactors(.lm$xlevels); incl_fact <- !is.null(fact_vars)
  num_vars <- gatherNumericals(terms, fact_vars); incl_num <- length(num_vars) > 0

  incl_int <- any(terms$is_intercept)

  yvar <- paste0(ifelse(add_hat, "\\widehat{", ""),
                 ifelse(dep_var == "NAME", paste0("\\verb|", as.character(.lm$terms[[2]]), "|"), dep_var),
                 ifelse(add_hat, "}", ""),
                 ifelse(sub_i, "_i", ""))

  sym_key <- data.frame(var = c(num_vars, fact_vars))

  if (ind_var == "NAME") {
    sym_key$sym <- c(paste0("\\cdot\\verb|", sym_key$var, ifelse(sub_i,"|_i","|")))
  } else {
    sym_key$sym <- c(prepVar(num_vars, ind_var, incl_num, sub_i),
                     prepVar(fact_vars, cat_var, incl_fact, sub_i))
  }

  latex_terms <- eqTerms(terms, sym_key, incl_int)

  if (!num_coef) {
    latex_terms$is_pos <- TRUE
    j <- as_numeric(incl_int)
    n <- nrow(latex_terms)
    latex_terms$coefs <- paste0("\\beta_{", (1-j):(n - j),"}")
  }

  eq <- paste0(yvar, " = ", ifelse(incl_int, latex_terms$coefs[1], ""))

  st <- ifelse(incl_int, 2, 1)

  for (i in st:nrow(latex_terms)) {
    sign <- ifelse(latex_terms$is_pos[i], " + ", " - ")
    coef <- latex_terms$coefs[i]
    eq_term <- paste0(sign, ifelse(num_coef, abs(coef), coef), latex_terms$sym[i])
    eq <- paste0(eq, eq_term)
  }

  if ("error" %in% add & "error2" %in% add) { stop("Must choose 'error' or 'error2', not both") }
  if ("range" %in% add & "range2" %in% add) { stop("Must choose 'range' or 'range2', not both") }
  if ("error2" %in% add & "range" %in% add) { stop("'error2' and 'range' not compatible") }
  if ("error2" %in% add & "range2" %in% add) { stop("'error2' and 'range2' not compatible") }
  if((ind_var == "NAME" | dep_var == "NAME") & "legend" %in% add) { stop("Legend can't be created if variable names are used in equation") }

  if ("error" %in% add) {
    error <- paste0(" + \\epsilon", ifelse(sub_i, "_i", ""))
    eq <- paste0(eq, error)
  }

  if ("error2" %in% add) {
    error <- paste0(" + \\epsilon", ifelse(sub_i, "_i", ""),", \\quad \\epsilon",
                    ifelse(sub_i, "_i", ""), " \\sim \\mathcal{N}(0, \\sigma^2)")
    eq <- paste0(eq, error)
  }

  if ("range" %in% add) {
    range <- ", 1 \\le i \\le n"
    eq <- paste0(eq, range)
  }

  if ("range2" %in% add) {
    nn <- length(.lm$fitted_values)
    range <- paste0(", 1 \\le i \\le ", nn)
    eq <- paste0(eq, range)
  }

  if ("legend" %in% add) {
    legend <- paste0("\n", yvar, " = ", paste0("\\verb|", as.character(.lm$terms[[2]]), "|"))
    n <- nrow(sym_key)
    for (i in 1:n) {
      newline <- paste0(", ", sym_key$sym[i]," = \\verb|", sym_key$var[i], "|")
      legend <- paste0(legend, newline)
    }
    eq <- paste0(eq, legend)
  }

  return(eq)

}

