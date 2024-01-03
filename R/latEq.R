#' Generate a LaTeX Equation from a Linear Model
#'
#' This function takes a linear model object and generates a LaTeX equation representation
#' based on the model's coefficients, terms, and other optional parameters.
#'
#' @param .lm A linear model object (\code{lm()}).
#' @param sub.i Logical. If \code{TRUE}, subscript ``i'' will be added to the dependent variable. Default is \code{TRUE}.
#' @param add.hat Logical. If \code{TRUE}, a hat will be added to the dependent variable. Default is \code{FALSE}.
#' @param num.coef Logical. If \code{TRUE}, numeric coefficients will be used in the equation. Default is \code{TRUE}.
#' @param round.to Numeric. The number of decimal places to round coefficients to. Default is 3.
#' @param dep.var Character. The symbol for the dependent variable. Default is ``y''.
#' @param ind.var Character. The symbol for the independent variable. Use \code{"NAME"} for variable names. Default is ``x''.
#' @param cat.var Character. The symbol for categorical variables. Default is ``\eqn{\gamma}''.
#' @param add Character vector specifying additional elements to add to the equation. Default is \code{NULL}.
#'
#' @details
#' The function supports various options for formatting the equation, including adding subscripts, hats,
#' using numeric coefficients, and including additional elements specified by the \code{add} parameter.
#'
#' The \code{.var} parameters (dep.var, ind.var, cat.var) can be set to \code{"NAME"} to use the actual variable names
#' instead of symbols in the equation. For example, setting \code{ind.var = "NAME"} will replace all independent variables with
#' the actual names of the variables in the output equation.
#'
#' The \code{add} parameter can take the following values:
#' \itemize{
#'   \item "error": Adds the error term to the equation. For example, \eqn{y = mx + b + \epsilon_i}.
#'   \item "error2": Similar to "error," but includes additional information about the error term, specifying that it follows a normal distribution. For example, \eqn{y = mx + b + \epsilon_i, \epsilon_i \sim \mathcal{N}(0, \sigma^2)}.
#'   \item "range": Appends a range condition to the equation, indicating that the equation is valid for a specific range of values of ``i''. For example, \eqn{y = mx + b, 1 \le i \le 30}.
#'   \item "range2": Similar to "range," but the range condition is based on the length of the fitted values of the linear model.
#'   \item "legend": Generates a legend for the equation, providing information about the symbols used in the equation. This is particularly useful when variable names are used instead of symbols. For example, \eqn{\code{dep.var} =  \beta_1 \cdot \code{ind.var} + \gamma \cdot \code{cat.var} + \epsilon_i}.
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
#' \code{latEq(lm(mpg ~ wt + hp, data = mtcars), sub.i = FALSE, add.hat = TRUE, num.coef = FALSE, dep.var = "y", ind.var = "NAME", cat.var = "\\beta")}
#'
#' # Example with error term and range condition
#' \code{latEq(lm(mpg ~ wt + hp, data = mtcars), add = c("error", "range"))}
#'
#' # Example with legend and custom variable names
#' \code{latEq(lm(mpg ~ wt + hp, data = mtcars), add = "legend", dep.var = "Dependent", ind.var = "Independent", cat.var = "Category")}
#'
#' @export
#' @author Zach Vig
#' @seealso
#' LaTeX Project: \url{https://www.latex-project.org/}
latEq <- function(.lm,
                  sub.i = T,
                  add.hat = F,
                  num.coef = T,
                  round.to = 3,
                  dep.var = "y",
                  ind.var = "x",
                  cat.var = "\\gamma",
                  add = NULL) {



  all.vars <- as.character(names(.lm$coef))

  terms <- data.frame(vars = all.vars,
                      coefs = unname(round(.lm$coef,round.to)),
                      is.pos = unname(.lm$coef) > 0,
                      is.intercept = isIntercept(all.vars),
                      is.interaction = isInteraction(all.vars))

  fact.vars <- gatherFactors(.lm$xlevels); incl.fact <- !is.null(fact.vars)
  num.vars <- gatherNumericals(terms, fact.vars); incl.num <- length(num.vars) > 0

  incl.int <- any(terms$is.intercept)

  yvar <- paste0(ifelse(add.hat, "\\widehat{", ""),
                 ifelse(dep.var == "NAME", paste0("\\verb|", as.character(.lm$terms[[2]]), "|"), dep.var),
                 ifelse(add.hat, "}", ""),
                 ifelse(sub.i, "_i", ""))

  sym.key <- data.frame(var = c(num.vars, fact.vars))

  if (ind.var == "NAME") {
    sym.key$sym <- c(paste0("\\cdot\\verb|", sym.key$var, ifelse(sub.i,"|_i","|")))
  } else {
    sym.key$sym <- c(prepVar(num.vars, ind.var, incl.num, sub.i),
                     prepVar(fact.vars, cat.var, incl.fact, sub.i))
  }

  latex.terms <- eqTerms(terms, sym.key, incl.int)

  if (!num.coef) {
    latex.terms$is.pos <- TRUE
    j <- as.numeric(incl.int)
    n <- nrow(latex.terms)
    latex.terms$coefs <- paste0("\\beta_{", (1-j):(n - j),"}")
  }

  eq <- paste0(yvar, " = ", ifelse(incl.int, latex.terms$coefs[1], ""))

  st <- ifelse(incl.int, 2, 1)

  for (i in st:nrow(latex.terms)) {
    sign <- ifelse(latex.terms$is.pos[i], " + ", " - ")
    coef <- latex.terms$coefs[i]
    eq.term <- paste0(sign, ifelse(num.coef, abs(coef), coef), latex.terms$sym[i])
    eq <- paste0(eq, eq.term)
  }

  if ("error" %in% add & "error2" %in% add) { stop("Must choose 'error' or 'error2', not both") }
  if ("range" %in% add & "range2" %in% add) { stop("Must choose 'range' or 'range2', not both") }
  if ("error2" %in% add & "range" %in% add) { stop("'error2' and 'range' not compatible") }
  if ("error2" %in% add & "range2" %in% add) { stop("'error2' and 'range2' not compatible") }
  if((ind.var == "NAME" | dep.var == "NAME") & "legend" %in% add) { stop("Legend can't be created if variable names are used in equation") }

  if ("error" %in% add) {
    error <- paste0(" + \\epsilon", ifelse(sub.i, "_i", ""))
    eq <- paste0(eq, error)
  }

  if ("error2" %in% add) {
    error <- paste0(" + \\epsilon", ifelse(sub.i, "_i", ""),", \\quad \\epsilon",
                    ifelse(sub.i, "_i", ""), " \\sim \\mathcal{N}(0, \\sigma^2)")
    eq <- paste0(eq, error)
  }

  if ("range" %in% add) {
    range <- ", 1 \\le i \\le n"
    eq <- paste0(eq, range)
  }

  if ("range2" %in% add) {
    nn <- length(.lm$fitted.values)
    range <- paste0(", 1 \\le i \\le ", nn)
    eq <- paste0(eq, range)
  }

  if ("legend" %in% add) {
    legend <- paste0("\n", yvar, " = ", paste0("\\verb|", as.character(.lm$terms[[2]]), "|"))
    n <- nrow(sym.key)
    for (i in 1:n) {
      newline <- paste0(", ", sym.key$sym[i]," = \\verb|", sym.key$var[i], "|")
      legend <- paste0(legend, newline)
    }
    eq <- paste0(eq, legend)
  }

  cat(eq)

}

