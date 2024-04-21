#' Helper functions for latEq function
#' @noRd
isIntercept <- function(var) {
  return(var == "(Intercept)")
}

#' @noRd
isInteraction <- function(var) {
  return(grepl("\\:|\\*", var, perl = T))
}

#' @noRd
isPoly <- function(var, type = F) {
  aa <- function(var) {grepl("poly(", var, fixed = T)}
  bb <- function(var) {grepl("\\^\\d+", var, perl = T)}
  if (!type) {
    return(aa(var)|bb(var))
  } else {
    type <- ifelse(aa(var), "Poly", ifelse(bb(var), "Caret", "Other"))
    return(type)
  }
}

#' @noRd
stripVar <- function(var) {
  var <- gsub("I\\(|\\^\\d+)|poly\\(|,\\d+)\\d+|\\)","",var)
  return(var)
}

#' @noRd
findOrder <- function (var) {
  if (!isPoly(var)) {
    return(1)
  } else {
    type <- isPoly(var, type = T)
    if (type == 'Poly') {
      order <- gsub("poly\\(.+\\)","", var, perl = T)
    } else if (type == "Caret") {
      order <- gsub("^", "", regmatches(var, regexpr("\\^\\d+", var)), fixed=T)
    }
    return(as.numeric(order))
  }
}

#' @noRd
buildPoly <- function(var, sym_key) {
  order <- findOrder(var)
  var <- stripVar(var)
  sym <- sym_key$sym[which(sym_key$var == var)]
  poly <- paste0(sym, "^", order)
  return(poly)
}

#' @noRd
symMatch <- function(var, sym_key) {
  if(isPoly(var)) {
    sym <- buildPoly(var, sym_key)
  } else {
    sym <- sym_key$sym[which(sym_key$var == stripVar(var))]
  }
  return(sym)
}

#' @noRd
gatherFactors <- function(xlevels) {
  if (length(xlevels) == 0) {
    return(NULL)
  } else {
    factors <- c()
    for (i in 1:length(xlevels)) {
      xx = paste0(names(xlevels)[i], xlevels[[i]])[-1]
      factors <- append(factors, xx)
    }
    return(factors)
  }
}

#' @noRd
gatherNumericals <- function(terms, fact_vars) {
  ii <- which(!terms$is_intercept)
  all.vars <- unique(stripVar(unlist(strsplit(terms[ii,]$vars,"\\:|\\*| \\* "))))
  num.vars <- setdiff(all.vars, fact_vars)
  return(stripVar(num.vars))
}

#' @noRd
prepVar <- function(vars, var_string, include, sub_i) {
  if (include) {
    ss <- c(paste0(var_string, "_{", ifelse(sub_i, "i", ""), 1:length(vars), "}"))
  } else {
    ss <- NULL
  }
  return(ss)
}

#' @noRd
eqTerms <- function(terms, sym_key, incl_int) {
  N <- nrow(terms)
  st <- ifelse(incl_int, 2, 1)
  terms$sym <- NA
  for (i in st:N) {
    term <- terms$vars[i]
    if (isInteraction(term)) {
      term <- strsplit(term,"\\:|\\*| \\* ")[[1]]
    }
    sym <- sapply(term, symMatch, sym_key)
    terms$sym[i] <- paste0(sym, collapse = "")
  }
  return(terms)
}
