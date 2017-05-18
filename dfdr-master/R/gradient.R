#' Compute the gradient-function of a function.
#'
#' Creates a function that computes the derivative of a function with respect to each parameter
#' and return a vector of these.
#'
#' @param f  A function
#' @param vars The variables to compute the derivatives of. If NULL
#'             the gradient will be computed for all formal arguments
#'             of f.
#' @param use_names Should the gradient add variable names to the output of the function?
#' @return  A function that computes the gradient of f at any point.
#' @export
gradient <- function(f, vars = NULL, use_names = FALSE) {
  if (is.null(vars)) {
    vars <- names(formals(f))
  }
  derivatives <- Map(function(v) d(f, v), vars)
  function(...) {
    unlist(Map(function(df) df(...), derivatives), use.names = use_names)
  }
}

#' Compute the Hessian-function of a function.
#'
#' Creates a function that computes the second-order derivatives of a function with respect to each pair of parameters
#' and return a vector of these.
#'
#' @param f  A function
#' @param vars The variables to compute the derivatives of. If NULL
#'             the gradient will be computed for all formal arguments
#'             of f.
#' @param use_names Should the gradient add variable names to the output of the function?
#' @return  A function that computes the gradient of f at any point.
#' @export
hessian <- function(f, vars = NULL, use_names = FALSE) {
  if (is.null(vars)) {
    vars <- names(formals(f))
  }
  first_derivatives <- Map(function(v) d(f, v), vars)
  second_derivatives <- first_derivatives # just an easy hack to get the right length with right names
  for (var in vars) {
    df <- first_derivatives[[var]]
    second_derivatives[[var]] <- Map(function(v) d(df, v), vars)
  }
  function(...) {
    H <- matrix(nrow = length(vars), ncol = length(vars))
    if (use_names) rownames(H) <- colnames(H) <- vars
    for (i in seq_along(vars)) {
      v1 <- vars[i]
      for (j in seq_along(vars)) {
        v2 <- vars[j]
        df <- second_derivatives[[v1]][[v2]]
        H[i, j] <- df(...)
      }
    }
    H
  }
}

