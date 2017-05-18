#' Simplify an expression by computing the values for constant expressions
#'
#' @param expr An expression
#' @return a simplified expression
#' @export
simplify_expr <- function(expr) {
  if (is.atomic(expr) || is.name(expr)) {
    expr

  } else if (is.call(expr)) {
    simplify_call(expr)

  } else {
    stop(paste0("Unexpected expression ", deparse(expr), " in simplifying")) # nocov
  }
}

simplify_addition <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (left == 0) return(right)
  if (right == 0) return(left)
  if (is.numeric(left) && is.numeric(right)) return(left + right)
  call("+", left, right)
}

simplify_unary_subtraction <- function(f) {
   simplified <- simplify_expr(f)
   if (is.numeric(simplified)) -simplified
   else if (is.call(simplified) && simplified[[1]] == "-") simplified[[2]]
   else bquote(-.(simplified))
}

simplify_subtraction <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (left == 0) {
    if (is.numeric(right)) return(-right)
    else return(bquote(-.(right)))
  }
  if (right == 0) return(left)
  if (is.numeric(left) && is.numeric(right)) return(left - right)
  call("-", left, right)
}

simplify_multiplication <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (left == 0 || right == 0) return(0)
  if (left == 1) return(right)
  if (right == 1) return(left)
  if (is.numeric(left) && is.numeric(right)) return(left * right)
  call("*", left, right)
}

simplify_division <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (right == 1) return(left)
  if (is.numeric(left) && is.numeric(right)) return(left / right)
  call("/", left, right)
}

simplify_exponentiation <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (right == 0) return(1)
  if (left == 0) return(0)
  if (left == 1) return(1)
  if (right == 1) return(left)
  if (is.numeric(left) && is.numeric(right)) return(left ^ right)
  call("^", left, right)
}

simplify_function_call <- function(expr) {
  function_name <- expr[[1]]
  arguments <- vector("list", length(expr) - 1)
  for (i in seq_along(arguments)) {
    arguments[i] <- list(simplify_expr(expr[[i + 1]]))
  }

  # if we have simplified all expressions we might as well try calling the function
  # if it is a function we know...
  if (all(unlist(Map(is.numeric, arguments)))) {
    if (is.name(function_name) &&
        as.character(function_name) %in% c("sin", "cos", "exp", "log")) {
      result <- do.call(as.character(function_name), arguments)
      names(result) <- names(expr)
      return(result)
    }
  }
  result <- as.call(c(list(function_name), arguments))
  names(result) <- names(expr)
  result
}

simplify_call <- function(expr) {
  if (is.name(expr[[1]])) {
    if (expr[[1]] == as.name("+")) return(simplify_addition(expr[[2]], expr[[3]]))
    if (expr[[1]] == as.name("-")) {
      if (length(expr) == 2) return(simplify_unary_subtraction(expr[[2]]))
      else return(simplify_subtraction(expr[[2]], expr[[3]]))
    }

    if (expr[[1]] == as.name("*")) return(simplify_multiplication(expr[[2]], expr[[3]]))
    if (expr[[1]] == as.name("/")) return(simplify_division(expr[[2]], expr[[3]]))

    if (expr[[1]] == as.name("^")) return(simplify_exponentiation(expr[[2]], expr[[3]]))

    if (expr[[1]] == as.name("(")) {
      subexpr <- simplify_expr(expr[[2]])
      if (is.atomic(subexpr) || is.name(subexpr)) return(subexpr)
      else if (is.call(subexpr) && subexpr[[1]] == as.name("(")) return(subexpr)
      else return(call("(", subexpr))
    }
  }

  simplify_function_call(expr)
}
