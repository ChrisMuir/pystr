#' String lookups
#' 
#' Perform lookups at the character vector and substring level.
#'
#' @param x A character vector.
#' @param table A character vector, list, or environment.
#'
#' @return A logical vector with length 1.
#' 
#' @references \url{https://docs.python.org/3.7/reference/expressions.html#in}
#'
#' @examples
#' # Is string in string (substring lookup).
#' "cats" %pystr_in% "I like cats quite a bit"
#' 
#' # Is string in char vector.
#' "cats" %pystr_in% c("dogs", "cats", "ducks")
#' 
#' # Is string in environment keys.
#' table <- new.env()
#' table$cats <- "blah1"
#' table$dogs <- "blah2"
#' "cats" %pystr_in% table # TRUE
#' "blah1" %pystr_in% table # FALSE
#' 
#' # string in list.
#' "cats" %pystr_in% list("dogs", "cats", "ducks")
#' 
#' # char vector in list.
#' c("yay", "cats") %pystr_in% list(c(1, 2, 3), c("yay", "cats"), c("dogs", "are", "okay")) # TRUE
#' c("yay", "frogs") %pystr_in% list(c(1, 2, 3), c("yay", "cats"), c("dogs", "are", "okay")) # FALSE
#' 
#' @export
`%pystr_in%` <- function(x, table) {
  UseMethod("%pystr_in%", table)
}

# Character vector method
`%pystr_in%.default` <- function(x, table) {
  stopifnot(is.character(x))
  if (!is.character(table)) {
    stop("arg 'table' must be a character vector, list, or environment")
  }
  
  # If x is missing or has length greater than 1, or table is missing, 
  # return FALSE.
  if (is_missing(x) || length(x) > 1 || is_missing(table)) return(FALSE)
  
  if (length(table) > 1) {
    return(pystr_in_(x, table)) # string in char vect
  } else {
    return(grepl(x, table, fixed = TRUE)) # string in string
  }
}

# List method
`%pystr_in%.list` <- function(x, table) {
  # If x is missing or table has length 0, return FALSE.
  if (is_missing(x) || length(table) < 1) return(FALSE)
  
  # If x has length 1, use `==`. Else if x has length 
  # greater than 1, look for identical vector match within table.
  if (length(x) == 1 ) {
    return(any(table == x)) # string in list
  } else {
    return(pystr_in_(x, table)) # char vect in list
  }
}

# Environment method
`%pystr_in%.environment` <- function(x, table) {
  # If x is missing or table has length 0, return FALSE.
  if (is_missing(x) || length(table) < 1) return(FALSE)
  
  return(exists(x, envir = table, inherits = FALSE)) # string in env keys
}

is_missing <- function(x) {
  if (is.na(x) || is.null(x) || length(x) == 0) {
    return(TRUE)
  }
  FALSE
}
