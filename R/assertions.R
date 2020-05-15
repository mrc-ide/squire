
#### HELPER FUNCTIONS ####################################################################

#------------------------------------------------
# for single value, return value as string. For vector of values return string
# of comma-separated values enclosed in curly brackets
#' @noRd
nice_format <- function(x) {
  if (is.null(x)) {
    return("")
  }
  if (length(x)==1) {
    ret <- as.character(x)
  } else {
    ret <- paste0("{", paste(x, collapse = ", "), "}")
  }
  return(ret)
}

#### BASIC OBJECT TYPES ####################################################################

#------------------------------------------------
# x is NULL
#' @noRd
assert_null <- function(x, message = "%s must be null", name = deparse(substitute(x))) {
  if (!is.null(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is not NULL
#' @noRd
assert_non_null <- function(x, message = "%s cannot be null", name = deparse(substitute(x))) {
  if (is.null(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is atomic
#' @noRd
assert_atomic <- function(x, message = "%s must be atomic (see ?is.atomic)", name = deparse(substitute(x))) {
  if (!is.atomic(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is atomic and single valued (has length 1)
#' @noRd
assert_single <- function(x, message = "%s must be a single value", name = deparse(substitute(x))) {
  assert_non_null(x, name = name)
  assert_atomic(x, name = name)
  assert_length(x, 1, name = name)
  return(TRUE)
}

#------------------------------------------------
# x is character string
#' @noRd
assert_string <- function(x, message = "%s must be character string", name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is single character string
#' @noRd
assert_single_string <- function(x, name = deparse(substitute(x))) {
  assert_length(x, n = 1, name = name)
  assert_string(x, name = name)
  return(TRUE)
}

#------------------------------------------------
# x is logical
#' @noRd
assert_logical <- function(x, message = "%s must be logical", name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is single logical
#' @noRd
assert_single_logical <- function(x, name = deparse(substitute(x))) {
  assert_length(x, n = 1, name = name)
  assert_logical(x, name = name)
  return(TRUE)
}

#------------------------------------------------
# x is numeric
#' @noRd
assert_numeric <- function(x, message = "%s must be numeric", name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is single numeric
#' @noRd
assert_single_numeric <- function(x, name = deparse(substitute(x))) {
  assert_length(x, n = 1, name = name)
  assert_numeric(x, name = name)
  return(TRUE)
}

#------------------------------------------------
# x is integer
#' @noRd
assert_int <- function(x, message = "%s must be integer valued", name = deparse(substitute(x))) {
  assert_numeric(x, name = name)
  if (!isTRUE(all.equal(x, as.integer(x), check.attributes = FALSE))) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is single integer
#' @noRd
assert_single_int <- function(x, name = deparse(substitute(x))) {
  assert_length(x, n = 1, name = name)
  assert_int(x, name = name)
  return(TRUE)
}


#------------------------------------------------
# x is positive (with or without zero allowed)
#' @noRd
assert_neg <- function(x, zero_allowed = TRUE, message1 = "%s must be less than or equal to zero", message2 = "%s must be greater than zero", name = deparse(substitute(x))) {
  assert_numeric(x, name = name)
  if (zero_allowed) {
    if (!all(x<=0)) {
      stop(sprintf(message1, name), call. = FALSE)
    }
  } else {
    if (!all(x<0)) {
      stop(sprintf(message2, name), call. = FALSE)
    }
  }
  return(TRUE)
}

#------------------------------------------------
# x is positive (with or without zero allowed)
#' @noRd
assert_pos <- function(x, zero_allowed = TRUE, message1 = "%s must be greater than or equal to zero", message2 = "%s must be greater than zero", name = deparse(substitute(x))) {
  assert_numeric(x, name = name)
  if (zero_allowed) {
    if (!all(x>=0)) {
      stop(sprintf(message1, name), call. = FALSE)
    }
  } else {
    if (!all(x>0)) {
      stop(sprintf(message2, name), call. = FALSE)
    }
  }
  return(TRUE)
}

#------------------------------------------------
# x is single positive (with or without zero allowed)
#' @noRd
assert_single_pos <- function(x, zero_allowed = TRUE, name = deparse(substitute(x))) {
  assert_length(x, n = 1, name = name)
  assert_pos(x, zero_allowed = zero_allowed, name = name)
  return(TRUE)
}

#------------------------------------------------
# x is positive integer (with or without zero allowed)
#' @noRd
assert_pos_int <- function(x, zero_allowed = TRUE, name = deparse(substitute(x))) {
  assert_int(x, name = name)
  assert_pos(x, zero_allowed = zero_allowed, name = name)
  return(TRUE)
}

#------------------------------------------------
# x is single positive integer (with or without zero allowed)
#' @noRd
assert_single_pos_int <- function(x, zero_allowed = TRUE, name = deparse(substitute(x))) {
  assert_length(x, n = 1, name = name)
  assert_pos_int(x, zero_allowed = zero_allowed, name = name)
  return(TRUE)
}

#------------------------------------------------
# x is single value bounded between limits
#' @noRd
assert_single_bounded <- function(x, left = 0, right = 1, inclusive_left = TRUE, inclusive_right = TRUE, name = deparse(substitute(x))) {
  assert_length(x, n = 1, name = name)
  assert_bounded(x, left = left, right = right, inclusive_left = inclusive_left, inclusive_right = inclusive_right, name = name)
  return(TRUE)
}

#------------------------------------------------
# x is a vector (and is not a list or another recursive type)
#' @noRd
assert_vector <- function(x, message = "%s must be a non-recursive vector", name = deparse(substitute(x))) {
  if (!is.vector(x) || is.recursive(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is a matrix
#' @noRd
assert_matrix <- function(x, message = "%s must be a matrix", name = deparse(substitute(x))) {
  if (!is.matrix(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is a list
assert_list <- function(x, message = "%s must be a list", name = deparse(substitute(x))) {
  if (!is.list(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is a data frame
assert_dataframe <- function(x, message = "%s must be a data frame", name = deparse(substitute(x))) {
  if (!is.data.frame(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is a date
assert_date <- function(x,
                        message = "%s must be a date or ISO-formatted string",
                        name = deparse(substitute(x))) {

  if (all(class(x) == "Date")) {
    return(TRUE)
  } else {
    if (is.character(x)) {
      if(all(class(as.Date(x)) == "Date")) {
        return(TRUE)
      }
    } else {
      stop(sprintf(message, name), call. = FALSE)
    }
  }

}


#------------------------------------------------
# x inherits from custom class c
assert_custom_class <- function(x, c, message = "%s must inherit from class '%s'", name = deparse(substitute(x))) {
  if (!inherits(x, c)) {
    stop(sprintf(message, name, c), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is a plotting limit, i.e. contains two increasing values
assert_limit <- function(x, message = "%s must be a valid plotting limit, i.e. contain two increasing values", name = deparse(substitute(x))) {
  assert_vector(x, name = name)
  assert_length(x, 2, name = name)
  assert_numeric(x, name = name)
  assert_increasing(x, name = name)
  return(TRUE)
}


#### VALUE COMPARISONS ####################################################################

#------------------------------------------------
# x and y are equal in all matched comparisons. x and y can be any type
#' @noRd
assert_eq <- function(x, y, message = "%s must equal %s",
                      name_x = deparse(substitute(x)), name_y = nice_format(y)) {
  assert_non_null(x, name = name_x)
  assert_non_null(y, name = name_y)
  assert_same_length(x, y, name_x = name_x, name_y = name_y)
  if (!isTRUE(all.equal(x, y, check.attributes = FALSE))) {
    stop(sprintf(message, name_x, name_y), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x and y are unequal in all matched comparisons. x and y can be any type
#' @noRd
assert_neq <- function(x, y, message = "%s cannot equal %s",
                       name_x = deparse(substitute(x)), name_y = nice_format(y)) {
  assert_non_null(x, name = name_x)
  assert_non_null(y, name = name_y)
  assert_same_length(x, y, name_x = name_x, name_y = name_y)
  if (any(x == y)) {
    stop(sprintf(message, name_x, name_y), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is greater than y in all matched comparisons
#' @noRd
assert_gr <- function(x, y, message = "%s must be greater than %s",
                      name_x = deparse(substitute(x)), name_y = nice_format(y)) {
  assert_numeric(x, name = name_x)
  assert_numeric(y, name = name_y)
  assert_in(length(y), c(1,length(x)))
  if (!all(x>y)) {
    stop(sprintf(message, name_x, name_y), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is greater than or equal to y in all matched comparisons
#' @noRd
assert_greq <- function(x, y, message = "%s must be greater than or equal to %s",
                        name_x = deparse(substitute(x)), name_y = nice_format(y)) {
  assert_numeric(x, name = name_x)
  assert_numeric(y, name = name_y)
  assert_in(length(y), c(1,length(x)))
  if (!all(x>=y)) {
    stop(sprintf(message, name_x, name_y), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is less than y in all matched comparisons
#' @noRd
assert_le <- function(x, y, message = "%s must be less than %s",
                      name_x = deparse(substitute(x)), name_y = nice_format(y)) {
  assert_numeric(x, name = name_x)
  assert_numeric(y, name = name_y)
  assert_in(length(y), c(1,length(x)))
  if (!all(x<y)) {
    stop(sprintf(message, name_x, name_y), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is less than or equal to y in all matched comparisons
#' @noRd
assert_leq <- function(x, y, message = "%s must be less than or equal to %s",
                       name_x = deparse(substitute(x)), name_y = nice_format(y)) {
  assert_numeric(x, name = name_x)
  assert_numeric(y, name = name_y)
  assert_in(length(y), c(1,length(x)))
  if (!all(x<=y)) {
    stop(sprintf(message, name_x, name_y), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is between bounds (inclusive or exclusive)
#' @noRd
assert_bounded <- function(x, left = 0, right = 1, inclusive_left = TRUE, inclusive_right = TRUE, name = deparse(substitute(x))) {
  assert_numeric(x, name = name)
  if (inclusive_left) {
    if (!all(x>=left)) {
      stop(sprintf("%s must be greater than or equal to %s", name, left), call. = FALSE)
    }
  } else {
    if (!all(x>left)) {
      stop(sprintf("%s must be greater than %s", name, left), call. = FALSE)
    }
  }
  if (inclusive_right) {
    if (!all(x<=right)) {
      stop(sprintf("%s must be less than or equal to %s", name, right), call. = FALSE)
    }
  } else {
    if (!all(x<right)) {
      stop(sprintf("%s must be less than %s", name, right), call. = FALSE)
    }
  }
  return(TRUE)
}

#------------------------------------------------
# all x are in y
#' @noRd
assert_in <- function(x, y, message = "all %s must be in %s",
                      name_x = deparse(substitute(x)), name_y = nice_format(y)) {
  assert_non_null(x, name = name_x)
  assert_non_null(y, name = name_y)
  if (!all(x %in% y)) {
    stop(sprintf(message, name_x, name_y), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# none of x are in y
#' @noRd
assert_not_in <- function(x, y, message = "none of %s can be in %s",
                          name_x = deparse(substitute(x)), name_y = nice_format(y)) {
  assert_non_null(x, name = name_x)
  assert_non_null(y, name = name_y)
  if (any(x %in% y)) {
    stop(sprintf(message, name_x, name_y), call. = FALSE)
  }
  return(TRUE)
}


#### DIMENSIONS ####################################################################

#------------------------------------------------
# length(x) equals n
#' @noRd
assert_length <- function(x, n, message = "%s must be of length %s", name = deparse(substitute(x))) {
  assert_pos_int(n)
  if (length(x) != n[1]) {
    stop(sprintf(message, name, n), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x and y are same length
#' @noRd
assert_same_length <- function(x, y, message =  "%s and %s must be the same length",
                               name_x = deparse(substitute(x)), name_y = deparse(substitute(y))) {
  if (length(x) != length(y)) {
    stop(sprintf(message, name_x, name_y), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# multiple objects all same length
#' @noRd
assert_same_length_multiple <- function(...) {
  l <- mapply(length, list(...))
  if (length(unique(l)) != 1) {
    l_names <- sapply(match.call(expand.dots = FALSE)$..., deparse)
    stop(sprintf("variables %s must be the same length", nice_format(l_names)), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is two-dimensional
#' @noRd
assert_2d <- function(x, message = "%s must be two-dimensional", name = deparse(substitute(x))) {
  is_2d <- FALSE
  if (!is.null(dim(x))) {
    if (length(dim(x)) == 2) {
      is_2d <- TRUE
    }
  }
  if (!is_2d) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# nrow(x) equals n
#' @noRd
assert_nrow <- function(x, n, message = "%s must have %s rows", name = deparse(substitute(x))) {
  assert_2d(x, name = name)
  if (nrow(x) != n) {
    stop(sprintf(message, name, n), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# ncol(x) equals n
#' @noRd
assert_ncol <- function(x, n, message = "%s must have %s cols", name = deparse(substitute(x))) {
  assert_2d(x, name = name)
  if (ncol(x) != n) {
    stop(sprintf(message, name, n), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# dim(x) equals y
#' @noRd
assert_dim <- function(x, y, message = "%s must have %s rows and %s columns", name = deparse(substitute(x))) {
  assert_2d(x, name = name)
  assert_pos_int(y, name = "y variable in assert_dim()")
  assert_length(y, 2, name = "y variable in assert_dim()")
  if (nrow(x) != y[1] | ncol(x) != y[2]) {
    stop(sprintf(message, name, y[1], y[2]), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is square matrix
#' @noRd
assert_square_matrix <- function(x, message = "%s must be a square matrix", name = deparse(substitute(x))) {
  assert_matrix(x, name = name)
  if (nrow(x) != ncol(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}


#### MISC ####################################################################

#------------------------------------------------
# is symmetric matrix
#' @noRd
assert_symmetric_matrix <- function(x, message = "%s must be a symmetric matrix", name = deparse(substitute(x))) {
  assert_square_matrix(x, name = name)
  if (!isSymmetric(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x contains no duplicates
#' @noRd
assert_noduplicates <- function(x, message = "%s must contain no duplicates", name = deparse(substitute(x))) {
  if (any(duplicated(x))) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# file exists at chosen path
#' @noRd
assert_file_exists <- function(x, message = "file not found at path %s", name = deparse(substitute(x))) {
  if (!file.exists(x)) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is increasing
#' @noRd
assert_increasing <- function(x, message = "%s must be increasing", name = deparse(substitute(x))) {
  assert_non_null(x, name = name)
  assert_numeric(x, name = name)
  if (!identical(x, sort(x))) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}

#------------------------------------------------
# x is decreasing
#' @noRd
assert_decreasing <- function(x, message = "%s must be decreasing", name = deparse(substitute(x))) {
  assert_non_null(x, name = name)
  assert_numeric(x, name = name)
  if (!identical(x, sort(x, decreasing = TRUE))) {
    stop(sprintf(message, name), call. = FALSE)
  }
  return(TRUE)
}
