
#' Constructor for S3 Class 'docTbl'
#'
#' Construct a new instance of an S3 object 'docTbl'. Uses a character vector
#' to initialize a data.frame of character columns. Column names are taken
#' from 'names(cols)'. New attribute 'colstring' is set equal to 'cols'.
#'
#' @param cols named character vector
#'
#' @return S3 object of class 'docTbl'
#' @export
new_docTbl <- function(cols) {

  if (missing(cols)) {stop("`cols` is missing", call. = TRUE)}

  # initialize object
  rs <- cols2DocTbl(cols = cols)

  # update class path
  class(rs) <- c('docTbl', class(rs))

  # return object
  return(rs)

}

#' Validator for S3 Class 'docTbl'
#'
#' @param obj S3 Object
#' @param bool_out TRUE/FALSE - should output be TRUE when input object
#' is valid, or should the input object be silently returned.
#' @param throw_err TRUE/FALSE - should function stop and throw error
#' messages in case of invalid input objects, or return character vector
#' of messages as function output.
#'
#' @return R Object (see parameters)
#' @export
validate_docTbl <- function(obj, bool_out = FALSE, throw_err = TRUE) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}

  # init empty error messages
  msg_q <- character()

  # check class path
  if (!isTRUE(inherits(obj, 'docTbl'))) {
    err_msg <- "`obj` must inherit from 'docTbl'"
    msg_q <- c(err_msg, msg_q)
  }

  # check 'colstrings' attribute
  colstrings <- attr(obj, 'colstrings')
  if (isTRUE(is.null(colstrings))) {
    err_msg <- "`attr(obj, 'colstrings')` was not found"
    msg_q <- c(err_msg, msg_q)
  }

  if (!isTRUE(setequal(names(colstrings), colnames(obj)))) {
    err_msg <- "`names(attr(obj, 'colstrings'))` must equal `colnames(obj)`"
    msg_q <- c(err_msg, msg_q)
  }

  # return result
  if (isTRUE(length(msg_q) == 0)) {
    if (isTRUE(bool_out)) {return(TRUE)} else {invisible(obj)}
  } else {
    if (isTRUE(throw_err)) {
      final_msg <- paste0('\n', paste(msg_q, collapse = '\n'))
      stop(final_msg, call. = TRUE)
    } else {
      return(msg_q)
    }
  }

}

#' Constructor with Validation for S3 Class 'docTbl'
#'
#' @inheritParams new_docTbl
#' @inheritParams validate_docTbl
#'
#' @return S3 object of class 'docTbl'
#' @export
docTbl <- function(cols, bool_out = FALSE, throw_err = TRUE) {

  if (missing(cols)) {stop("`cols` is missing", call. = TRUE)}
  validate_docTbl(new_docTbl(cols = cols), bool_out = bool_out, throw_err = throw_err)

}
