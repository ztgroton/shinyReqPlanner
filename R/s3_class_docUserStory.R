
#' Constructor for S3 Class 'docUserStory'
#'
#' Construct a new instance of an S3 object 'docUserStory'.
#' Inherits functionality from S3 Class 'docTbl'.
#'
#' @return S3 object of class 'docUserStory'
#' @export
new_docUserStory <- function() {

  # cosntruct 'cols'
  cols <- getDocTemplates('user_story', 'colstrings')
  names(cols) <- getDocTemplates('user_story', 'name')

  # initialize object
  rs <- docTbl(cols = cols)

  # update class path
  class(rs) <- c('docUserStory', class(rs))

  # return object
  return(rs)

}

#' Validator for S3 Class 'docUserStory'
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
validate_docUserStory <- function(obj, bool_out = FALSE, throw_err = TRUE) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}

  # apply parent class validator
  obj <- validate_docTbl(obj = obj)

  # init empty error messages
  msg_q <- character()

  # check class path
  if (!isTRUE(inherits(obj, 'docUserStory'))) {
    err_msg <- "`obj` must inherit from 'docUserStory'"
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

  if (!isTRUE(setequal(names(colstrings), getDocTemplates('user_story', 'name')))) {
    err_msg <- "`names(attr(obj, 'colstrings'))` must match `docTemplates`"
    msg_q <- c(err_msg, msg_q)
  }

  if (!isTRUE(setequal(colstrings, getDocTemplates('user_story', 'colstrings')))) {
    err_msg <- "`attr(obj, 'colstrings')` must match `docTemplates`"
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

#' Constructor with Validation for S3 Class 'docUserStory'
#'
#' @inheritParams validate_docUserStory
#'
#' @return S3 object of class 'docUserStory'
#' @export
docUserStory <- function(bool_out = FALSE, throw_err = TRUE) {

  validate_docUserStory(new_docUserStory(), bool_out = bool_out, throw_err = throw_err)

}
