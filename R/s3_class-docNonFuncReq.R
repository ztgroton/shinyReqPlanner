#' Constructor for S3 Class 'docNonFuncReq'
#'
#' Construct a new instance of an S3 object 'docNonFuncReq'.
#' Inherits functionality from S3 Class 'docTbl'.
#'
#' @return S3 object of class 'docNonFuncReq'
#' @export
new_docNonFuncReq <- function() {

  # cosntruct 'cols'
  cols <- getDocTemplates('non_func_req', 'colstrings')
  names(cols) <- getDocTemplates('non_func_req', 'name')

  # initialize object
  rs <- docTbl(cols = cols)

  # update class path
  class(rs) <- c('docNonFuncReq', class(rs))

  # return object
  return(rs)

}

#' Validator for S3 Class 'docNonFuncReq'
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
validate_docNonFuncReq <- function(obj, bool_out = FALSE, throw_err = TRUE) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}

  # apply parent class validator
  obj <- validate_docTbl(obj = obj)

  # init empty error messages
  msg_q <- character()

  # check class path
  if (!isTRUE(inherits(obj, 'docNonFuncReq'))) {
    err_msg <- "`obj` must inherit from 'docNonFuncReq'"
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

  if (!isTRUE(setequal(names(colstrings), getDocTemplates('non_func_req', 'name')))) {
    err_msg <- "`names(attr(obj, 'colstrings'))` must match `docTemplates`"
    msg_q <- c(err_msg, msg_q)
  }

  if (!isTRUE(setequal(colstrings, getDocTemplates('non_func_req', 'colstrings')))) {
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

#' Constructor with Validation for S3 Class 'docNonFuncReq'
#'
#' @inheritParams validate_docNonFuncReq
#'
#' @return S3 object of class 'docNonFuncReq'
#' @export
docNonFuncReq <- function(bool_out = FALSE, throw_err = TRUE) {

  validate_docNonFuncReq(new_docNonFuncReq(), bool_out = bool_out, throw_err = throw_err)

}
