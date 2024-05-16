
# docTbl ----

#' Constructor for S3 Class 'docTbl'
#'
#' Construct a new instance of an S3 object 'docTbl'. Uses a character vector
#' to initialize a data.frame of character columns. Column names are taken
#' from 'names(cols)'. New attribute 'colstring' is set equal to 'cols'.
#'
#' @param data data.frame
#'
#' @return S3 object of class 'docTbl'
#' @export
new_docTbl <- function(data) {

  if (missing(data)) {stop("`data` is missing", call. = TRUE)}
  rs <- as.data.frame(data)

  # update class path
  class(rs) <- c('docTbl', class(rs))

  # return object
  return(rs)

}

#' Validator for S3 Class 'docTbl'
#'
#' @inheritParams obj_msg_result
#'
#' @return R Object (see parameters)
#' @export
validate_docTbl <- function(obj, bool_out = FALSE, throw_err = TRUE) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}

  # init empty error messages
  msg <- character()

  # check if obj inherits from 'data.frame'
  if (!isTRUE(is.data.frame(obj))) {
    msg[length(msg)+1] <- "`obj` must be valid data.frame"
  }

  # check class path
  if (!isTRUE(inherits(obj, 'docTbl'))) {
    msg[length(msg)+1] <- "`obj` must inherit from 'docTbl'"
  }

  # check 'colstrings' attribute
  colstrings <- attr(obj, 'colstrings')
  if (isTRUE(is.null(colstrings))) {
    msg[length(msg)+1] <- "`attr(obj, 'colstrings')` was not found"
  }

  if (!isTRUE(setequal(names(colstrings), colnames(obj)))) {
    msg[length(msg)+1] <- "`names(attr(obj, 'colstrings'))` must equal `colnames(obj)`"
  }

  # return result
  obj_msg_result(obj, msg, bool_out, throw_err)

}

#' Constructor with Validation for S3 Class 'docTbl'
#'
#' @inheritParams new_docTbl
#' @inheritParams validate_docTbl
#'
#' @return S3 object of class 'docTbl'
#' @export
docTbl <- function(data, bool_out = FALSE, throw_err = TRUE) {

  if (missing(data)) {stop("`data` is missing", call. = TRUE)}
  validate_docTbl(new_docTbl(data = data), bool_out = bool_out, throw_err = throw_err)

}

# docUserStory ----

#' Constructor for S3 Class 'docUserStory'
#'
#' Construct a new instance of an S3 object 'docUserStory'.
#' Inherits functionality from S3 Class 'docTbl'.
#'
#' @inheritParams new_docTbl
#'
#' @return S3 object of class 'docUserStory'
#' @export
new_docUserStory <- function(data = NULL) {

  if (is.null(data)) {

    t_name <- getDocTemplate('user_story', 'name')
    t_colstrings <- getDocTemplate('user_story', 'colstrings')
    names(t_colstrings) <- t_name

    data <- emptyDocTemplate(cols = t_colstrings)

  }

  rs <- docTbl(data = data)

  # update class path
  class(rs) <- c('docUserStory', class(rs))

  # return object
  return(rs)

}

#' Validator for S3 Class 'docUserStory'
#'
#' @inheritParams obj_msg_result
#'
#' @return R Object (see parameters)
#' @export
validate_docUserStory <- function(obj, bool_out = FALSE, throw_err = TRUE) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}

  # apply parent class validator
  obj <- validate_docTbl(obj = obj)

  # init empty error messages
  msg <- character()

  # check class path
  if (!isTRUE(inherits(obj, 'docUserStory'))) {
    msg[length(msg)+1] <- "`obj` must inherit from 'docUserStory'"
  }

  # check 'colstrings' attribute
  colstrings <- attr(obj, 'colstrings')
  if (!isTRUE(setequal(names(colstrings), getDocTemplate('user_story', 'name')))) {
    msg[length(msg)+1] <- "`names(attr(obj, 'colstrings'))` must match `docTemplates`"
  }

  if (!isTRUE(setequal(colstrings, getDocTemplate('user_story', 'colstrings')))) {
    msg[length(msg)+1] <- "`attr(obj, 'colstrings')` must match `docTemplates`"
  }

  # return result
  obj_msg_result(obj, msg, bool_out, throw_err)

}

#' Constructor with Validation for S3 Class 'docUserStory'
#'
#' @inheritParams new_docTbl
#' @inheritParams validate_docUserStory
#'
#' @return S3 object of class 'docUserStory'
#' @export
docUserStory <- function(data = NULL, bool_out = FALSE, throw_err = TRUE) {

  validate_docUserStory(new_docUserStory(data = data), bool_out = bool_out, throw_err = throw_err)

}

# docFuncReq ----

#' Constructor for S3 Class 'docFuncReq'
#'
#' Construct a new instance of an S3 object 'docFuncReq'.
#' Inherits functionality from S3 Class 'docTbl'.
#'
#' @inheritParams new_docTbl
#'
#' @return S3 object of class 'docFuncReq'
#' @export
new_docFuncReq <- function(data = NULL) {

  if (is.null(data)) {

    t_name <- getDocTemplate('func_req', 'name')
    t_colstrings <- getDocTemplate('func_req', 'colstrings')
    names(t_colstrings) <- t_name

    data <- emptyDocTemplate(cols = t_colstrings)

  }

  rs <- docTbl(data = data)

  # update class path
  class(rs) <- c('docFuncReq', class(rs))

  # return object
  return(rs)

}

#' Validator for S3 Class 'docFuncReq'
#'
#' @inheritParams obj_msg_result
#'
#' @return R Object (see parameters)
#' @export
validate_docFuncReq <- function(obj, bool_out = FALSE, throw_err = TRUE) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}

  # apply parent class validator
  obj <- validate_docTbl(obj = obj)

  # init empty error messages
  msg <- character()

  # check class path
  if (!isTRUE(inherits(obj, 'docFuncReq'))) {
    msg[length(msg)+1] <- "`obj` must inherit from 'docFuncReq'"
  }

  # check 'colstrings' attribute
  colstrings <- attr(obj, 'colstrings')
  if (isTRUE(is.null(colstrings))) {
    msg[length(msg)+1] <- "`attr(obj, 'colstrings')` was not found"
  }

  if (!isTRUE(setequal(names(colstrings), colnames(obj)))) {
    msg[length(msg)+1] <- "`names(attr(obj, 'colstrings'))` must equal `colnames(obj)`"
  }

  if (!isTRUE(setequal(names(colstrings), getDocTemplate('func_req', 'name')))) {
    msg[length(msg)+1] <- "`names(attr(obj, 'colstrings'))` must match `docTemplates`"
  }

  if (!isTRUE(setequal(colstrings, getDocTemplate('func_req', 'colstrings')))) {
    msg[length(msg)+1] <- "`attr(obj, 'colstrings')` must match `docTemplates`"
  }

  # return result
  obj_msg_result(obj, msg, bool_out, throw_err)

}

#' Constructor with Validation for S3 Class 'docFuncReq'
#'
#' @inheritParams new_docTbl
#' @inheritParams validate_docFuncReq
#'
#' @return S3 object of class 'docFuncReq'
#' @export
docFuncReq <- function(data = NULL, bool_out = FALSE, throw_err = TRUE) {

  validate_docFuncReq(new_docFuncReq(data = data), bool_out = bool_out, throw_err = throw_err)

}

# docNonFuncReq ----

#' Constructor for S3 Class 'docNonFuncReq'
#'
#' Construct a new instance of an S3 object 'docNonFuncReq'.
#' Inherits functionality from S3 Class 'docTbl'.
#'
#' @inheritParams new_docTbl
#'
#' @return S3 object of class 'docNonFuncReq'
#' @export
new_docNonFuncReq <- function(data = NULL) {

  if (is.null(data)) {

    t_name <- getDocTemplate('non_func_req', 'name')
    t_colstrings <- getDocTemplate('non_func_req', 'colstrings')
    names(t_colstrings) <- t_name

    data <- emptyDocTemplate(cols = t_colstrings)

  }

  rs <- docTbl(data = data)

  # update class path
  class(rs) <- c('docNonFuncReq', class(rs))

  # return object
  return(rs)

}

#' Validator for S3 Class 'docNonFuncReq'
#'
#' @inheritParams obj_msg_result
#'
#' @return R Object (see parameters)
#' @export
validate_docNonFuncReq <- function(obj, bool_out = FALSE, throw_err = TRUE) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}

  # apply parent class validator
  obj <- validate_docTbl(obj = obj)

  # init empty error messages
  msg <- character()

  # check class path
  if (!isTRUE(inherits(obj, 'docNonFuncReq'))) {
    msg[length(msg)+1] <- "`obj` must inherit from 'docNonFuncReq'"
  }

  # check 'colstrings' attribute
  colstrings <- attr(obj, 'colstrings')
  if (isTRUE(is.null(colstrings))) {
    msg[length(msg)+1] <- "`attr(obj, 'colstrings')` was not found"
  }

  if (!isTRUE(setequal(names(colstrings), colnames(obj)))) {
    msg[length(msg)+1] <- "`names(attr(obj, 'colstrings'))` must equal `colnames(obj)`"
  }

  if (!isTRUE(setequal(names(colstrings), getDocTemplate('non_func_req', 'name')))) {
    msg[length(msg)+1] <- "`names(attr(obj, 'colstrings'))` must match `docTemplates`"
  }

  if (!isTRUE(setequal(colstrings, getDocTemplate('non_func_req', 'colstrings')))) {
    msg[length(msg)+1] <- "`attr(obj, 'colstrings')` must match `docTemplates`"
  }

  # return result
  obj_msg_result(obj, msg, bool_out, throw_err)

}

#' Constructor with Validation for S3 Class 'docNonFuncReq'
#'
#' @inheritParams new_docTbl
#' @inheritParams validate_docNonFuncReq
#'
#' @return S3 object of class 'docNonFuncReq'
#' @export
docNonFuncReq <- function(data = NULL, bool_out = FALSE, throw_err = TRUE) {

  validate_docNonFuncReq(new_docNonFuncReq(data = data), bool_out = bool_out, throw_err = throw_err)

}

# docTestCases ----

#' Constructor for S3 Class 'docTestCases'
#'
#' Construct a new instance of an S3 object 'docTestCases'.
#' Inherits functionality from S3 Class 'docTbl'.
#'
#' @inheritParams new_docTbl
#'
#' @return S3 object of class 'docTestCases'
#' @export
new_docTestCases <- function(data = NULL) {

  if (is.null(data)) {

    t_name <- getDocTemplate('test_cases', 'name')
    t_colstrings <- getDocTemplate('test_cases', 'colstrings')
    names(t_colstrings) <- t_name

    data <- emptyDocTemplate(cols = t_colstrings)

  }

  rs <- docTbl(data = data)

  # update class path
  class(rs) <- c('docTestCases', class(rs))

  # return object
  return(rs)

}

#' Validator for S3 Class 'docTestCases'
#'
#' @inheritParams obj_msg_result
#'
#' @return R Object (see parameters)
#' @export
validate_docTestCases <- function(obj, bool_out = FALSE, throw_err = TRUE) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}

  # apply parent class validator
  obj <- validate_docTbl(obj = obj)

  # init empty error messages
  msg <- character()

  # check class path
  if (!isTRUE(inherits(obj, 'docTestCases'))) {
    err_msg <- "`obj` must inherit from 'docTestCases'"
    msg <- c(err_msg, msg)
  }

  # check 'colstrings' attribute
  colstrings <- attr(obj, 'colstrings')
  if (isTRUE(is.null(colstrings))) {
    msg[length(msg)+1] <- "`attr(obj, 'colstrings')` was not found"
  }

  if (!isTRUE(setequal(names(colstrings), colnames(obj)))) {
    msg[length(msg)+1] <- "`names(attr(obj, 'colstrings'))` must equal `colnames(obj)`"
  }

  if (!isTRUE(setequal(names(colstrings), getDocTemplate('test_cases', 'name')))) {
    msg[length(msg)+1] <- "`names(attr(obj, 'colstrings'))` must match `docTemplates`"
  }

  if (!isTRUE(setequal(colstrings, getDocTemplate('test_cases', 'colstrings')))) {
    msg[length(msg)+1] <- "`attr(obj, 'colstrings')` must match `docTemplates`"
  }

  # return result
  obj_msg_result(obj, msg, bool_out, throw_err)

}

#' Constructor with Validation for S3 Class 'docTestCases'
#'
#' @inheritParams new_docTbl
#' @inheritParams validate_docTestCases
#'
#' @return S3 object of class 'docTestCases'
#' @export
docTestCases <- function(data = NULL, bool_out = FALSE, throw_err = TRUE) {

  validate_docTestCases(new_docTestCases(data = data), bool_out = bool_out, throw_err = throw_err)

}
