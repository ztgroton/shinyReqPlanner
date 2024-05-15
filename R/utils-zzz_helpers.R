
#' Product Final Output based on User Settings and Error Messages
#'
#' @param obj R Object
#' @param msg character - error messages
#' @param bool_out TRUE/FALSE - should output be TRUE when input object
#' is valid, or should the input object be silently returned.
#' @param throw_err TRUE/FALSE - should function stop and throw error
#' messages in case of invalid input objects, or return character vector
#' of messages as function output.
#'
#' @return R Object (see parameters)
#' @export
obj_msg_result <- function(obj, msg, bool_out = FALSE, throw_err = TRUE) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}
  if (missing(msg)) {stop("`msg` is missing", call. = TRUE)}

  # return result
  if (isTRUE(length(msg) == 0)) {
    if (isTRUE(bool_out)) {return(TRUE)} else {invisible(obj)}
  } else {
    if (isTRUE(throw_err)) {
      final_msg <- paste0('\n', paste(msg, collapse = '\n'))
      stop(final_msg, call. = TRUE)
    } else {
      return(msg)
    }
  }

}
