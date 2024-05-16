
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

#' User-Friendly Error Message for Set Differences
#'
#' Displays elements missing from 'x' and unexpected elements in 'y'
#'
#' @param x character vector
#' @param y character vector
#'
#' @return character
#' @export
msg_setdiff <- function(x, y) {

  if (missing(x)) {stop("`x` is missing", call. = TRUE)}
  if (missing(y)) {stop("`y` is missing", call. = TRUE)}

  x_y <- setdiff(x,y)
  y_x <- setdiff(y,x)

  if (isTRUE(length(x_y) > 0)) {
    msg_xy <- paste0("  Missing: (", paste(x_y, collapse = ', '), ")")
  } else {
    msg_xy <- NULL
  }

  if (isTRUE(length(y_x) > 0)) {
    msg_yx <- paste0("  Unexpected: (", paste(y_x, collapse = ', '), ")")
  } else {
    msg_yx <- NULL
  }

  if (isTRUE(length(x_y)>0) || isTRUE(length(y_x)>0)) {
    msg_head <- "\n Difference in Vectors: "
  } else {
    msg_head <- NULL
  }

  msg <- paste(c(msg_head, msg_xy, msg_yx), collapse = '\n')
  return(msg)

}
