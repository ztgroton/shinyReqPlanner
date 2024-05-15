
#' Get Document Template Fields
#'
#' @param template character - name of template
#' @param cols - character - column names to return
#'
#' @return R Object
#' @export
#'
getDocTemplate <- function(template, cols) {

  if (missing(template)) {stop("`template` is missing", call. = TRUE)}
  if (missing(cols)) {stop("`cols` is missing", call. = TRUE)}

  return(shinyReqPlanner::docTemplates[shinyReqPlanner::docTemplates$template == template, c(cols)])

}

#' Convert Tablular Data to Document Template Format
#'
#' @param data data.frame
#' @param template character
#'
#' @return R Object
#' @export
data2DocTemplate <- function(data, template) {

  if (missing(data)) {stop("`data` is missing", call. = TRUE)}
  if (missing(template)) {stop("`template` is missing", call. = TRUE)}

  # init empty error messages
  msg <- character()

  # check `data` is data.frame
  if (!isTRUE(is.data.frame(data))) {
    msg[length(msg)+1] <- "`data` must be data.frame"
  } else {

    # check all columns are character
    if (!isTRUE(all(sapply(data, is.character)))) {
      msg[length(msg)+1] <- "`data` must only contain character columns"
    }

    # check `colnames(data)` against template 'colstrings'
    if (!isTRUE(setequal(colnames(data), getDocTemplate(template, 'colstrings')))) {
      msg[length(msg)+1] <- "`colnames(data)` must match template 'colstrings'"
    }

  }

  # format data according to template
  t_name <- getDocTemplate(template, 'name')
  t_colstrings <- getDocTemplate(template, 'colstrings')
  t_name_map <- t_name

  names(t_name_map) <- t_colstrings
  names(t_colstrings) <- t_name

  colnames(data) <- t_name_map[colnames(data)]
  attr(data, 'colstrings') <- t_colstrings

  # return result
  obj_msg_result(data, msg, bool_out = FALSE, throw_err = FALSE)

}
