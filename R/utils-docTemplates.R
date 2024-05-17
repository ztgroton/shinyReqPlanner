
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

  file_path <- system.file('extdata/csv', 'docTemplates.csv', package = 'shinyReqPlanner')
  data <- utils::read.csv(file_path, encoding = 'UTF-8', stringsAsFactors = FALSE)

  return(data[data$template == template, c(cols)])

}

#' Build Empty 'Document' Table
#'
#' Initialize an empty data.frame of character vectors. Object is meant
#' as a base class implementation of a requirements document table.
#'
#' @param cols named character vector
#'
#' @return data.frame
#' @export
emptyDocTemplate <- function(cols) {

  if (missing(cols)) {stop("`cols` is missing", call. = TRUE)}

  # check for names attribute
  if (is.null(attr(cols, 'names', exact = TRUE))) {stop("`names(cols)` cannot be NULL", call. = TRUE)}

  # get names attribute
  cols_n <- names(cols)

  # remove whitespace
  cols_n <- gsub(' ', '', cols_n, fixed = TRUE)
  cols <- trimws(gsub('\\s+', ' ', cols))

  # check for blank values
  if (any(is.na(cols_n))) {stop("`names(cols)` cannot contain NA values", call. = TRUE)}
  if (any(is.na(cols))) {stop("`cols` cannot contain NA values", call. = TRUE)}

  if (any(cols_n == '')) {stop("`names(cols)` cannot contain blank values", call. = TRUE)}
  if (any(cols == '')) {stop("`cols` cannot contain blank values", call. = TRUE)}

  # check for non-alphanumeric characters
  non_alnum_cols_n <- grep("[^a-zA-Z0-9]", cols_n)
  if (length(non_alnum_cols_n) > 0) {stop("`names(cols)` cannot contain non-alphanumeric characters", call. = TRUE)}

  # build data.frame
  doc_df <- as.list(rep(NA_character_, length(cols)))
  names(doc_df) <- cols_n
  doc_df <- as.data.frame(doc_df)
  attr(doc_df, 'colstrings') <- cols

  # return
  return(doc_df)

}

#' Convert Tablular Data to Document Template Format
#'
#' @param data data.frame
#' @param template character
#'
#' @return R Object
#' @export
dataDocTemplate <- function(data, template) {

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

  if (isTRUE(length(msg) == 0)) {

    # format data according to template
    t_name <- getDocTemplate(template, 'name')
    t_colstrings <- getDocTemplate(template, 'colstrings')
    t_name_map <- t_name

    names(t_name_map) <- t_colstrings
    names(t_colstrings) <- t_name

    colnames(data) <- t_name_map[colnames(data)]
    attr(data, 'colstrings') <- t_colstrings

  }

  # return result
  obj_msg_result(data, msg, bool_out = FALSE, throw_err = FALSE)

}
