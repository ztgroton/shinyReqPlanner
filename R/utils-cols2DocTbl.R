
#' Build Empty 'Document' Table
#'
#' Initialize an empty data.frame of character vectors. Object is meant
#' as a base class implementation of a requirements document table.
#'
#' @param cols named character vector
#'
#' @return data.frame
#' @export
cols2DocTbl <- function(cols) {

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
