
# exportExcel ----

#' S3 Generic - Export S3 Object to Excel
#'
#' @param obj S3 Object
#' @param file character - file path for excel export
#' @param ... R ellipsis
#'
#' @export
exportExcel <- function(obj, file, ...) {UseMethod("exportExcel", obj)}


#' S3 Method - Export S3 Class 'docTbl' to Excel
#'
#' @inheritParams exportExcel
#' @export
exportExcel.docTbl <- function(obj, file, ...) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}
  if (missing(file)) {stop("`file` is missing", call. = TRUE)}

  # change column names using 'colstrings' attribute
  file_data <- as.data.frame(obj)
  colnames(file_data) <- attr(obj, 'colstrings')[colnames(file_data)]

  # export to excel
  openxlsx::write.xlsx(x = file_data, file = file, ..., colWidths = 'auto')

}

#' S3 Method - Export S3 Class 'docUserStory' to Excel
#'
#' @inheritParams exportExcel
#' @export
exportExcel.docUserStory <- function(obj, file, ...) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}
  if (missing(file)) {stop("`file` is missing", call. = TRUE)}

  NextMethod()

}

#' S3 Method - Export S3 Class 'docFuncReq' to Excel
#'
#' @inheritParams exportExcel
#' @export
exportExcel.docFuncReq <- function(obj, file, ...) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}
  if (missing(file)) {stop("`file` is missing", call. = TRUE)}

  NextMethod()

}

#' S3 Method - Export S3 Class 'docNonFuncReq' to Excel
#'
#' @inheritParams exportExcel
#' @export
exportExcel.docNonFuncReq <- function(obj, file, ...) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}
  if (missing(file)) {stop("`file` is missing", call. = TRUE)}

  NextMethod()

}

#' S3 Method - Export S3 Class 'docTestCases' to Excel
#'
#' @inheritParams exportExcel
#' @export
exportExcel.docTestCases <- function(obj, file, ...) {

  if (missing(obj)) {stop("`obj` is missing", call. = TRUE)}
  if (missing(file)) {stop("`file` is missing", call. = TRUE)}

  NextMethod()

}
