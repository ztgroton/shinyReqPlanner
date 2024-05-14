
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

  openxlsx::write.xlsx(x = obj, file = file, ..., colWidths = 'auto')

}
