
#' Get Document Template Fields
#'
#' @param template character - name of template
#' @param cols - character - column names to return
#'
#' @return R Object
#' @export
#'
getDocTemplates <- function(template, cols) {

  return(shinyReqPlanner::docTemplates[shinyReqPlanner::docTemplates$template == template, c(cols)])

}
