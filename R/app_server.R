
#' Application Server
#' @import shiny bslib
#' @param input,output,session Internal Paremeters for {shiny}
#'     DO NOT REMOVE
#' @noRd
app_server <- function(input, output, session) {

  # ____________ ----
  # reactive values ----

  # * docs ----
  docs <- reactiveValues(
    user_stories = docUserStory(),
    func_req = docFuncReq(),
    non_func_req = docNonFuncReq(),
    test_cases = docTestCases()
  )

  # _______ ----
  # modules ----

  # * view_func_req ----
  view_func_req <- mod_action_tbl_server(id = 'view_func_req', docs = docs, doc_id = 'func_req')

  # * view_non_func_req ----
  view_non_func_req <- mod_action_tbl_server(id = 'view_non_func_req', docs = docs, doc_id = 'non_func_req')

  # * tbl_func_req ----
  tbl_func_req <- mod_action_tbl_server(id = 'tbl_func_req', docs = docs, doc_id = 'func_req')

  # * tbl_non_func_req ----
  tbl_non_func_req <- mod_action_tbl_server(id = 'tbl_non_func_req', docs = docs, doc_id = 'non_func_req')


  # ______ ----
  # outputs ----

  # * tbl_user_stories ----
  output$tbl_user_stories <- DT::renderDataTable({
    DT::datatable(
      data = docs$user_stories,
      fillContainer = getOption("DT.fillContainer", TRUE)
    )
  })

  # * tbl_func_req ----
  output$tbl_func_req <- DT::renderDataTable({
    DT::datatable(
      data = docs$func_req,
      fillContainer = getOption("DT.fillContainer", TRUE)
    )
  })

  # * tbl_non_func_req ----
  output$tbl_non_func_req <- DT::renderDataTable({
    DT::datatable(
      data = docs$non_func_req,
      fillContainer = getOption("DT.fillContainer", TRUE)
    )
  })



}
