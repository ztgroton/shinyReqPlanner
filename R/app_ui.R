
#' Application UI
#' @import shiny bslib
#' @noRd
app_ui <- function() {

  page_navbar(
    title = "Project Requirements", id = 'page',
    theme = bs_theme(version = 5, preset = 'cosmo'),
    underline = FALSE,

    nav_panel(
      title = "User Stories",

      layout_columns(
        col_widths = c(12, 12),
        mod_action_tbl_ui('view_func_req', title = "Functional Requirements"),
        mod_action_tbl_ui('view_non_func_req', title = "Non-Functional Requirements")
      ),
      value = 'stories'
    ),

    nav_panel(
      title = "Requirements",
      layout_columns(
        col_widths = c(12, 12),
        mod_action_tbl_ui('tbl_non_func_req', title = "Non-Functional Requirements"),
        mod_action_tbl_ui('tbl_func_req', title = "Functional Requirements")
      ),
      value = 'requirements'
    ),

    nav_panel(
      title = "Test Cases",
      card(
        card_title("Test Cases")
      ),
      value = "test_cases"
    ),

    nav_spacer(),

    nav_item(
      input_dark_mode(id = "dark_mode", mode = "light")
    )

  )

}
