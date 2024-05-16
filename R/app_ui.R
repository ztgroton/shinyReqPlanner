
#' Application UI
#' @import shiny bslib
#' @noRd
app_ui <- function() {

  page_navbar(
    title = "Project Requirements", id = 'page',
    theme = bs_theme(version = 5, preset = 'cosmo'),
    underline = FALSE,

    sidebar = sidebar(
      HTML("TEST")
    ),

    nav_panel(
      title = "User Stories",
      layout_columns(
        col_widths = c(4, 8, 6, 6),
        row_heights = c(1, 2),
        card(
          card_title("User Story")
        ),
        card(
          card_title("Story Steps")
        ),
        card(
          card_title("Functional Requirements")
        ),
        card(
          card_title("Non-Functional Requirements")
        )
      ),
      value = 'user_stories'
    ),

    nav_menu(
      title = "Requirements",

      nav_panel(
        title = "Functional",
        card(
          card_title("Functional Requirements")
        ),
        value = 'fun_req'
      ),

      nav_panel(
        title = "Non-Functional",
        card(
          card_title("Non-Functional Requirements")
        ),
        value = 'non_func_req'
      )

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
      input_dark_mode(id = "dark_mode", mode = "dark")
    )

  )

}
