
#' Application UI
#' @import shiny
#' @noRd
app_ui <- function() {

  navbarPage(
    title = "Project Requirements", id = 'tabs',
    tabPanel(
      title = "User Stories",
      fluidRow(
        sidebarLayout(
          sidebarLayout(

          ),
          mainPanel(

          )
        )
      ),
      value = 'user_stories'
    )
  )

}
