
#' Module UI - Action Table
#'
#' UI Function for R shiny module 'action_tbl'.
#' Uses 'bslib' and 'DT' packages to render an editable table.
#'
#' @param id character - module id
#' @param title character - custom title for table
#' @param position character - 'left'/'right', specifies position of sidebar
#'
#' @export
mod_action_tbl_ui <- function(id, title, position = 'left') {

  ns <- NS(id)

  tagList(
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          position = position, open = FALSE, width = 200,
          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              title = "Edit Actions",
              actionButton(inputId = ns('add_btn'), label = "Add Row", class = 'btn-primary'),
              h6(),
              actionButton(inputId = ns('drop_btn'), label = "Drop Row")
            ),
            bslib::accordion_panel(
              title = "Import/Export",
              actionButton(inputId = ns('import_btn'), label = "Import Rows", class = 'btn-success'),
              h6(),
              actionButton(inputId = ns('append_btn'), label = "Append Rows", class = 'btn-warning'),
              h6(),
              actionButton(inputId = ns('overwrite_btn'), label = "Overwrite", class = 'btn-danger')
            )
          )
        ),
        bslib::card_title(title),
        DT::dataTableOutput(ns('tbl'))
      )
    )
  )

}

#' Module Server - Action Table
#'
#' Server Function for R shiny module 'action_tbl'.
#' Uses 'bslib' and 'DT' packages to render an editable table.
#'
#' @param id character - module id
#' @param docs reactive values - list of reactive values created with 'shiny::reactiveValues()'
#' @param doc_id character - name of element in 'docs' containing table data
#' @param rownames logical - TRUE/FALSE, specifies if rownames should be displayed in table
#'
#' @export
mod_action_tbl_server <- function(id, docs, doc_id, rownames = FALSE) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ______________ ----
    # reactive values ----

    # * data ----
    data <- reactive({docs[[doc_id]]})

    # * data_cols ----
    data_cols <- reactive({
      req(data())
      res <- colnames(data())
      names(res) <- attr(data(), 'colstrings')[res]
      res
    })

    # ______ ----
    # outputs ----

    # * tbl ----
    output$tbl <- DT::renderDataTable({
      DT::datatable(
        data = isolate(data()),
        editable = 'cell',
        colnames = isolate(data_cols()),
        rownames = rownames,
        fillContainer = getOption("DT.fillContainer", TRUE)
      )
    })

    proxy_tbl <- DT::dataTableProxy('tbl', session = session)

    # ________ ----
    # observers ----

    # * tbl_cell_edit ----
    observeEvent(input$tbl_cell_edit, {

      info <- input$tbl_cell_edit
      i <- info$row
      j <- info$col + 1L  # column index offset by 1
      v <- info$value
      docs[[doc_id]][i, j] <- DT::coerceValue(v, docs[[doc_id]][i, j])

    })

    # * data() ----
    observeEvent(data(), {

      DT::replaceData(
        proxy = proxy_tbl,
        data = data(),
        resetPaging = FALSE,
        rownames = FALSE
      )

    })

  })

}
