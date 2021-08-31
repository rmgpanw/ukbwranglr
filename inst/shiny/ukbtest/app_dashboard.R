#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(shinydashboard)
library(tidyverse)
library(reactable)
library(ukbwranglr)
library(crosstalk)

options(shiny.maxRequestSize = 10 * 1024^2)

ukb_data_dict <- read_tsv("/Users/alasdair/Documents/Data/UKB/ukb_backup_files/Data_Dictionary_Showcase.tsv") %>%
  mutate(across(everything(), as.character))

test <- make_data_dict("/Users/alasdair/Documents/Data/UKB/AK/raw/ukb29900.tab", delim = "\t", ukb_data_dict = ukb_data_dict)

selected_data_dict_cols <- c("Field",
                             "FieldID",
                             "instance",
                             "array",
                             "Path",
                             "Category",
                             "Participants",
                             "Items",
                             "Stability",
                             "ValueType",
                             "Units",
                             "ItemType",
                             "Strata",
                             "Sexed",
                             "Instances",
                             "Array",
                             "Coding",
                             "Notes",
                             "Link")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  # Application title
  dashboardHeader(title = "ukbwranglr"),
  dashboardSidebar(
    sidebarMenu(
    menuItem(
      "Make data dictionary",
      tabName = "data_dict",
      icon = icon("dashboard")
    )
  )
  ),
  dashboardBody(
    tabItems(tabName = "data_dict",
    fluidRow(
        box(shinyFilesButton(
            "ukb_file",
          label = "Select UKB file",
          title = "Please select a file",
          multiple = FALSE,
          viewtype = "detail"
        )),
        box(downloadButton("download"))
      ),

      # Show a plot of the generated distribution
      fluidRow(
        box(verbatimTextOutput("filepaths")),
        box(reactableOutput("data_dict")),
        box(verbatimTextOutput("selected"))
      )
  )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # # setup for shinyFiles
  # volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  # shinyFileChoose(input, "ukb_file", roots = volumes, session = session)
  #
  # # print file selection to console
  # observe({
  #   cat("\ninput$ukb_file value:\n\n")
  #   print(input$ukb_file)
  # })
  #
  # ## print file selection to browser
  # output$filepaths <- renderPrint({
  #   if (is.integer(input$ukb_file)) {
  #     cat("No files have been selected (shinyFileChoose)")
  #   } else {
  #     parseFilePaths(volumes, input$ukb_file)$datapath %>%
  #       str_split(" • ") %>%
  #       .[[1]] %>%
  #       as.list() %>%
  #       do.call(file.path, .)
  #   }
  # })
  #
  # ## get filepath to selected UKB file
  # ukb_file <- reactive({
  #   if (is.integer(input$ukb_file)) {
  #     cat("No files have been selected (shinyFileChoose)")
  #   } else {
  #     parseFilePaths(volumes, input$ukb_file)$datapath %>%
  #       str_split(" • ") %>%
  #       .[[1]] %>%
  #       as.list() %>%
  #       do.call(file.path, .)
  #   }
  # })
  #
  # # create data dict
  # data_dict <- reactive({
  #   req(ukb_file())
  #
  #   ext <- tools::file_ext(ukb_file())
  #
  #   switch(
  #     ext,
  #     csv = make_data_dict(ukb_file(), delim = ",", ukb_data_dict = ukb_data_dict) %>%
  #       select(all_of(selected_data_dict_cols)),
  #     tsv = make_data_dict(ukb_file(), delim = "\t", ukb_data_dict = ukb_data_dict) %>%
  #       select(all_of(selected_data_dict_cols)),
  #     validate("Invalid file; Please upload a .csv or .tsv file")
  #   )
  # })
  #
  # # display data dictionary for selected UKB file
  # output$data_dict <- renderReactable({
  #
  #   reactable(
  #     # data,
  #     data_dict(),
  #     # TRY UPDATING SHINY TO USE STICKY COLS
  #     # columns = list(
  #     #     Field = colDef(
  #     #         sticky = "left",
  #     #         # Add a right border style to visually distinguish the sticky column
  #     #         style = list(borderRight = "1px solid #eee"),
  #     #         headerStyle = list(borderRight = "1px solid #eee")
  #     #     )
  #     # ),
  #     filterable = TRUE,
  #     searchable = TRUE,
  #     showPageSizeOptions = TRUE,
  #     pageSizeOptions = c(10, 25, 50, 100, 200),
  #     groupBy = "Field",
  #     selection = "multiple",
  #     onClick = "select",
  #     defaultSelected = c(1, 2),
  #     theme = reactableTheme(
  #       rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
  #     ))
  #
  # })
  #
  # # selected rows
  # selected <- reactive(getReactableState("data_dict", "selected"))
  #
  # # print selected rows to app
  # output$selected <- renderPrint({
  #   print(selected())
  # })
  #
  # # print selected rows in console
  # observe({
  #   print(data_dict()[selected(), ])
  # })
  #
  # # download selected rows
  # output$download <- downloadHandler(
  #   filename = function() {
  #     paste0(input$download_file_name, ".csv")
  #   },
  #   content = function(file) {
  #     write_csv(data_dict()[selected(), ], file)
  #   }
  # )
}

# Run the application
shinyApp(ui = ui, server = server)
