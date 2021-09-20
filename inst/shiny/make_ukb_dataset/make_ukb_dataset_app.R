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
library(tidyverse)
library(reactable)
library(ukbwranglr)

options(shiny.maxRequestSize = 1000000 * 1024^2)

ukb_data_dict <- read_tsv("/Users/alasdair/Documents/Data/UKB/ukb_backup_files/Data_Dictionary_Showcase.tsv") %>%
    mutate(across(everything(), as.character))

test <- make_data_dict("/Users/alasdair/Documents/Data/UKB/AK/raw/ukb29900.tab", delim = "\t", ukb_data_dict = ukb_data_dict)

selected_data_dict_cols <- c(
    "Field",
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
    "Link",
    "descriptive_colnames",
    "colheaders_raw",
    "colheaders_processed"
)


# UI ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UKB data dictionary"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            width = 2,
            h5("UKB file paths"),
            shinyFilesButton("ukb_main_dataset_filepath",
                             label = "UKB main dataset",
                             title = "Please select a UK Biobank main dataset file",
                             multiple = FALSE,
                             viewtype = "detail"),
            shinyFilesButton("data_dict_preselected",
                             label = "Data dictionary",
                             title = "Please select a UK Biobank main dataset data dictionary file",
                             multiple = FALSE,
                             viewtype = "detail"),
            h5("Download data dictionary"),
            downloadButton("download_data_dict_full",
                           label = "All variables"),
            downloadButton("download_data_dict_selected",
                           label = "Selected variables"),
        ),

        # Show the data dictionary
        mainPanel(
            verbatimTextOutput("ukb_main_dataset_filepath"),
            verbatimTextOutput("data_dict_preselected_filepath"),
           reactableOutput("data_dict"),
           verbatimTextOutput("selected"),
           width = 9
        )
    )
)


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {


# SHINYFILES SETUP --------------------------------------------------------

    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())

# Main dataset ------------------------------------------------------------

    # main dataset file
    shinyFileChoose(input, "ukb_main_dataset_filepath", roots = volumes, session = session)

    # print UKB main dataset file selection to console
    observe({
        cat("\ninput$ukb_main_dataset_filepath value:\n\n")
        print(input$ukb_main_dataset_filepath)
    })

    # print file selection to browser
    output$ukb_main_dataset_filepath <- renderPrint({
        if (is.integer(input$ukb_main_dataset_filepath)) {
            cat("Please select a UKB main dataset")
        } else {
            parseFilePaths(volumes, input$ukb_main_dataset_filepath)$datapath %>%
                str_split(" • ") %>%
                .[[1]] %>%
                as.list() %>%
                do.call(file.path, .)
        }
    })

    # get filepath to main dataset
    ukb_main_dataset_filepath <- reactive({
        if (is.integer(input$ukb_main_dataset_filepath)) {
            cat("No files have been selected (shinyFileChoose)")
        } else {
            parseFilePaths(volumes, input$ukb_main_dataset_filepath)$datapath %>%
                str_split(" • ") %>%
                .[[1]] %>%
                as.list() %>%
                do.call(file.path, .)
        }
    })


# Data dictionary (pre-selected) ------------------------------------------

    # pre-selected data dictionary
    shinyFileChoose(input, "data_dict_preselected", roots = volumes, session = session)

    # print selection to console
    observe({
        cat("\ninput$data_dict_preselected value:\n\n")
        print(input$data_dict_preselected)
    })

    # print file selection to browser
    output$data_dict_preselected_filepath <- renderPrint({
        if (is.integer(input$data_dict_preselected)) {
            cat("Please select a data dictionary (optional)")
        } else {
            parseFilePaths(volumes, input$data_dict_preselected)$datapath %>%
                str_split(" • ") %>%
                .[[1]] %>%
                as.list() %>%
                do.call(file.path, .)
        }
    })

    # get filepath to selected UKB data dictionary file (of pre-selected variables)
    data_dict_preselected_filepath <- reactive({
        if (is.integer(input$data_dict_preselected)) {
            cat("No files have been selected (shinyFileChoose)")
        } else {
            parseFilePaths(volumes, input$data_dict_preselected)$datapath %>%
                str_split(" • ") %>%
                .[[1]] %>%
                as.list() %>%
                do.call(file.path, .)
        }
    })



# Data dictionary reactable table -----------------------------------------

    # create data dict
    data_dict <- reactive({
        req(ukb_main_dataset_filepath())

        ext <- tools::file_ext(ukb_main_dataset_filepath())

        switch(
            ext,
            csv = make_data_dict(ukb_main_dataset_filepath(), delim = ",", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols)),
            tsv = make_data_dict(ukb_main_dataset_filepath(), delim = "\t", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols)),
            txt = make_data_dict(ukb_main_dataset_filepath(), delim = "\t", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols)),
            tab = make_data_dict(ukb_main_dataset_filepath(), delim = "\t", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols)),
            validate("Invalid file extension for main dataset; Please upload a comma (.csv) or tab delimited (.tsv/.txt/.tab) file")
        )
    })

    # read data dictionary of pre-selected variables (optional)
    data_dict_preselected <- reactive({
        req(data_dict_preselected_filepath())

        ext <- tools::file_ext(data_dict_preselected_filepath())

        if (ext != "csv") {
            validate("Invalid file extension for data dicionary, must be a csv file")
        }

        readr::read_csv(data_dict_preselected_filepath())
    })

    # display data dictionary for selected UKB file
    output$data_dict <- renderReactable({
        req(data_dict())

        reactable(
            # data,
            data_dict(),
            filterable = TRUE,
            searchable = TRUE,
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(10, 25, 50, 100, 200),
            groupBy = "Field",
            selection = "multiple",
            onClick = "select",
            defaultSelected = 1,
            theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
            ))

    })

    # selected rows
    selected <- reactive(getReactableState("data_dict", "selected"))

    # update selected rows to include those in data_dict_preselected
    observeEvent(data_dict_preselected(), {
        # currently selected
        currently_selected_rowids <- getReactableState("data_dict", "selected")

        # get row indices to update
        preselected_rowids <- data_dict() %>%
            tibble::rowid_to_column() %>%
            dplyr::filter(colheaders_raw %in% data_dict_preselected()$colheaders_raw) %>%
            dplyr::pull(rowid)

        # union
        updated_selection_rowids <- unique(c(currently_selected_rowids, preselected_rowids))

        # update
        updateReactable("data_dict", selected = updated_selection_rowids)
    })

    # print selected rows to app
    output$selected <- renderPrint({
        print(selected())
    })

    # print selected rows in console
    observe({
        print(data_dict()[selected(), ])
    })


# Downloads ---------------------------------------------------------------


    # download full data dictionary
    output$download_data_dict_full <- downloadHandler(
        filename = function() {
            paste0(input$download_file_name, "data_dict_full.csv")
        },
        content = function(file) {
            write_csv(data_dict(), file, na = "")
        }
    )

    # download data dictionary of selected rows only
    output$download_data_dict_selected <- downloadHandler(
        filename = function() {
            paste0(input$download_file_name, "data_dict_selected.csv")
        },
        content = function(file) {
            write_csv(data_dict()[selected(), ], file, na = "")
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
