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
    "descriptive_colnames",
    "colheaders_raw",
    "colheaders_processed",
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
    "Link"
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UKB data dictionary"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            width = 2,
            h5("UKB file paths"),
            shinyFilesButton("ukb_file",
                             label = "UKB main dataset",
                             title = "Please select a file",
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
            verbatimTextOutput("filepaths"),
           reactableOutput("data_dict"),
           verbatimTextOutput("selected"),
           width = 9
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # setup for shinyFiles
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyFileChoose(input, "ukb_file", roots = volumes, session = session)

    # print file selection to console
    observe({
        cat("\ninput$ukb_file value:\n\n")
        print(input$ukb_file)
    })

    ## print file selection to browser
    output$filepaths <- renderPrint({
        if (is.integer(input$ukb_file)) {
            cat("Please select a UKB main dataset")
        } else {
            parseFilePaths(volumes, input$ukb_file)$datapath %>%
                str_split(" • ") %>%
                .[[1]] %>%
                as.list() %>%
                do.call(file.path, .)
        }
    })

    ## get filepath to selected UKB file
    ukb_file <- reactive({
        if (is.integer(input$ukb_file)) {
            cat("No files have been selected (shinyFileChoose)")
        } else {
            parseFilePaths(volumes, input$ukb_file)$datapath %>%
                str_split(" • ") %>%
                .[[1]] %>%
                as.list() %>%
                do.call(file.path, .)
        }
    })

    # create data dict
    data_dict <- reactive({
        req(ukb_file())

        ext <- tools::file_ext(ukb_file())

        switch(
            ext,
            csv = make_data_dict(ukb_file(), delim = ",", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols)),
            tsv = make_data_dict(ukb_file(), delim = "\t", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols)),
            txt = make_data_dict(ukb_file(), delim = "\t", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols)),
            tab = make_data_dict(ukb_file(), delim = "\t", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols)),
            validate("Invalid file extension; Please upload a comma (.csv) or tab delimited (.tsv/.txt/.tab) file")
        )
    })

    # display data dictionary for selected UKB file
    output$data_dict <- renderReactable({

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

    # print selected rows to app
    output$selected <- renderPrint({
        print(selected())
    })

    # print selected rows in console
    observe({
        print(data_dict()[selected(), ])
    })

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
