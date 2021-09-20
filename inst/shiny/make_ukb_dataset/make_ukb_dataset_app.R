
# SETUP -------------------------------------------------------------------

library(shiny)
library(shinyFiles)
library(tidyverse)
library(reactable)
library(ukbwranglr)

options(shiny.maxRequestSize = 1000000 * 1024^2)

# ukb_data_dict <- read_tsv("/Users/alasdair/Documents/Data/UKB/ukb_backup_files/Data_Dictionary_Showcase.tsv") %>%
#     mutate(across(everything(), as.character))

ukb_data_dict <- ukbwranglr::get_ukb_data_dict()
ukb_codings <- ukbwranglr::get_ukb_codings()

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


# Conditional UI elements -------------------------------------------------

download_data_dict_tabs <- tabsetPanel(
    id = "download_data_dict_inputs",
    type = "hidden",
    tabPanel("empty"),
    tabPanel("download_data_dict_inputs",
        tags$hr(),
        h4("Download data dictionary"),
        downloadButton("download_data_dict_full",
                       label = "All variables"),
        downloadButton("download_data_dict_selected",
                       label = "Selected variables")
    )
)

make_dataset_tabs <- tabsetPanel(
    id = "make_dataset_inputs",
    type = "hidden",
    tabPanel("empty"),
    tabPanel(
        "make_dataset_inputs",
        tags$hr(),
        h4("Create dataset"),
        numericInput(
            "nrows",
            "N rows to include",
            value = 510000,
            min = 0,
            max = 510000
        ),
        checkboxInput("descriptive_colnames", "Descriptive column names", value = TRUE),
        checkboxInput("labelled", "Labelled", value = TRUE),
        textInput(
            "ukb_dataset_outname",
            "Dataset file name",
            value = paste0("my_ukb_dataset_", Sys.Date(), ".rds")
        ),
        downloadButton("ukb_dataset",
                       label = "Make dataset"),
    )
)

# Main UI -----------------------------------------------------------------

ui <- fluidPage(

    # Application title
    titlePanel("Make a UK Biobank dataset"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            width = 2,
            h4("Input file paths"),
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
            download_data_dict_tabs,
            make_dataset_tabs
        ),

        # Show the data dictionary
        mainPanel(
            verbatimTextOutput("ukb_main_dataset_filepath"),
            verbatimTextOutput("data_dict_preselected_filepath"),
           reactableOutput("data_dict"),
           verbatimTextOutput("selected variable indices"),
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

# DATA DICTIONARY REACTABLE TABLE -----------------------------------------

    # create data dict
    ukb_main_dataset_filepath_ext <- reactive({
        req(ukb_main_dataset_filepath())

        ext <- tools::file_ext(ukb_main_dataset_filepath())

        if (!ext %in% c("csv", "tsv", "txt", "tab", "dta")) {
            validate("Invalid file extension for main dataset; Please upload a comma (.csv), tab delimited (.tsv/.txt/.tab) or STATA (.dta) file")
        }

        ext
    })

    data_dict <- reactive({
        req(ukb_main_dataset_filepath(),
            ukb_main_dataset_filepath_ext())

        switch(
            ukb_main_dataset_filepath_ext(),
            csv = make_data_dict(ukb_main_dataset_filepath(), delim = ",", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols)),
            tsv = make_data_dict(ukb_main_dataset_filepath(), delim = "\t", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols)),
            txt = make_data_dict(ukb_main_dataset_filepath(), delim = "\t", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols)),
            tab = make_data_dict(ukb_main_dataset_filepath(), delim = "\t", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols)),
            dta = make_data_dict(ukb_main_dataset_filepath(), delim = "\t", ukb_data_dict = ukb_data_dict) %>%
                select(all_of(selected_data_dict_cols))
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


# DYNAMIC UI ELEMENTS -----------------------------------------------------

    observeEvent(data_dict(), {
        if (!is.null(data_dict())) {
            updateTabsetPanel(inputId = "download_data_dict_inputs", selected = "download_data_dict_inputs")
            updateTabsetPanel(inputId = "make_dataset_inputs", selected = "make_dataset_inputs")
        } else {
            updateTabsetPanel(inputId = "download_data_dict_inputs", selected = "empty")
            updateTabsetPanel(inputId = "make_dataset_inputs", selected = "empty")
        }
    })

# DOWNLOADS ---------------------------------------------------------------

# Data dictionary - full --------------------------------------------------
    output$download_data_dict_full <- downloadHandler(
        filename = function() {
            ("data_dict_full.csv")
        },
        content = function(file) {
            write_csv(data_dict(), file, na = "")
        }
    )


# Data dictionary - selected rows only ------------------------------------
    output$download_data_dict_selected <- downloadHandler(
        filename = function() {
            ("data_dict_selected.csv")
        },
        content = function(file) {
            write_csv(data_dict()[selected(), ], file, na = "")
        }
    )


# UKB dataset -------------------------------------------------------------
    output$ukb_dataset <- downloadHandler(
        filename = function() {
            input$ukb_dataset_outname
        },
        content = function(file) {
            # setup
            ext <- tools::file_ext(file)

            if (!ext %in% c("rds", "dta", "csv", "tsv")) {
                showNotification("Invalid file type. File extensionn must be one of '.rds', '.dta', '.csv' or '.tsv'. Please click 'cancel' in dialog box",
                                 type = "error")
                stop("Invalid file type. File extensionn must be one of '.rds', '.dta', '.csv' or '.tsv'")
            }

            req(data_dict())

            delim <- switch(ukb_main_dataset_filepath_ext(),
                            csv = ",",
                            tsv = "\t",
                            txt = "\t",
                            tab = "\t",
                            dta = "")

            # read selected variables
            notify <- function(msg, id = NULL) {
                showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
            }

            message("Reading UKB data")
            id <- notify("Reading data...")
            on.exit(removeNotification(id), add = TRUE)

            ukb_main <- ukbwranglr::read_ukb(path = ukb_main_dataset_filepath(),
                                             delim = delim,
                                             ukb_data_dict = ukb_data_dict,
                                             ukb_codings = ukb_codings,
                                             data_dict = data_dict()[selected(), ],
                                             descriptive_colnames = input$descriptive_colnames,
                                             labelled = input$labelled,
                                             nrows = input$nrows)

            # write dataset
            message("Writing data\n")
            notify("Writing data...", id = id)

            switch(
                ext,
                rds = saveRDS(ukb_main, file = file),
                dta = haven::write_dta(ukb_main, path = file),
                csv = readr::write_csv(ukb_main, file = file, na = ""),
                tsv = readr::write_tsv(ukb_main, file = file, na = "")
                )

            message("Finished writing data")
            showNotification("Dataset succesfully created!")
        }
    )


# RUN APPLICATION ---------------------------------------------------------
}

shinyApp(ui = ui, server = server)
