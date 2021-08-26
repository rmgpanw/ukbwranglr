#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(reactable)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UKB data dictionary"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("upload", "Upload UKB data dictionary", accept = c(".csv", ".tsv")),
            downloadButton("download"),
            width = 3
        ),

        # Show a plot of the generated distribution
        mainPanel(
           reactableOutput("ukb_data_dict"),
           verbatimTextOutput("selected"),
           width = 9
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # upload table from file
    ukb_data_dict <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    
    switch(ext,
           csv = read_delim(input$upload$datapath, delim = ","),
           tsv = read_delim(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file"))
    })

    # render reactable table
    output$ukb_data_dict <- renderReactable({
        reactable(ukb_data_dict(),
                  # TRY UPDATING SHINY TO USE STICKY COLS
                  # columns = list(
                  #     Species = colDef(
                  #         sticky = "left",
                  #         # Add a right border style to visually distinguish the sticky column
                  #         style = list(borderRight = "1px solid #eee"),
                  #         headerStyle = list(borderRight = "1px solid #eee")
                  #     )
                  # ),
                  filterable = TRUE,
                  searchable = TRUE,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 25, 50, 100, 200),
                  selection = "multiple", 
                  onClick = "select",
                  defaultSelected = c(1, 2),
                  theme = reactableTheme(
                      rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
                  ))
    })
    
    # selected rows
    selected <- reactive(getReactableState("ukb_data_dict", "selected"))
    
    # print selected rows to app
    output$selected <- renderPrint({
        print(selected())
    })
    
    # print selected rows in console
    observe({
        print(ukb_data_dict()[selected(), ])
    })
    
    # download selected rows
    output$download <- downloadHandler(
        filename = function() {
            paste0(input$download_file_name, ".csv")
        },
        content = function(file) {
            write_csv(ukb_data_dict()[selected(), ], file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
