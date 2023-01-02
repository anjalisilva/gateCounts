library(shiny)


# Define UI for random distribution app ----
ui <- fluidPage(

  # App title ----
  titlePanel(tags$h1(tags$b("gateCounts:"),"Calculates Cumulative Gate Counts")),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      tags$p("Description: This is a simple Shiny App that is part of the
             gateCounts package. Provided a csv file containing raw daily
             gate counts in the specified format, the Shiny app will
             calculate the cumulative count sum adjusted for several
             factors outlined under package details. The package was developed
             to improve current methodologies for calculating cumulative gate
             counts."),

      # br() element to introduce extra vertical spacing ----
      br(),
      br(),

      # input
      tags$b("Instructions: Below, enter or select values required to perform the analysis.
              Default values are shown. Then press 'Run'. Navigate through
              the different tabs to the right to explore the results."),

      # br() element to introduce extra vertical spacing ----
      br(),
      br(),
      br(),
      # input
      shinyalert::useShinyalert(),  # Set up shinyalert
      uiOutput("tab2"),
      actionButton(inputId = "data1",
                   label = "Data 1 Details"),
      uiOutput("tab1"),
      actionButton(inputId = "data2",
                   label = "Data 2 Details"),
      fileInput(inputId = "file1",
                label = "Select a gate count dataset for analysis. Important: The file should be in .csv format with rows corresponding to dates and only one column, containing daily raw gate counts. There should be no header.",
                accept = c(".csv")),
      selectInput(inputId = 'gateType',
                  label = 'Select the gate type method:',
                  choices = c("Unidirectional",
                              "Bidirectional")),
      textInput(inputId = "gatecounterMaxValue",
                label = "Enter a numeric value greater than 0 indicating the gate counter max value:", "999999"),

      # br() element to introduce extra vertical spacing ----
      br(),

      # actionButton
      actionButton(inputId = "button2",
                   label = "Run"),

      # br() element to introduce extra vertical spacing -
      br(),

    ), # End of side pannel


    # Main panel for displaying outputs
    mainPanel(

      # Output: Tabet
      tabsetPanel(type = "tabs",
                  tabPanel("Plot of Raw Data",
                           h3("Instructions: Enter values on the left side and click 'Run' at the bottom."),
                           h3("Plot of Raw Count Data Provided By User:"),
                           br(),
                           plotOutput("lineplot")),
                  tabPanel("Input Summary",
                           h3("Instructions: Enter values on the left side and click 'Run' at the bottom."),
                           h3("Summary of Input Data Provided By User:"),
                           br(),
                           verbatimTextOutput("textOut")),
                  tabPanel("Output Count",
                           h3("Instructions: Enter values on the left side and click 'Run' at the bottom."),
                           h3("Summary of Output Data, Cumulative Sum:"),
                           br(),
                           verbatimTextOutput('calculating')),
      )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {

  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression


  # Step I: save input csv as a reactive
  dataInput <- reactive({
    if (! is.null(input$file1))
      read.csv(input$file1$datapath,
                         sep = ",",
                         header = FALSE,
                         row.names = 1)$V2
  })

  startcalculating <- eventReactive(eventExpr = input$button2, {
    withProgress(message = 'Calculating', value = 1, {
      # Number of times we'll go through the loop

      gateCountCumulative(
        rawGateCounts = as.vector(dataInput()),
        gateType = as.character(input$gateType),
        gatecounterMaxValue = as.numeric(input$gatecounterMaxValue),
        printMessages = FALSE)

    })
  })

  # Textoutput - Raw data summary
  output$textOut <- renderPrint({
    if (! is.null(startcalculating()))
      turnNumeric <- as.numeric(dataInput())
      summary(turnNumeric)
  })

  # Visualize raw counts
  output$lineplot <- renderPlot({
    if (! is.null(startcalculating()))
      turnNumeric <- as.numeric(dataInput())
      plot(turnNumeric, type = "l", lty=5, xlab = "day",
           ylab = "raw daily counts", main = "Raw counts")
  })


  # Step II: calculating
  output$calculating <- renderText({
    if (! is.null(startcalculating()))
      startcalculating()$adjustedCountSum
  })



  # URLs for downloading data
  url1 <- a("Example Data 2", href="https://raw.githubusercontent.com/anjalisilva/TestingPackage/master/inst/extdata/GeneCountsData2.csv")
  output$tab1 <- renderUI({
    tagList("Download:", url1)
  })


  url2 <- a("Example Data 1", href="https://drive.google.com/file/d/1jMBTPpsBwaigjR3mO49AMYDxzjVnNiAv/view?usp=sharing")
  output$tab2 <- renderUI({
    tagList("Download:", url2)
  })

  observeEvent(input$data1, {
    # Show a modal when the button is pressed
    shinyalert(title = "Example Data 1",
               text = "This is a simulated data 1 for daily gate counts generated using
               random numbers from R Poisson model, rpois. To read more about the function
               see ?rpois in R. Data was generated in September, 2022. To save the file,
               go to the Shiny app screen, clink link, then click 'Download' from the
               top right side.",
               type = "info")
  })

  observeEvent(input$data2, {
    # Show a modal when the button is pressed
    shinyalert(title = "Example Data 2",
               text = "This is a simulated data 2 for daily gate counts generated using
               random numbers from R Poisson model, rpois. To read more about the function
               see ?rpois in R. Data was generated in September, 2022. To save the file,
               go to the Shiny app screen, clink link, then click 'Download' from the
               top right side.",
               type = "info")
  })


}

# Create Shiny app ----
shinyApp(ui, server)

# END
