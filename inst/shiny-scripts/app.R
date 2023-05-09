library(shiny)
library(shinyalert)


# Define UI for random distribution app ----
ui <- fluidPage(

  # App title ----
  titlePanel(tags$h1(tags$b("gateCounts:"),"Calculates Visitor Count Summaries Using Daily Gate Counts")),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      tags$p("Description: This is a simple Shiny App that is part of the
             gateCounts R package. Provided a csv file containing raw daily
             gate counts in the specified format, the Shiny app will
             calculate the daily, weekly, monthly visitor count summaries
             and statistics adjusted for several factors outlined under
             in README file. The package was developed to improve
             methodologies for calculating visitor counts from gate
             counts, initially using library daily gate count values as
             an example. However, the package can be applied to calculate
             visitor counts from any setting. The factors adjusted for
             are outlined below."),

      # br() element to introduce extra vertical spacing ----
      br(),
      br(),

      # input
      tags$b("Instructions: Below, enter or select values required to perform
              the analysis. Default values are shown. Then press 'Run'.
              Navigate through the different tabs to the right to explore
              the results."),

      # br() element to introduce extra vertical spacing ----
      br(),
      br(),
      br(),
      # input
      shinyalert::useShinyalert(),  # Set up shinyalert
      uiOutput("tab2"),
      actionButton(inputId = "data1",
                   label = "Data 1 Details"),
      fileInput(inputId = "file1",
                label = "Select a gate count dataset for analysis.
                The file should be in .csv format only. The file
                should have number of rows equaling to length of
                days and columns equaling to two, such that the
                dimension is: days x 2. Here days is the number of
                days for which raw gate counts are present. First
                column must contain the dates and should contain
                column name 'dates'. Dates must be in the format
                of date-month-year. The second column must contain
                the gate count reading for the given date and should
                be called 'counts'.",
                accept = c(".csv")),
      selectInput(inputId = 'gateType',
                  label = 'Select the gate type method:',
                  choices = c("Unidirectional",
                              "Bidirectional")),
      textInput(inputId = "gatecounterMaxValue",
                label = "Enter a numeric value greater than 0 indicating the gate counter max value:", "999999"),
      selectInput(inputId = 'printMessages',
                  label = 'Select TRUE or FALSE for printing messages:',
                  choices = c("TRUE",
                              "FALSE")),

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
                  tabPanel("Plot of Daily Data",
                           h3("Instructions: Enter values on the left side and click 'Run' at the bottom."),
                           h3("Plot of Raw Count Data Provided By User:"),
                           br(),
                           plotOutput("lineplot1")),
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
                         header = TRUE1)
  })

  startcalculating <- eventReactive(eventExpr = input$button2, {
    withProgress(message = 'Calculating', value = 1, {
      # Number of times we'll go through the loop

      gateCountSummary(
        rawGateCounts = as.vector(dataInput()),
        gateType = as.character(input$gateType),
        gatecounterMaxValue = as.numeric(input$gatecounterMaxValue),
        printMessages = FALSE)

    })
  })

  # Textoutput - Raw data summary
  output$textOut <- renderPrint({
    if (! is.null(startcalculating()))
      startcalculating()$monthlyVisitorCounts
  })

  # Visualize raw counts
  output$lineplot <- renderPlot({
    if (! is.null(startcalculating()))
      gateCountsVisDaily(
        outputDailyCounts = startcalculating())[[4]]
  })


  # Step II: calculating
  output$calculating <- renderText({
    if (! is.null(startcalculating()))
      startcalculating()$monthlyVisitorCounts
  })



  # URLs for downloading data
  url1 <- a("Example Data 1", href="https://drive.google.com/file/d/1HcjsdN6RvMys0-hdSJCgecV0yZvvIRWi/view?usp=sharing")
  output$tab2 <- renderUI({
    tagList("Download:", url1)
  })

  observeEvent(input$data1, {
    # Show a modal when the button is pressed
    shinyalert(title = "Example Data 1",
               text = "This is a simulated data 1 for daily gate counts generated using
               random numbers from R Poisson model, rpois. To read more about the function
               see ?rpois in R. Here, the gate counter maximum is assumed 999,999 and gate
               maybe treated either unidirectional or bidirectional. Data was generated
               in September, 2022. To save the file, go to the Shiny app screen, click
               link, then click 'Download' from the top right side.",
               type = "info")
  })


}

# Create Shiny app ----
shinyApp(ui, server)

# END
