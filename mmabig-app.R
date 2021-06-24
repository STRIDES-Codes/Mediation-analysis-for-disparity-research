#
# Shiny App for Multiple Mediation Analysis for Big Data Sets
# NIH Code-a-thon 2021
# Team: Nguyen, Briana, Jia-Hua, Rime, Qingzhao 
# Last updated: June 6, 2021
#

library(shiny)
library(mmabig)

dta <- read.csv("dta.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Multiple Mediation Analysis for Big Data Sets"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Select your file", 
                multiple = T, 
                accept = c("text/csv",
                           "text/comma-separated-values",
                           ".csv",
                           ".sas7bdat")),
      checkboxInput("header", "Header", T),
      splitLayout(         
        selectInput("sep", "Separator:", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
        selectInput("dec", "Decimal:", choices = c(Period = ".", Comma = ","), selected = ".")
      ),
      splitLayout(
        selectInput("quote", "Quote:", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'),
        selectInput("disp", "Display:", choices = c(Head = "head", All = "all"), selected = "head")
      ),
      tags$hr(),
      selectInput("dataset","Data:",
                  choices =list(uploaded_file = "inFile"), selected=NULL),
      selectInput("pred", "Independent Variable(s):", choices = NULL, multiple = T),
      selectInput("y", "Dependent Variable(s):", choices = NULL, multiple = T),
      selectInput("m", "Mediator(s):", choices = NULL, multiple = T),
      numericInput("boot", "Number of Boostrapts Samples", 50, 0, 1000, step = 1),
      actionButton("run", "Submit", class = "btn-success")
    ),
    mainPanel(
      h3("Dataframe"),
      tableOutput("contents"),
      h3("Mediation Results"),
      verbatimTextOutput("mediation")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Preview of dataframe
  output$contents <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote, 
                   dec = input$dec
    )
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  # update variables based on the data
  observe({
    #browser()
    if(!exists(input$dataset)) return() #make sure upload exists
    var.opts<-colnames(get(input$dataset))
    updateSelectInput(session, "y", choices = var.opts)
    updateSelectInput(session, "pred", choices = var.opts)
    updateSelectInput(session, "m", choices = var.opts)
  })
  
  # get data object
  get_data<-reactive({
      if(!exists(input$dataset)) return() # if no upload
      check<-function(x){is.null(x) || x==""}
      if(check(input$dataset)) return()

      obj<-list(data=get(input$dataset),
                y=input$y,
                pred=input$pred,
                m=input$m
      )
      # require all to be set to proceed
      if(any(sapply(obj,check))) return()
      #make sure choices had a chance to update
      check<-function(obj){
          !all(c(obj$y,obj$pred,obj$m) %in% colnames(obj$data))
      }
      if(check(obj)) return()
      obj
  })
  
  upload_data<-reactive({
      inFile <- input$file1
      if (is.null(inFile))
          return(NULL)
      # could also store in a reactiveValues
      read.csv(input$file1$datapath)
  })

  observeEvent(input$file1, {
      inFile<<-upload_data()
  })
  
  # create mma.big output  
  output$mediation <- renderPrint({
    
    dataset_list <- get_data
    
    pred <- dataset_list$pred
    y <- dataset_list$y
    m <- subset(dataset_list, select = -c(pred, y))
    
    model <- mma.big(x = m, y = y, mediator = 1:ncol(m),
                             pred = pred, alpha = 1, alpha1 = 0.05, alpha2 = 0.05)
    
    summary(model)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
