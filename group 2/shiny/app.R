library(shiny)
library(mmabig)

options(shiny.maxRequestSize=100*1024^2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel("Import data", 
                 fileInput("file", "Data", accept = ".csv", buttonLabel = "Upload..."),
                 numericInput("pred", "Column number of one predictor",25),
                 numericInput("y", "Column number of one outcome",1),
                 numericInput("m", "Column number of mediators",3),
                 actionButton("run1", "Run data.org.big"),
                 actionButton("run2", "Run med.big"),
                 actionButton("run3", "Run mma.big")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table", tableOutput("table")),
                  tabPanel("Result 1", 
                           textOutput("summary_result1"),
                           downloadButton("result1", "Download result 1")),
                  tabPanel("Result 2", 
                           textOutput("summary_result2"),
                           downloadButton("result2", "Download result 2")),
                  tabPanel("Result 3", 
                           textOutput("summary_result3"),
                           downloadButton("result3", "Download result 3")))
      )
))


server <- function(input, output) {
  MyData <- reactive({
    
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath)
    
    return(tbl)
  })
  
  output$table <- renderTable({
    MyData()
  })
  
  factor_pred <- reactive({input$pred})
  factor_y <- reactive({input$y})
  factor_m <- reactive({input$m})

  observeEvent(input$run1,{
    result1 <- reactive({
      pred <- MyData()[,input$pred]
      y <- MyData()[,input$y]
      m <- MyData()[,input$m]
      
      R_resultl <- data.org.big(x=m,
                                y=data.frame(y),
                                mediator=1:ncol(m),
                                pred=data.frame(pred),
                                testtype=1)
      
      return(R_resultl)
    })
    output$summay_result1 <- reactive(summary(result1()))
    
    output$result1 <- downloadHandler(
      filename <- function(){
        paste("result1.Rdata")
      },
      
      content = function(file) {
        data_result1 <- result1()
        save(data_result1, file = file)
      })
  })
  
  
  observeEvent(input$run2,{
    result2 <- reactive({med.big(resultl())})
    output$summay_result2 <- reactive(summary(result2()))
    output$result2 <- downloadHandler(
      filename <- function(){
        paste("result2.Rdata")
      },
      
      content = function(file) {
        data_result2 <- result2()
        save(data_result2, file = file)
      })
  })
  
  observeEvent(input$run3,{
    result3 <- reactive({
      pred <- MyData()[,as.numeric(factor_pred)]
      y <- MyData()[,as.numeric(factor_y)]
      m <- MyData()[,as.numeric(factor_m)]
      
      R_result3 <- mma.big(x=m,y=data.frame(y), mediator=1:ncol(m),
                           pred=data.frame(pred), alpha=1, alpha1=0.05, alpha2=0.05)
      return(R_result3)
    })
    
    output$summay_result3 <- reactive(summary(result3()))
    
    output$result3 <- downloadHandler(
      filename <- function(){
        paste("result3.Rdata")
      },
      
      content = function(file) {
        data_result3 <- result1()
        save(data_result3, file = file)
      })
  })
  
}

shinyApp(ui = ui, server = server)