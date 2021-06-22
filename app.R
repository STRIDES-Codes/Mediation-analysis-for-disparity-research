#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram

ui<-pageWithSidebar(
    headerPanel("Statistical analysis"),
    sidebarPanel(
        fileInput('file1', 'Select your file',
                  accept = c(
                      'text/csv',
                      'text/comma-separated-values','.csv',
                      '.sas7bdat'
                  )
        ),
        splitLayout(
            textInput("subvar","Subset Variable:"),
            textInput("subval","Subset Value:")),
        splitLayout(
            textInput("dep","Dependent variable:"),
            textInput("indep","Independent variable:")),
        selectInput("varlist","Pick Treatment
variable:",choices=c(colnames(data()))),
selectInput("Model","Model:",
            list("Linear regression" = "lm",
                 "Logistic regression" = "logistic",
                 "Survival analysis" = "surv",
                 "Anova test" = "anova",
                 "Chisquare test" = "chisq",
                 "Correlation" = "corr",
                 "Bootstrap CI" = "BCa")),
radioButtons("type", "Select the plot file
type",choices=list("png","pdf"))
    ),
# Show the output and plot
mainPanel(
    h3(textOutput("caption")),
    plotOutput("myPlot"),
    downloadButton("save1", "Save the plot"),
    verbatimTextOutput("regSum")
)
)

server<-function(input, output, session) {
    # Import data
    data1 <- reactive({
        inFile <- input$file1
        if(is.null(file)){return()} [1]
        read_sas(inFile$datapath)
    })
    observeEvent(input$file1,{
        updateSelectInput(session,"varlist",choices=c(colnames(data1())))
    })
    # Compute the forumla text
    formulaText <- reactive({
        paste(input$dep,"~", input$indep)
    })
    output$caption <- renderText({
        formulaText()
    })
    # Data
    new_data<-reactive({
        raw_data <- data1()
        if (input$Model == 'surv') {
            df<-data.frame(
                Time = raw_data[[input$dep]],
                trt = raw_data[[input$varlist]],
                cen = raw_data[[input$indep]],
                sub = raw_data[[input$subvar]]
            )
            if (input$subval == "") { df2<-df }
            else { df2<-subset(df,sub==input$subval) }
            return(df2)
        }
        if (input$Model == 'logistic') {
            df<-data.frame(
                y = raw_data[[input$dep]],
                trt = raw_data[[input$indep]],
                sub = raw_data[[input$subvar]]
            )
            if (input$subval == "") { df2<-df }
            else { df2<-subset(df,sub==input$subval) }
            return(df2)
        }})
        # Statistics
        new_fit<-reactive({
            if (input$Model == 'surv') {
                fit<-survfit(Surv(Time , cen) ~ trt , data = new_data())
                return(fit)
            }
            if (input$Model == 'logistic') {
                fit<-glm(y ~ factor(trt), data = new_data(), family = "binomial")
                print(paste('Odds ratio:',exp(coef(fit))))
                return(fit)
            }})
            # Graph
            new_plot<-reactive({
                if (input$Model == 'surv') {
                    graph<-ggsurvplot(new_fit(), risk.table = TRUE, pval = TRUE, data =
                                          new_data())
                    return(graph)
                }
                if (input$Model == 'logistic') {
                    graph<-ggplot(new_data(),aes(y))+geom_bar()+facet_wrap('trt')
                    +labs(x=input$dep)
                    return(graph)
                }
                # Generate the plot
                output$myPlot <- renderPlot({
                    new_plot()
                })
                # Generate the summary of analysis
                output$regSum <- renderPrint({
                    summary(new_fit())
                })
                # Save the plot
                output$save1 <- downloadHandler(
                    filename = function() {
                        paste("myplot",input$type,sep=".")
                    },
                    content = function(file){
                        if(input$type=="png") png(file)
                        else pdf(file)
                        print(new_plot())
                        dev.off()
                    })
            })
            }
# Run the application 
shinyApp(ui = ui, server = server)
