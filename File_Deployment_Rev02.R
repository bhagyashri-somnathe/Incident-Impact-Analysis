library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
library(tableHTML)

#----------------------------------------------------------------------------------------------
Test_10 <- read.csv("D:/DataScience-26th Feb/Projects/Incident_Prediction/Final Project/test_top5.csv")

#----------------------------------------------------------------------------------------------
#************************* Define UI for app **************************************

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Incident Impact Analysis",titleWidth = 350),
                                   
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Prediction (User Input)", tabName = "pred1", icon = icon("list-alt", lib = "glyphicon")),
                            menuItem("Upload File", tabName = "dashboard", icon = icon("dashboard")),
                            menuItem("Prediction (File Upload)", tabName = "pred", icon = icon("list-alt", lib = "glyphicon")),
                            menuItem("Graph", tabName = "Graph", icon = icon("stats", lib = "glyphicon"))
                        )
                    ),
                    
                    dashboardBody(
                        
                        # Changing theme
                        shinyDashboardThemes(
                            theme = "blue_gradient"
                        ),
    tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                fluidRow(
                    box(title="File Upload", fileInput("file1", "Choose CSV File"), status = "success", solidHeader = TRUE, width = "12"),
                    box(title = "Data Preview",DT::dataTableOutput("contents"), status = "success", solidHeader = TRUE, width = "12")
                    
                )
        ),
        
        # Second tab content
        tabItem(tabName = "Graph",
                
                h2("Analysis"),
                
                fluidRow(
                    box(title = "Unique Values in each Column", plotOutput("propGraph1"), status = "success", solidHeader = TRUE),
                    box(title = "Proportion Graph(After Prediction)", plotOutput("propGraph2"), status = "success", solidHeader = TRUE)
                )   
                
        ),
        
        #third tab content
        tabItem(tabName = "pred",
                h2("Model Prediction"),
                verbatimTextOutput("prop"),
                verbatimTextOutput("confusionMatrix"),
                verbatimTextOutput("modelSummary"),
                downloadButton("downloadData", "Download Predictions")
        ), 
                            
    #fourth tab content                    
    tabItem(tabName = "pred1",
            h2("Enter Required Input"),
    br(),
    
    fluidRow(
        
        box(width= 4, title = "Incident ID",background = "teal",
            splitLayout(textInput(inputId = "INC",label=" ",value = "INC"),
                        selectizeInput(inputId = "id",label="",
                                       choices = sort(unique(Test_10$ID)),
                                       options = list(maxOptions=1000)),width = 3,status = "success")),
        
        box(width= 4,title = "ID Caller",background = "teal",
            splitLayout(textInput(inputId = "Caller",label=" ",value =  "Caller"),
                        selectizeInput(inputId = "id_caller",label = " ",
                                       choices = sort(unique(Test_10$ID_caller)),
                                       options = list(maxOptions=1000)),width = 3,status = "success")),
        
        box(width= 4,title = "Opened By",background = "teal",
            splitLayout(textInput(inputId = "Opened_by",label=" ",value ="Opened by"),
                        selectInput(inputId = "open_by",label=" ",
                                    choices = sort(unique(Test_10$opened_by)),
                                    selected = unique(Test_10$opened_by)[1]),width = 3,status = "success")),
        
        box(width= 4,title = "Location", background = "teal",
            splitLayout(textInput(inputId = "location",label = " ",value ="Location"),
                        selectInput(inputId = "Location",label=" ",
                                    choices = sort(unique(Test_10$location)),
                                    selected = unique(Test_10$location)[1]),width = 3,status = "success"),
                        tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))),
        
        box(width= 4,title = "User Symptom",background = "teal",
            splitLayout(textInput(inputId = "user_symptom",label = " ",value = "Symptom"),
                        selectInput(inputId = "user_symptom",label=" ",
                                    choices = sort(unique(Test_10$user_symptom)),
                                    selected = unique(Test_10$user_symptom)[1]),width = 3,status = "success")),
        
        br(),
        br(),
        
        fluidRow(column(12,actionButton("go","Predict", icon("paper-plane"), 
                     style="color: #fff; background-color: #708090; border-color: black"))),
        br(),
        br(),
        verbatimTextOutput("predvalue"),
        imageOutput("img")
    )
    )
    )
    ) 
    
)

options(shiny.maxRequestSize=40*1024^2) # for max size exceeded

# Define server logic to read selected file ----

server <- function(input, output) {
    
    data <- reactive({
        req(input$file1)
    })
    
    output$contents <- DT :: renderDataTable({
        req(input$file1)
        df <- read.csv(input$file1$datapath)
        DT::datatable(df, options = list(pageLength = 10,autoWidth =TRUE,scrollX =TRUE))
    })
    
    output$propGraph1 <- renderPlot({
        req(input$file1)
        df <- read.csv(input$file1$datapath,header = TRUE)
        
        # Unique values in each variable in ordered way
        uni <- sort(lengths(lapply(df,unique)),decreasing = T)
        
        # 01_Barplot of Unique Values in each column
        b1=barplot(uni, col = rainbow(7), las=2, ylim = c(0,21000),cex.axis = 0.60,cex.names = 0.60,
                   ylab = 'Count', main = 'Unique Value Frequency')
        text(x=b1, y= uni, labels = uni,pos = 3,cex = 0.6,col = 'black')
        
    })
    
    output$modelSummary <- renderPrint({
        req(input$file1)
        df <- read.csv(input$file1$datapath)
        model <- load(file = "D:/DataScience-26th Feb/Projects/Incident_Prediction/Final Project/rf_up_1.R")
        pred_data <- df[,1:6]
        data1 <- data.frame(pred_data[,1:5],impact =pred_data[,6],Predicted = predict(get(model),pred_data[,1:6]))
        print(data1)
        prop1 <- table(data1$Predicted)
        output$prop <- renderPrint(prop1)
        confusionmatrix <- confusionMatrix(data1$Predicted,data1$impact)
        output$confusionMatrix <- renderPrint(confusionmatrix)
        
        # Downloadable csv of selected dataset ----
        output$downloadData <- downloadHandler(
            filename = function() {
                paste(input$data1, "predictions.csv", sep = "")
            },
            content = function(file) {
                write.csv(data1, file, row.names = FALSE)
                
            }) 
        
        output$propGraph2 <- renderPlot({
            data1 %>% 
                count(pred_impact = factor(data1$Predicted)) %>% 
                mutate(Percentage = prop.table(n)) %>% 
                ggplot(aes(x = pred_impact, y = Percentage, fill = pred_impact, label = scales::percent(Percentage))) + 
                geom_col(position = 'dodge') + 
                geom_text(position = position_dodge(width = .9),
                          vjust = -0.5, 
                          size = 3) + 
                scale_y_continuous(labels = scales::percent)
            
        })
    })
    
    observeEvent(input$go,
                 {
                     frames <- cbind(ID = as.numeric(input$id),ID_caller=as.numeric(input$id_caller),
                                     opened_by = as.numeric(input$open_by),location = as.numeric(input$Location),
                                     user_symptom = as.numeric(input$user_symptom))
                     
                     frames <- data.frame(frames)
                     model <- load(file = "D:/DataScience-26th Feb/Projects/Incident_Prediction/Final Project/rf_up_1.R")
                     dt <- as.character(predict(get(model),frames))
                     output$predvalue <- renderText({dt})
                     
                     output$img <- renderImage({
                         
                         if(dt == "1 - High"){
                             list(src ="D:/DataScience-26th Feb/Projects/Incident_Prediction/Final Project/high.jpg",
                                  width = 250,height = 150)
                         } 
                         
                         else if(dt == "2 - Medium"){
                             list(src ="D:/DataScience-26th Feb/Projects/Incident_Prediction/Final Project/medium.jpg",
                                  width = 250,height = 150)
                         }
                         
                         else {
                             list(src ="D:/DataScience-26th Feb/Projects/Incident_Prediction/Final Project/low.jpg",
                                  width = 250,height = 150)
                         }
                     },deleteFile = FALSE)
   }
    )
}

# Create Shiny app ----
shinyApp(ui, server)
            