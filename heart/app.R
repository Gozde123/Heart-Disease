# heart data app

library(shiny)
library(ggplot2)
library(shinythemes)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
library(knitr)

dt <- read.csv("C:/Users/gozde/Desktop/Heart Data/heart.csv")
xAxisChoices <- colnames(dt)
yAxisChoices <- colnames(dt)
colChoices<-c("brown","pink","green","black","red","purple",
              "yellow","blue","orange","gray","darkred",
              "darkblue","magenta","lightsalmon")
predicChoices<-colnames(dt)[c(1,2,4,5,6,8,9,10)]

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "Heart Disease",
    dropdownMenu(
      type = "notifications",
      notificationItem(
        text = "Welcome my app",
        icon = icon("exclamation-triangle"),
        status = "warning"
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("General",tabName = "general",icon = icon("home")),
      menuItem("Plots", tabName = "plots",icon = icon("th")),
      menuItem("Summary", tabName = "summary",icon = icon("dashboard")),
      menuItem("Reference", tabName = "reference",icon = icon("link")),
      menuItem("Prediction",tabName = "predict",icon = icon("question")),
      menuItem("Power BI",tabName = "power",icon=icon("link"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "general",
        fluidRow(
          column(
            width = 12,
            align = "center",
            textOutput("spacer"),
            textOutput("text1")
          ),
          column(
            width = 12,
            align = "center",
            imageOutput("image", width = 600, height = 500)
          )
        )
      ),
      # Plots Tab
      tabItem(
        tabName = "plots",
        fluidRow(
          box(
            title = "Plot of Heart Disease Data Set",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput("xSelector", label = "Select the x axis:", choices = xAxisChoices),
            selectInput("ySelector", label = "Select the y axis:", choices = yAxisChoices),
            selectInput("colSelector",label="Select the color",choices=colChoices),
            actionButton("refreshPlot", label = "Refresh"),
            plotOutput("p1")
          )
        )
      ),
      tabItem(
        tabName = "summary",
        fluidRow(
          box(
            title = "Summary of Heart Disease Data Set",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput("Summary")
          )
        )
      ),
      
      # Reference Tab
      tabItem(
        tabName = "reference",
        fluidRow(
          box(
            title = "Reference",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            tags$a("This app is built with a Kaggle dataset",
                   href="https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction")
          ),
          downloadButton("download1"),
          tableOutput("table1")
        )
      ),
      # prediction tab
      tabItem(
        tabName = "predict",
        fluidRow(
          box(
            title = "Prediction of Heart Disease Data Set",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput("prediction", label = "Select the prediction:", choices = predicChoices),
            actionButton("MakePrediction", label = "Predict"),
            tableOutput("summary2")
          ),
          box(
            title = "Prediction Formula",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("formula_box")
          )
        )
      ),
      tabItem(
        tabName = "power",
        fluidRow(
          column(
            width = 12,
            align = "center",
            textOutput("spacer2"),
            textOutput("text2")
          ),
          column(
            width = 12,  # Use the full width of the column
            align = "center",  # Align content in the center
            imageOutput("image2", width = 800, height = 600)
          )
        )
      )
    )
  )
)


server <- function(input, output){
  logit_result <- eventReactive(input$MakePrediction, {
    print("Logit result reactive event triggered")
    dt$HeartDisease <- ifelse(dt$HeartDisease == 0, "No Heart Disease", "Yes Heart Disease")
    dt$HeartDisease <- factor(dt$HeartDisease)
    dt$HeartDisease <- ifelse(dt$HeartDisease == "Yes Heart Disease", 1, 0)
    dt$HeartDisease <- factor(dt$HeartDisease)
    dt$Sex <- factor(dt$Sex)
    dt$ExerciseAngina <- factor(dt$ExerciseAngina)
    dt$RestingECG <- factor(dt$RestingECG)
    dt$ST_Slope <- factor(dt$ST_Slope)
    dt$ChestPainType <- factor(dt$ChestPainType)
    
    # Relevel the factors
    dt$Sex <- relevel(dt$Sex, ref = "M")
    dt$ExerciseAngina <- relevel(dt$ExerciseAngina, ref = "N")
    
    if (input$prediction == "Cholesterol") {
      logit <- glm(data = dt, formula = HeartDisease ~ Cholesterol, family = binomial)
    } else if (input$prediction == "FastingBS") {
      logit <- glm(data = dt, formula = HeartDisease ~ FastingBS, family = binomial)
    } else if (input$prediction == "Sex") {
      dt$Sex <- factor(dt$Sex, levels = c("M", "F"))  # Replace "M" and "F" with your actual levels
      logit <- glm(data = dt, formula = HeartDisease ~ Sex, family = binomial)
    } else if (input$prediction == "ExerciseAngina") {
      dt$ExerciseAngina <- factor(dt$ExerciseAngina, levels = c("N", "Y"))  # Replace "No" and "Yes" with your actual levels
      logit <- glm(data = dt, formula = HeartDisease ~ ExerciseAngina, family = binomial)
    } else if (input$prediction == "Oldpeak") {
      logit <- glm(data = dt, formula = HeartDisease ~ Oldpeak, family = binomial)
    } else if (input$prediction=="MaxHR"){
      logit <- glm(data = dt, formula = HeartDisease ~ MaxHR, family = binomial)
    } else if(input$prediction=="Age"){
      logit<-glm(data=dt,formula=HeartDisease~Age,family = binomial)
    } else{
      logit<-glm(data=dt,formula=HeartDisease ~ RestingBP,family=binomial)
    }
    
    # Return the fitted model
    logit
  })
  
  plot1 <- eventReactive(input$refreshPlot, {
    if (input$xSelector == "Age" && input$ySelector == "HeartDisease") {
      dt$HeartDisease<-as.factor(dt$HeartDisease)
      plot <- ggplot(data = dt, aes(y = HeartDisease, x = Age)) +
        geom_boxplot(col=input$colSelector) +
        labs(y = "Heart Disease", x = "Age", title = "Heart Disease vs Age")
    } else if (input$xSelector == "HeartDisease" && input$ySelector == "Age") {
      dt$HeartDisease<-as.factor(dt$HeartDisease)
      plot <- ggplot(data = dt, aes(x = HeartDisease, y = Age)) +
        geom_boxplot(col=input$colSelector) +
        labs(x = "Heart Disease", y = "Age", title = "Heart Disease vs Age")
      
    } else if (input$xSelector == "HeartDisease" && input$ySelector == "Cholesterol") {
      dt$HeartDisease<-as.factor(dt$HeartDisease)
      plot <- ggplot(data = dt, aes(x = HeartDisease, y = Cholesterol)) +
        geom_boxplot(col=input$colSelector) +
        labs(x = "Heart Disease", y = "Cholesterol", title = "Heart Disease vs Cholesterol")
    } else if (input$ySelector == "HeartDisease" && input$xSelector == "Cholesterol") {
      dt$HeartDisease<-as.factor(dt$HeartDisease)
      plot <- ggplot(data = dt, aes(x = Cholesterol, y = HeartDisease)) +
        geom_boxplot(col=input$colSelector) +
        labs(x = "Cholesterol", y = "HeartDisease", title = "Heart Disease vs Cholesterol")
    } else if (input$ySelector=="Cholesterol" && input$xSelector=="MaxHR" ){
      plot<-ggplot(data=dt, aes(x=Cholesterol, fill=as.factor(MaxHR))) +
        geom_bar(position="dodge",col=input$colSelector,fill=input$colSelector)+
        labs(x = "Cholesterol", y = "Count", title = "Cholesterol")
    } else if (input$xSelector=="Cholesterol" && input$ySelector=="MaxHR" ){
      plot <- ggplot(data=dt, aes(x=MaxHR, fill=as.factor(Cholesterol))) +
        geom_bar(position="dodge",col=input$colSelector,fill=input$colSelector)+
        labs(x = "MaxHR", y= "Count", title = "MaxHr")
    } else if (input$xSelector=="RestingBP" && input$ySelector=="Cholesterol" ){
      plot <- ggplot(data=dt, aes(x=RestingBP, fill=as.factor(Cholesterol))) + 
        geom_bar(position="dodge",col=input$colSelector,fill=input$colSelector)+
        labs(x = "RestingBP", y = "Count", title = "RestingBP")
    }else if (input$xSelector=="Cholesterol" && input$ySelector=="RestingBP" ){
      plot <- ggplot(data=dt, aes(x=Cholesterol, fill=as.factor(RestingBP))) + 
        geom_bar(position="dodge",col=input$colSelector,fill=input$colSelector)+
        labs(x = "Cholesterol", y = "Count", title = "Cholesterol")
    } else {
      plot <- ggplot(data = dt, aes_string(x = input$xSelector, fill = as.factor(input$ySelector))) +
        geom_bar(position="dodge",col=input$colSelector,fill=input$colSelector)+
        labs(x = input$xSelector, y = "Count", title = input$xSelector)
    } 
    plot
  })
  output$summary2 <- renderTable({
    # Check if the model has been fitted before displaying results
    if (!is.null(logit_result())) {
      summary_data <- summary(logit_result())
      coefficients <- summary_data$coefficients
      
      # Extract only the Estimate, Std. Error, z value, and Pr(>|z|) columns
      coefficients <- coefficients[, c("Estimate", "Std. Error", "z value", "Pr(>|z|)")]
      
      # Return the coefficients as a data frame
      coefficients
    } else {
      # Return an empty data frame if the model hasn't been fitted yet
      data.frame()
    }
  })
  output$formula_box<-renderText({
    if(input$prediction=="Cholesterol"){
      logt <- paste("1.15 + (-0.0046) *", "\"Cholesterol\"")
    } else if(input$prediction=="Age"){
      logt <- paste("(-3.21) + 0.064 *", "\"Age\"")
    } else if(input$prediction=="FastingBS"){
      logt <- paste("(-0.07959) + 1.43120 *", "\"FastingBS\"")
    } else if(input$prediction=="Sex"){
      logt <- paste("(0.5396) - (1.5904) *", "\"Female\"")
    } else if(input$prediction=="ExerciseAngina"){
      logt <- paste("-0.61462 + 2.36303 *", "\"ExerciseAngina\"")
    } else if(input$prediction=="Oldpeak"){
      logt <- paste("(-0.5689) + (1.00672) *", "\"(Depression)\"")
    } else if(input$prediction=="MaxHR"){
      logt <- paste("5.3 + (-0.037) *", "\"Maximum Heart Rate\"")
    } else {
      logt <- paste("-1.372 * (0.012) *", "\"RestingBP\"")
    }
    logt
  })
  
  output$image <- renderImage({
    list(src = "photo.webp", width = 600, height = 500)
  }, deleteFile = FALSE)
  output$image2 <- renderImage({
    list(src = "new.png", width = 800, height = 600)
  }, deleteFile = FALSE)
  output$text1<-renderText("This web page are made for giving the general information about heart disease data set. It also has POWER BI and R Markdown files.")
  output$text2<-renderText("This image is built with a Power BI")
  output$spacer <- renderText(" ") 
  output$spacer2 <- renderText(" ") 
  output$p1 <- renderPlot(plot1())
  output$Summary<-renderTable(summary(dt))
  output$table1<-renderTable(dt)
  output$download1 <- downloadHandler(
    filename = function() {
      "heart_data.csv"  # Specify the desired filename here
    },
    content = function(file) {
      write.csv(dt, file)  # Write the 'dt' data frame to the file
    }
  )
}

shinyApp(ui, server)


