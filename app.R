library(shiny)
library(dplyr)
library(DT)
library(caret)
library(forecast)


mad = read.csv("Madison.csv", header=T, sep=',')
mke = read.csv("Milwaukee.csv", header = T, sep=',')
madClean = read.csv("Madison_clean.csv", header=T, sep=',')
mkeClean = read.csv("Milwaukee_clean.csv", header = T, sep=',')

ui <- navbarPage("Predicting Brix Cider Sales in Milwaukee",
  
  tabPanel("Logistic Regression",
           sidebarLayout(
             sidebarPanel(
               #pick own variables
               uiOutput("ind2_val_select"), 
               uiOutput("rest_var_select")
              ),
           mainPanel( 
             helpText("Confusion Matrix from your Selected Variables"),
                        verbatimTextOutput("log_val_show")))
          ),

  #Display table with MKE data predicted values - Use best model variables, and just show company and actual, predicted
  tabPanel("Dataset Input",
           sidebarLayout(
             sidebarPanel(
               uiOutput("var1_select"),
               uiOutput("input_var_select")
             ),
             mainPanel(
               uiOutput("table")
             ))
           )
)

server<-function(input,output) { 
  
output$rest_var_select<-renderUI({
  checkboxGroupInput("log_var_select","Select Variables:",choices =as.list(names(madClean[,-c(1,2,5)])), selected = "City")
})
output$ind2_val_select<-renderUI({
  selectInput("log_ind_var_select","Select Independent Var", choices =as.list(names(madClean[,-c(1,2)])),multiple = FALSE, selected = "Vendor")
})
output$log_val_show <- renderPrint({
  if(is.null(madClean)){return ()}
  if(is.null(mkeClean)){return ()}
  input$log_var_select
  input$log_ind_var_select
  set.seed(2)
  trainIndex<-createDataPartition(madClean[,-c(1,2)]$Vendor, p=.7, list=FALSE, times=1)
  train.df<-madClean[trainIndex,]
  valid.df<-madClean[-trainIndex,]
  form <- sprintf("%s~%s",input$log_ind_var_select,paste0(input$log_var_select,collapse="+"))
  logit <- glm(as.formula(form),data=train.df,family="binomial")
  logit.reg.pred <- predict(logit, valid.df, type = "response")
  p <- data.frame(actual=valid.df$Vendor, predicted = ifelse(logit.reg.pred>=.5, 1, 0))
  confusionMatrix(factor(p$predicted), factor(p$actual))
})
output$input_var_select<-renderUI({
  checkboxGroupInput("variables","Select Variables:",choices =as.list(names(madClean[,-c(1,2,5)])), selected = "City")
})
output$var1_select<-renderUI({
  selectInput("ind_var_select","Select Independent Var", choices =as.list(names(madClean[,-c(1,2)])),multiple = FALSE, selected = "Vendor")
})
output$table <- renderTable({
  if(is.null(madClean)){return ()}
  if(is.null(mkeClean)){return ()}
  input$variables
  input$ind_var_select
  set.seed(2)
  trainIndex<-createDataPartition(madClean[,-c(1,2)]$Vendor, p=.7, list=FALSE, times=1)
  train.df<-madClean[trainIndex,]
  valid.df<-madClean[-trainIndex,]
  form <- sprintf("%s~%s",input$ind_var_select,paste0(input$variables,collapse="+"))
  logit <- glm(as.formula(form),data=train.df,family="binomial")
  k <- predict(logit, mkeClean, type = "response")
  m <- data.frame(Company = mkeClean$Company, Type = mke$Type, Distance.Miles = mke$Distance, Predicted.Percentage = k)
  m <- na.omit(m)
  m
})
}
shinyApp(ui=ui,server=server)