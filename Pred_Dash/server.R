library(shiny)
library(plotly)
library(shinydashboard)
library(DT)
library(plotly)
library(caret)
library(dygraphs)
library(xts)         
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(plotly)
library(corrplot)
library(RColorBrewer)
d=read_excel('donnees_Electricite.xlsx',sheet = 2)
Date=paste(d$annee,d$mois,d$jour,sep = '-')
d$Date=as.Date(Date)

shinyServer(function(input, output) {
            
output$table = DT::renderDataTable({
  
    DT::datatable(d,options = list(scrollX = TRUE))
    
})
output$sum <-renderPrint({
  f=d
  for ( i in 9:29) { f[[i]] <- as.factor (f[[i]]) }
  summary(f)
})          

output$tb<- renderUI({
    tabsetPanel(
      
      tabPanel("Dataset",br(), DT::dataTableOutput("table")),
      tabPanel("Summary",br(),verbatimTextOutput("sum"))) })            

output$plot=renderDygraph({
  
  Variab<- ts(d[[input$Var]],start = c(2010, as.numeric(format(d$Date[1], "%j"))),
              frequency = 365)
  
  don <- xts(x =Variab, order.by = d$Date)
  
  dyg=dygraph(don) %>%
    dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
    dyRangeSelector() %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
    dyRoller(rollPeriod = 1)
  dyg
  
})

output$plot2=renderPlot({
  
  Variab<- ts(d[[input$Var]],start = c(2010, as.numeric(format(d$Date[1], "%j"))),
              frequency = 365)
  decompose(Variab) %>% autoplot+theme_bw()
  
})

output$plot3=renderPlotly({
  
 p= plot_ly(x = d[[input$Var3]], y = d[[8]],type = 'scatter', 
            mode   = 'markers',
               marker = list(size = 10,
                             color = '#80bfff',#ccccff
                             line = list(color = '#3399ff',
                                         width = 2))) %>%
    layout(title = 'Styled Scatter',
           yaxis = list(zeroline = FALSE),
           xaxis = list(zeroline = FALSE))
  
  p
  
  
})

output$down<- downloadHandler(
  filename =  function() {
    paste("Indkmeans.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(
      plot_ly(x = d[[input$Var3]], y = d[[8]],type = 'scatter', 
              mode   = 'markers',
              marker = list(size = 10,
                            color = '#80bfff',#ccccff
                            line = list(color = '#3399ff',
                                        width = 2))) %>%
        layout(title = 'Styled Scatter',
               yaxis = list(zeroline = FALSE),
               xaxis = list(zeroline = FALSE))
    )
    dev.off()  # turn the device off
  }
)



output$ttt= renderPlot({
  
  M=cor(d[,5:8])
  corrplot(M, type="upper", method='pie')
  
}) 



output$text=renderPrint({
  
  
  formule = paste("Energie_trans",'~',paste(c(input$Var4,input$Var5), collapse= "+"))
  a = unlist(input$nb)
  a = unlist(input$nb)
  b=2922-a
  train <- d[1:b, ] 
  train=ts(train,start = 2010,frequency = 365)
 fit <- tslm(formula(formule), data = train)
  summary(fit)
})
output$plot4=renderPlotly({
  formule = paste("Energie_trans",'~',paste(c(input$Var4,input$Var5), collapse= "+"))
  a = unlist(input$nb)
  b=2922-a
  train1 <- d[1:b, ] #excluding the last two years
  test <-  d[b+1:2922, ]
  
  train=ts(train1,start = 2010, frequency = 365)
   fit <- tslm(formula(formule), data = train)
  Energie_trans<-train1$Energie_trans
  
  fore<-forecast(fit,test)
  plot_ly() %>%
    add_lines(x = (d)$Date , y = (d)$Energie_trans,
              color = I('#595959') , name = "observed") %>%
    add_ribbons(x = (test)$Date, ymin = fore$lower[, 2], ymax = fore$upper[, 2],
                color = I("#b3b3ff"), name = "95% confidence") %>%
    add_lines(x = (test)$Date, y = fore$mean, color = I("#0073e6"), name = "prediction")
  
  
  
  
})
output$text2=renderPrint({
  
  
  formule = paste("Energie_trans",'~',paste(c(input$Var4,input$Var5), collapse= "+"))
  a = unlist(input$nb)
  b=2922-a
  train <- d[1:b, ] #excluding the last two years
  train=ts(train,start = 2010,frequency = 365)
  fit <- tslm(formula(formule), data = train)
  accuracy(fit)
})




 })



    