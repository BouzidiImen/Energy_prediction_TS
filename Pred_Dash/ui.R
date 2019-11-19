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
library(readxl)
d=read_excel('donnees_Electricite.xlsx',sheet = 2)


shinyUI(dashboardPage(
    
    
    dashboardHeader(title = 'Electrical Energy'),
    ########## sider panel ####
    dashboardSidebar( width = 250,
                      sidebarMenu(
                          menuItem("Visualize Data", tabName = "VD", icon = icon("boxes")),
                          
                          menuItem("Exploratory Analysis", tabName = "DA", icon = icon("connectdevelop"),
                                   startExpanded = T,
                                   menuSubItem("Univariate Analysis", tabName = "UA", icon = icon("boxes")),
                                   menuSubItem("Bivariate analysis", tabName = "BA", icon = icon("megaport"))
                          ),
                          menuItem("Prediction", tabName = "ML", icon = icon("slideshare"))
                          
                          
                          
                      )),
    dashboardBody(
        tabItems(
            
        #####Vizualise Data #########
            
        tabItem(
            tabName = 'VD', uiOutput('tb'),
            
            tags$div(id="cite",
                             '\U00A9 Imen Bouzidi'
            )
            
            #tabPanel("Summary",br(), verbatimTextOutput("sum"))
            
        ),
        
        
            ########## Univariate analysis ######
            tabItem(  tabName = "UA",
                      
                      box( status = "primary",  width = 12,
                                      tabsetPanel(
                                          selected = "Plot",
                                          br(),
                                         
                                              selectInput("Var", "Choose a variable :",width = '200px',
                                                          colnames(d[,5:8]),selected = "Energie_trans"),
                                          
                                        
                            
                                          tabPanel("Plot",br(),dygraphOutput("plot"),downloadButton(outputId = "down", label = "Download the plot")),
                                          tabPanel("Decomposition",br(),plotOutput('plot2'))
                                      )),
                                 tags$head(tags$style(" #cite {
                                      color: #949793;
                                      position: absolute;
                                      bottom: 10px;
                                      right: 10px;
                                      font-size: 12px;
                                      }
                                      ")),
                      
                      tags$div(id="cite",
                               '\U00A9 Imen Bouzidi'
                      )),
            ########## Bivariate analysis ########
            tabItem(
                tabName = "BA",
                
                fluidRow( 
                box( status = "primary",  width = 8 ,
                     tabsetPanel(
                         
                         selected = "Plot",
                         
                         
                         tabPanel("Plot",
                                  br(),
                                  
                                  selectInput("Var3", "Choose a variable :",width = '200px',
                                              colnames(d[,5:7]),selected = "tmin"),
                                  
                                  
                                  br(),plotlyOutput('plot3'),br(),
                                  downloadButton(outputId = "down", label = "Download the plot")),
                         tabPanel("Correlation",br(),
                                  br(),
                                  
                                  plotOutput('ttt'))
                )
                
                
               ),
               tags$div(id="cite",
                        '\U00A9 Imen Bouzidi'
               )
                
                
            )),
            ########## Machine learning#####################
            tabItem(tabName = "ML",
                    fluidRow(  box( title = "Choose the variables and the method: ", solidHeader = T,status = "primary",  width = 4 , 
                                    selectInput("Var4", "Choose model's features:",multiple = T
                                                ,choices =names(d[,c(1,9:29)]),selected =names(d[,c(1,9:29)]) ),
                                    
                                    selectInput("Var5", "Choose which temperature to iclude:",multiple = F
                                                ,choices =names(d[,5:7]),selected ='Tmoy'),
                                    
                                    
                                    sliderInput("nb", "Number of days of prediction:",
                                                 min = 1, max =365 , value = 365)
                                    
                    ),
                    box(status = "primary",  width = 8,
                        tabsetPanel(
                            
                            selected = "Plot",
                            
                            
                            tabPanel("Prediction Plot",plotlyOutput('plot4')),
                            tabPanel("Summary of fit",br(),verbatimTextOutput('text')),
                            
                            tabPanel("Accuracy",br(),verbatimTextOutput('text2'))
                            
                        )
                    )
                    
                    
                    ),
                    tags$div(id="cite",
                             '\U00A9 Imen Bouzidi'
                    )
                    
                    
                    
            )
        
        
        
    
    
    
    
    
    
    
)


)
))



