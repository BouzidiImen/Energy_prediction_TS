library(dygraphs)
library(xts)         
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(plotly)
setwd(getwd())
D=as.data.frame(read.csv('Pred_Dash/DATA.csv'))
set.seed(25)
#######  Exploratory data analysis 
Date=paste(D$annee,D$mois,D$jour,sep = '-')
D$Date=as.Date(Date)

##### Correlation of temperature#########
t.test(D$tmin,D$Tmax)
fix(D)
p <- plot_ly(data = D, x = ~Tmoy, y = ~Energie_trans,
             marker = list(size = 10,
                           color = 'rgba(255, 182, 193, .9)',
                           line = list(color = 'rgba(152, 0, 0, .8)',
                                       width = 2))) %>%
  layout(title = 'Styled Scatter',
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))

p
t.test(D$tmin,D$Tmoy)
p <- plot_ly(data = D, x = ~tmin, y = ~Energie_trans,
             marker = list(size = 10,
                           color = 'rgba(255, 182, 193, .9)',
                           line = list(color = 'rgba(152, 0, 0, .8)',
                                       width = 2))) %>%
  layout(title = 'Styled Scatter',
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))

p
t.test(D$Tmax,as.numeric(D$Tmoy))
p <- plot_ly(data = D, x = ~Energie_trans, y = ~Tmax,
             marker = list(size = 10,
                           color = 'rgba(255, 182, 193, .9)',
                           line = list(color = 'rgba(152, 0, 0, .8)',
                                       width = 2))) %>%
  layout(title = 'Styled Scatter',
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))

p







## Create a time series object#####

Energ <- ts(D$Energie_trans,start = c(2010, as.numeric(format(D$Date[1], "%j"))),
            frequency = 365)

Tm <- ts(D$Tmoy, start = c(2010, as.numeric(format(D$Date[1], "%j"))),
         frequency = 365)

D$Energ=Energ
D$TM=Tm

## Visualise The whole Data
don <- xts(x = D$Energ, order.by = D$Date)
p <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

p

plot.ts(D$tmin)
plot.ts(D$Tmax)
plot.ts(D$Tmoy)

#Decomposition
decompose(Energ) %>% autoplot+theme_bw()

decompose(D$TM) %>% autoplot
train <- D[1:2192,] #excluding the last two years
test <-  D[2193:2922,]
don <- xts(x = train$Energ, order.by = train$Date)
p <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

p
don <- xts(x = test$Energ, order.by = test$Date)
p <- dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

p
train =ts(train,start = c(2010, as.numeric(format(train$Date[1], "%j"))),
   frequency = 365)
test =ts(test,start = c(2010, as.numeric(format(test$Date[1], "%j"))),
          frequency = 365)

fit <- tslm(Energ ~ trend+Lundi+Vendredi+season, data = train)
accuracy(fit)
summary(fit)
forc=forecast(fit,newdata = test, h=584.4)# does't work

View(test)





