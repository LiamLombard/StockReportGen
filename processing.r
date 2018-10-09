library(flexdashboard)
library(prophet)
library(dygraphs)
library(plotly)
library(tseries)
library(quantmod)
library(ggplot2)
library(dplyr)
library(zoo)

boxPlot <- function(data, x)
{
  plot <- ggplot(data) +
  geom_boxplot(aes(x="", y=x), size=0.3)+
  xlab("") +
  coord_flip() +
  coord_cartesian(ylim = boxplot.stats(x)$stats[c(1, 5)]*1.1) +
  ylab("Price (USD)")

  return(plot)
}

tripleBoxPlot <- function(data)
{
  p <- boxPlot(data, data$Close)
  p2 <- boxPlot(data, data$High - data$Low)
  p3 <- boxPlot(data, data$Close - data$Open)

  subplot(ggplotly(p), ggplotly(p2),ggplotly(p3))
}

timePlot <- function(data, x, y)
{
  plot <- ggplot(data) +
  geom_line(aes(x, y), size=0.3)+
  xlab("Year") +
  ylab("Price (USD)")

  ggplotly(plot)
}

quarterPlot <- function(datazoo)
{
  tempdata <- as.data.frame(datazoo)
  tempdata$dates <- as.Date(time(datazoo)) 
  tempdata$quarter <- quarters(tempdata$date)

  plot <-ggplot(tempdata, aes(dates, Close, colour = quarter)) +
  geom_line(size=0.3)+
  xlab("Year") +
  ylab("Price (USD)")

  ggplotly(plot)
}

prophetForecast <- function(dates, values)
{
  model <- data.frame(dates, values)
  colnames(model) <- c("ds", "y")
  fitModel <- prophet(model)
  future <- make_future_dataframe(fitModel, periods = 30)
  forecast <- predict(fitModel, future)
  dyplot.prophet(fitModel, forecast)
}

forecastForecast<- function(dates, values)
{
  model <- data.frame(dates, values)
  colnames(model) <- c("ds", "y")
  fitModel <- prophet(model)
  future <- make_future_dataframe(fitModel, periods = 30)
  forecast <- predict(fitModel, future)
  dyplot.prophet(fitModel, forecast)
}