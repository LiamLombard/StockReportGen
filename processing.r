library(tseries)
library(ggplot2)
library(dplyr)
library(zoo)

settings <- read.csv("settings.csv", header=TRUE, sep=",")

datazoo <- get.hist.quote(paste(settings$stockname), quiet=FALSE)
data <- as.data.frame(datazoo)
times <- as.Date(time(datazoo))
figureWidth <- 7
figureHeight <- 2

boxPlot <- function(data, x)
{
  ggplot(data) +
  geom_boxplot(aes(x="", y=x), size=0.3)+
  xlab("") +
  coord_flip() +
  coord_cartesian(ylim = boxplot.stats(x)$stats[c(1, 5)]*1.1) +
  ylab("Price (USD)")
}

timePlot <- function(data, x, y)
{
  ggplot(data) +
  geom_line(aes(x, y), size=0.3)+
  xlab("Year") +
  ylab("Price (USD)")
}

quarterPlot <- function(datazoo)
{
  tempdata <- as.data.frame(datazoo)
  tempdata$dates <- as.Date(time(datazoo)) 
  tempdata$quarter <- quarters(tempdata$date)

  ggplot(tempdata, aes(dates, Close, colour = quarter)) +
  geom_line(size=0.3)+
  xlab("Year") +
  ylab("Price (USD)")
}

tableGen <-function(data, caption)
{
  Data <- c(mean(data), sd(data))
  thistable <- data.frame(Data)
  thistable <- cbind(Stats = c("Mean", "SD"), thistable)
  knitr::kable(t(thistable), caption=caption)
}