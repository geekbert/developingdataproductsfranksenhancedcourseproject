library(shiny)
library(shinyapps)
library(ggplot2)
library(directlabels)
library(RCurl)

#setwd("D:/Users/FRAENG/Documents/2 Projects/2014 Data Analysis/09_DevelopingDataProducts/shiny/franksenhancedcourseproject")

SAP <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=SAP.DE&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv", ssl.verifypeer = FALSE)
SOW <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=SOW.DE&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")
ORA <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=ORCL&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")
IBM <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=IBM&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")
MSFT <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=MSFT&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")
HP <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=HPQ&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")

# future to do: connect date ranges as inputs into getURL as opposed to hardcoding them 

SOW <- read.csv(text = SOW); SAP <- read.csv(text = SAP); 
ORA <- read.csv(text = ORA); IBM <- read.csv(text = IBM); 
MSFT <- read.csv(text = MSFT); HP <- read.csv(text = HP); 

SOW <- cbind(symbol = "SOW", SOW); SAP <- cbind(symbol = "SAP", SAP); 
ORA <- cbind(symbol = "ORA", ORA); IBM <- cbind(symbol = "IBM", IBM); 
MSFT <- cbind(symbol = "MSFT", MSFT); HP <- cbind(symbol = "HP", HP);  

# y day trendanalysis
y = 245 # until December 20, 2014 # problem: some have max of 245 observations, screws up diff formula
# future to do: replace with which.min function

division <- function(x) {
  x/x[y] -1
} 

SOW <-SOW[1:y, ]; SOW$diff <- division(SOW$Adj.Close)
SAP <-SAP[1:y, ]; SAP$diff <- division(SAP$Adj.Close)
ORA <-ORA[1:y, ]; ORA$diff <- division(ORA$Adj.Close)
IBM <-IBM[1:y, ]; IBM$diff <- division(IBM$Adj.Close)
MSFT <- MSFT[1:y, ]; MSFT$diff <- division(MSFT$Adj.Close)
HP <- HP[1:y, ]; HP$diff <- division(HP$Adj.Close)


df <- rbind(SOW,SAP,ORA,IBM,MSFT,HP) 
colnames(df) <- tolower(colnames(df))
colnames(df) <- gsub("[.]","", colnames(df)) # important syntax 
#y$Date <- as.Date(y$Date, format = "%m/%d/%Y")
df$date <- as.Date(df$date)


dataset <-  df
 
shinyUI(pageWithSidebar(
 
  headerPanel("Stock Price Trends in Enterprise Software Industry"),
  
  sidebarPanel(
	h3('% change of select stock prices since 1/1/2014'), 
	helpText("Check or uncheck stocks, change end date"),
	
	checkboxGroupInput("symbol", label = h4("Select Stocks:"), c("Software AG (SOW)"="SOW","SAP"="SAP","Oracle (ORA)"="ORA", "IBM"="IBM","Microsoft (MSFT)"="MSFT", "Hewlett-Packard (HP)"="HP"), selected=c("SOW"="SOW","SAP"="SAP","ORA"="ORA", "IBM"="IBM", "MSFT"="MSFT","HP"="HP")), 
	#checkboxGroupInput("SYMBOL", "Names", c(levels(dataset$SYMBOL))), selected=c(levels(dataset$SYMBOL)))), 
		
	dateRangeInput("DATERANGE", label = h4("Select Date range:"), start  = "2014-01-01", end = "2014-12-31")  
			

  ),

  mainPanel(
	#h4('Trend Chart'),	 
	#textOutput("text1"), 
	#textOutput("text2"), 

    plotOutput('plot')
  )
))  

# to test, type "runApp()" into console. Don't use as part of ui.R or server.R script 
# to deploy on shinyapps.io: deployApp(). For prerequisites, see mpeg in shiny folder 
# in case of error: shinyapps package out of date. To update visit: https://github.com/rstudio/shinyapps
# devtools::install_github("rstudio/shinyapps") # if (!require("devtools")) # install.packages("devtools")
# note: deploying app gives it automatically the name of windows folder, e.g. franksenhancedcourseproject





