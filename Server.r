library(shiny)
library(shinyapps)
library(ggplot2)
library(directlabels)
library(RCurl)

#setwd("D:/Users/FRAENG/Documents/2 Projects/2014 Data Analysis/09_DevelopingDataProducts/shiny/franksenhancedcourseproject")

# NOTE: possible to manually change start and end dates in URL 
#SAP <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=SAP.DE&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv", ssl.verifypeer = FALSE)
#SOW <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=SOW.DE&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")
#ORA <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=ORCL&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")
#IBM <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=IBM&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")
#MSFT <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=MSFT&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")
#HP <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=HPQ&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")
#ACN <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=ACN&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")
#NASDAQ <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=%5ENDX&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")
#TECDAX <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=%5ETECDAX&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")
#DAX <- getURL("http://real-chart.finance.yahoo.com/table.csv?s=%5EGDAXI&a=00&b=1&c=2014&d=11&e=31&f=2016&g=d&ignore=.csv")


#x <- getURL("https://raw.githubusercontent.com/geekbert/DevelopingDataProducts/master/WeeklyStockPrices2014.csv", ssl.verifypeer = FALSE)
#y <- read.csv(text = x) 
#y$DATE <- as.Date(y$DATE, format = "%m/%d/%Y")


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

# y days trendanalysis
y = 245 # until December 20 # some have max of 245 obs, screws up diff formula 
# future to do: replace with which.min function

division <- function(x) {
  x/x[y] -1
} 

SOW <- SOW[1:y, ]; SOW$diff <- division(SOW$Adj.Close)
SAP <- SAP[1:y, ]; SAP$diff <- division(SAP$Adj.Close)
ORA <- ORA[1:y, ]; ORA$diff <- division(ORA$Adj.Close)
IBM <- IBM[1:y, ]; IBM$diff <- division(IBM$Adj.Close)
MSFT <- MSFT[1:y, ]; MSFT$diff <- division(MSFT$Adj.Close)
HP <- HP[1:y, ]; HP$diff <- division(HP$Adj.Close)


df <- rbind(SOW,SAP,ORA,IBM,MSFT,HP) 
colnames(df) <- tolower(colnames(df))
colnames(df) <- gsub("[.]","", colnames(df)) # important syntax 
#y$Date <- as.Date(y$Date, format = "%m/%d/%Y")
df$date <- as.Date(df$date)


dataset <-  df
 
 
shinyServer(function(input, output) {
	
	#output$text1 <- renderText({paste("You have selected", input$SYMBOL)})
 	
	output$plot <- renderPlot({   
	    
	dataset <- dataset[dataset$symbol  %in% paste(input$symbol), ] # I HAD big trouble subsetting df by multiple factors 
	
	# subset by date:  dataset[dataset$DATE <= "2014-10-13" & dataset$DATE >= "2014-09-22", ]
	dataset <- dataset[dataset$date <= paste(input$DATERANGE[2]) & dataset$date >= paste(input$DATERANGE[1]), ]   
	#dataset <- dataset[dataset$DATE <= paste(input$DATERANGE), ] # this works 
	#print(str(dataset)) 
	p <- ggplot(data=dataset, aes(x=date, y=diff, colour=symbol)) + geom_line(aes(group=symbol)) 
	p <- direct.label(ggplot(data=dataset, aes(x=date, y=diff, colour=symbol)) + geom_line())
	print(p)
   
    
  }, height=700)
  
})


