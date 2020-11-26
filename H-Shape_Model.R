library(quantmod)
library(dplyr)
library(RMySQL)

# Query price historical data from database
# Connect to the database
StocksDB <- dbConnect(MySQL(), dbname = "Stocks(TW)", username="root", password="")
# Query data of which dates are after 2015
data <- dbGetQuery(StocksDB ,"select * from History_Prices where date > '2015-01-01'")
colnames(data) <- 
  c("Date","High","Close","Volume","Low","Open","AdjClose","Id")
# Load the industries of stocks
stock_cat <- read.csv('/Users/chenlyn/Desktop/Quantative Analyst/Project/Stock Price Database/Stocks_Categories.csv')stock_cat$Id <- as.character(stock_cat$Id)

 
# Find the buy point which meet the strategy qualifications
# Create the buying function
H_shape_buy <- function(data=data, StockId){
  stock <- data %>% filter(Id == StockId)
  stock$Date <- as.Date(stock$Date)
  # Remove volume == 0
  stock <- stock %>% filter(Volume != 0)
  # Calculate candle lengths,  t-1 and t-2 close and open
  stock <- stock %>% mutate(
    # t-1 day close
    lagClose1 = lag(Close, 1),
    # t-2 day close
    lagClose2 = lag(Close, 2),
    # t-1 day open
    lagOpen1 = lag(Open, 1),
    # t-2 day open
    lagOpen2 = lag(Open, 2),
    # t day - candle length
    candleValue = abs(Close / Open-1),
    # t-1 day - candle length
    lagcandleValue1 = lag(candleValue, 1),
    # t-2 dat - candle length
    lagcandleValue2 = lag(candleValue, 2),
  )
  # Find H shape candlestick plot 
  fitStrategy <- stock %>% filter(
    # t day Close > t day Open
    Close > Open,
    # t-2 day Close > t-2 day Open
    lagClose2 > lagOpen2,
    # t day - candle length > 1%
    candleValue > 0.01,
    # t-1 day - candle length < 0.5%
    lagcandleValue1 < 0.005,
    # t-2 day - candle length > 1%
    lagcandleValue2 > 0.01
  ) %>% select(BuyDate = Date,BuyPrice = Close, Id = Id)
  # Return the stock data and buying points 
  buyStock <- list(Stock = stock, Buy = fitStrategy)
  return(buyStock)
}

# Find the selling points
# Create the finding selling points function
# Function parameters including different moving average functions and different periods to average over
sellStock <- function(Stock, MAday = 20, MAFUN = SMA){
  sellStock <- Stock %>% 
    mutate(
      # Calculate t day MA
      MA = MAFUN(Close, MAday),
      # Calculate t-1 day MA
      lagMA = lag(MA, 1)) %>%
    # When close is less than MA, sell the stock 
    filter(
      # t day Close < t day MA
      Close < MA,
      # t-1 day Close > t-1 day MA
      lagClose1 > MA) %>%
    select(SellDate = Date, SellPrice = Close)
  return(sellStock)
}

# Combine the buying points and selling points to create trading tables
TradingRecord <- function(fitStrategy, SellPoint){
  TradingTable <- NULL
  for (i in 1:nrow(fitStrategy)){
    # buy the stock
    inDate <- fitStrategy$BuyDate[i]
    # sell the stock
    outdate <- which(SellPoint$SellDate > inDate)[1]
    # Foolproof mechanism - if the buying point is at the end of data period, it can't find the selling point.
    if(length(outdate)>0){
      # Create trading table
      TradingTable <- rbind(TradingTable, cbind(fitStrategy[i,], SellPoint[outdate,]))
    }
    TradingTable <- TradingTable[complete.cases(TradingTable),]
  }
  return(TradingTable)
}

# Take the stock Id 2330 as the example
Buying_2330 <- H_shape_buy(data, 2330)
Selling_2330 <- sellStock(Buying_2330$Stock)
TradingTable <- TradingRecord(Buying_2330$Buy,Selling_2330)

# Create plot function
PlotGraph <- function(stock, TradingTable,plotSample = 5){
  # Load the stock data
  stock <- stock %>%
    mutate(
      MA5 = SMA(Close, 5),
      MA20 = SMA(Close, 20))
  # Find the buy point and sell point in the trading table 
  buyDate <- TradingTable$BuyDate[plotSample]
  sellDate <- TradingTable$SellDate[plotSample]
  # start date of the plot is 35 trading days before the buy point
  match <- which(stock$Date == buyDate)- 35
  plotStartDate <- stock$Date[ifelse(match <1, 1, match)]
  # end date of the plot is 35 trading days after the sell point
  match <- which(stock$Date == sellDate)+35
  plotEndDate <- stock$Date[ifelse(match > nrow(stock), nrow(stock), match)]
  # Select the data meeting the holding stock date range
  plotData <- stock[which((stock$Date >= plotStartDate)&(stock$Date <= plotEndDate)),]
  plotData <- plotData %>% select(Date:Open, MA5:MA20)
  # Find the buying point
  plotData$buy <- rep(NA, nrow(plotData))
  plotData$buy[which(plotData$Date == buyDate)] <- plotData$Open[which(plotData$Date == buyDate)]*0.97
  # Ｆind the selling point
  plotData$sell <- rep(NA, nrow(plotData))
  plotData$sell[which(plotData$Date == sellDate)] <- plotData$Close[which(plotData$Date == sellDate)]*1.03
  # Convert the data format into xts
  plotData <- xts(plotData[,-1], order.by = plotData$Date)
  # Set the color of candlestick chart
  myTheme <- chart_theme()
  myTheme$col$dn.col <- c('firebrick3') 
  myTheme$col$up.col <- c('chartreuse3')
  # Draw the candlestick chart
  pic <- chart_Series(x=plotData[,1:6], name = paste('2330', 'Technical Analysis Plot',sep =' '), theme = myTheme)
  # Add Volume
  pic <- add_Vo()
  # Add line plot of 5 day moving average of price
  pic <- add_TA(x=plotData$MA5, on=1, type= 'l', col = 'blue', lwd = 1.5)
  # Add line plot of 20 day moving average of price
  pic <- add_TA(x = plotData$MA20, on = 1, type = 'l', col = 'orange', lwd = 1.5)
  # Mark the buying point
  pic <- add_TA(x = plotData$buy, on = 1, type = 'p', col='red', pch = 2, cex = 5, lwd = 1.5)
  # Mark the selling point
  pic <- add_TA(x = plotData$sell, on = 1, type = 'p', col = 'green', pch = 6, cex =5, lwd =1.5)
  return(pic)
}
# Call the plot function
PlotGraph(Buying_2330$Stock, TradingTable,2)


# Calculate the performance
TradingPerformance <- function(TradingTable){
  # Fee and taxes
  buyCost <- 0.001425
  sellCost <- 0.004425 
  TradingTable <- TradingTable %>% mutate(
    netProfit = SellPrice*(1-sellCost)*1000 - BuyPrice*(1+buyCost)*1000,
    # the rate of investment(ROI)
    ret = ((SellPrice*(1-sellCost)) / (BuyPrice*(1+buyCost)) -1)*100,
    holdDays =  as.numeric(SellDate - BuyDate),
    profitPerDay = ((SellPrice*(1-sellCost))- (BuyPrice*(1+buyCost)))*1000 / holdDays
  )
  return(TradingTable)
}
# Integrate all function into one function 
H_shape_performance <- function(StockID, MAday=20, MAFUN = SMA){
  tryCatch({
    BuyIn <- H_shape_buy(data, StockID)
    SellOut <- sellStock(BuyIn$Stock, MAday, MAFUN)
    Trade <- TradingRecord(BuyIn$Buy, SellOut)
    performance <- TradingPerformance(Trade)
    return(performance)},
    error = function(e){return(NULL)}
  )
}
# Apply to all Taiwan Stock and compare the different periods to average over
StockID <- unique(data$Id)
H_shape_SMA5 <- do.call(rbind, lapply(StockID,H_shape_performance, MAday =5))
H_shape_SMA5$MAdays <- 5
H_shape_SMA10 <- do.call(rbind, lapply(StockID,H_shape_performance, MAday =10))
H_shape_SMA10$MAdays <- 10
H_shape_SMA15 <- do.call(rbind, lapply(StockID,H_shape_performance, MAday =15))
H_shape_SMA15$MAdays <- 15
H_shape_SMA20 <- do.call(rbind, lapply(StockID,H_shape_performance, MAday =20))
H_shape_SMA20$MAdays <- 20
H_shape_SMA40 <- do.call(rbind, lapply(StockID,H_shape_performance, MAday =40))
H_shape_SMA40$MAdays <- 40
H_shape_SMA60 <- do.call(rbind, lapply(StockID,H_shape_performance, MAday =60))
H_shape_SMA60$MAdays <- 60
H_shape_SMA80 <- do.call(rbind, lapply(StockID,H_shape_performance, MAday =80))
H_shape_SMA80$MAdays <- 80
H_shape_SMA100 <- do.call(rbind, lapply(StockID,H_shape_performance, MAday =100))
H_shape_SMA100$MAdays <- 100
H_shape_SMA120 <- do.call(rbind, lapply(StockID,H_shape_performance, MAday =120))
H_shape_SMA120$MAdays <- 120
SMA_Stock <- rbind(H_shape_SMA5, H_shape_SMA10, H_shape_SMA15, H_shape_SMA20, 
                   H_shape_SMA40, H_shape_SMA60, H_shape_SMA80, H_shape_SMA100, H_shape_SMA120)
# Left join with industries information
SMA_Stock <- left_join(SMA_Stock, stock_cat) 
SMA_Stock <- SMA_Stock %>% mutate(Year = ifelse(BuyDate > '2017-01-01', 'After 2017', 'After 2015'))

# The overall winRatio, ROI, EV(stock price), EROI(expected ROI)
Overall_Performance <- SMA_Stock %>% 
  summarise(WinRatio = sum(ifelse(netProfit >0, 1, 0))/n(), Mean_netProfit = mean(netProfit),
            Mean_ROI = mean(ret)*100,  EV = WinRatio*Mean_netProfit, EROI = WinRatio*Mean_ROI)

# Compare the performances on different periods of moving average over and industries   
Performance <- SMA_Stock %>% inner_join(stock_cat)%>% group_by(MainCategory, SubCategory, MAdays, Year) %>%
  summarise(WinRatio = sum(ifelse(netProfit >0, 1, 0))/n(), Mean_netProfit = mean(netProfit),
            Mean_ROI = mean(ret)*100,  EV = WinRatio*Mean_netProfit, EROI = WinRatio*Mean_ROI, number = n())

anova <- aov(WinRatio ~ MAdays+MainCategory+Year+
              MAdays:MainCategory+MainCategory:Year, data = Performance)
summary(anova)

# Selling points with different periods of moving average can make win ratios have significant differences of means.
# Do pairwise t-test between different MAdays groups 
pairwise.t.test(Performance$WinRatio, Performance$MAdays)

Performance %>% group_by(MAdays) %>% summarise(WinRatio = mean(WinRatio))%>% 
  ggplot(aes(x = MAdays, y = WinRatio))+
  geom_bar(stat="identity")


# Do pairwise t-test between different industries
pairwise.t.test(Performance$WinRatio, Performance$MainCategory)

# Select the better stocks
# Applying selling points on 40 days' MA prices strategy
# Find the stocks whose ROI is better than 75% and win Ratio is better than 50% of the H shape trading in 2015~2016 and 2017~now separately
# The stock needs to meet the H shape requirements twice in both periods.
# Filter the stocks which perform well both in 2015~2016 and 2017~now
After_2015_Stock <- SMA_Stock %>% group_by(Id)%>% mutate(WinRatio = sum(ifelse(netProfit >0, 1, 0))/n()) %>%
  filter(ret > 0, ret > quantile(SMA_Stock$ret, 0.75), MAdays == 40, Year == 'After 2015', WinRatio > quantile(Performance$WinRatio, 0.50)) %>% 
  group_by(Id) %>% count() %>% filter(n > 1) %>%  select(Id)%>% distinct()
After_2017_Stock <- SMA_Stock %>% group_by(Id)%>% mutate(WinRatio = sum(ifelse(netProfit >0, 1, 0))/n()) %>%
  filter(ret > 0, ret > quantile(SMA_Stock$ret, 0.75), MAdays == 40, Year == 'After 2017', WinRatio > quantile(Performance$WinRatio, 0.50)) %>% 
  group_by(Id) %>% count() %>% filter(n > 1) %>%  select(Id)%>% distinct()
Better_Stock <- rbind(After_2015_Stock, After_2017_Stock) %>% group_by(Id) %>% count() %>% filter(n == 2)

# Set today's signal
# Query the stock price in recent 3 days from the database
Recent_Prices <- dbGetQuery(StocksDB ,"select Date, high, close, open, volume, low, AdjClose, Id
                            FROM (SELECT *,
                            @cur := IF(id=@id, @cur+1, 1) AS ROW,
                            @id := id
                            FROM History_Prices CROSS JOIN (SELECT @id := (SELECT MAX(Id) FROM History_Prices), @cur := 0) AS init
                            WHERE date > '2020-05'
                            ORDER BY History_Prices.id, Date DESC) As t
                            WHERE ROW < 4")
dbDisconnect(StocksDB)
colnames(Recent_Prices) <- 
  c("Date","High","Close","Volume","Low","Open","AdjClose","Id")

# Filter out the data of better stock in H shape 
Recent_Prices <- Recent_Prices %>% filter(Id %in% Better_Stock$Id)
Better_Stock_Id <- unique(Recent_Prices)

# Today's signal - which meet the H shape requirements
Hshape_Buy_Signal <- lapply(Better_Stock_Id$Id,H_shape_buy, data = Recent_Prices)
Hshape_Buy_Signal  <-  do.call(rbind,sapply(temp1, `[`,'Buy'))
