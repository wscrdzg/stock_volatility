# stock history loaded from yahoo finance, author of these functions does not take |
# responsibilty if any information is missing or unaccurate.

load_stock_history <- function(ticker = "SPY",start_year = 1950,end_year = 2017) {
  download_address <- paste("https://query1.finance.yahoo.com/v7/finance/download/",ticker,"?period1=1521796767&period2=1524475167&interval=1d&events=history&crumb=HeJRqgGl5Ca",sep="")
  download.file(download_address,destfile = paste(ticker,"_hist_data.csv",sep=""))
  data <- read.csv(paste(ticker,"_hist_data.csv",sep=""))
  data
}

library(lubridate)
# then calculate stock sd

stock_sd <- function(ticker){
  data <- load_stock_history(ticker = ticker)
  # adjust time format
  data$Date <- ymd(data$Date)
  c_year <- year(data$Date[1]) # current year
  count <- 0
  con <- 0
  
  #length of data
  length <- length(data$Date)
  
  # first get expected return (I here use end - begin, and calculate )
  for(i in 1:(length-1)) {
    count <- ifelse(month(data$Date[i]) - month(data$Date[i+1]) != 0, 
                    count + 1, count)
  }
  total_months <- count + 1 # last month did not count, so +1
  total_return <- (data$Adj.Close[1] - data$Adj.Close[length]) / data$Adj.Close[length]
  monthly_return <- (total_return+1)^(1/total_months) - 1
  print(paste("expected monthly return calculated from total return is:", monthly_return *100, "%"))
  print(paste("total number of month available in data: ", total_months))
  
  # loop each month to compare with the expected monthly return
  data$year <- year(data$Date)
  data$month <- month(data$Date)
  how_many_year <- c_year - data$year[length] + 1
  return_by_month <- vector(length = total_months)
  for(i in 1:how_many_year){
    data1 <- subset(data, year == (c_year + 1 - i))
    month_number <- data1$month[1] - data1$month[length(data1$month)] + 1
    for(m in 1:month_number) {
      data2 <- subset(data1, month == data1$month[1] + 1 - m)
      con <- con + 1
      return_by_month[con] <- (data2$Adj.Close[1] - data2$Adj.Close[length(data2$Close)]) / data2$Adj.Close[length(data2$Close)]
    }
  }
  print(paste("mean of monthly return: ", mean(return_by_month)*100, "%"))
  print(paste("SD of monthly return: ", sd(return_by_month)*100, "%"))
}
