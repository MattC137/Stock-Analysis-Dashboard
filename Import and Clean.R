library(rvest)
library(dplyr)
library(readr)
library(tidyr)

#install.packages("rvest")

setwd("~/Workspace/Stock Analyser")

## Import Tickers

sp500_url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

sp500 <- read_html(sp500_url) %>% 
  html_node("table") %>% 
  html_table()

sp500 <- sp500 %>% select(Symbol, Security, `GICS Sector`, `GICS Sub Industry`, `Headquarters Location`)
names(sp500) <- c("Ticker", "Name", "Sector", "Industry", "HQ_Location")

save(sp500, file = "sp500.RData")

## Import Price Data

aapl <- read_csv("https://query1.finance.yahoo.com/v7/finance/download/AAPL?period1=1280102400&period2=1595721600&interval=1d&events=history")
msft <- read_csv("https://query1.finance.yahoo.com/v7/finance/download/MSFT?period1=1280102400&period2=1595721600&interval=1d&events=history")
mode(aapl)

BRK.B <- try(read_csv("https://query1.finance.yahoo.com/v7/finance/download/BRK.B?period1=1280102400&period2=1595721600&interval=1d&events=history"))
mode(BRK.B)

returns <- as.data.frame(matrix(NA, ncol = 8, nrow = 0))
names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adj_Close", "Volumen", "Ticker")

for(symbol in sp500$Ticker){
  print(symbol)
  url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/", symbol, "?period1=1280102400&period2=1595721600&interval=1d&events=history")
  print(url)
  
  ret <- try(read_csv(url))
  
  if(mode(ret) != "character"){
    ret$Ticker <- symbol
    returns <- rbind(returns, ret)
  }
}

names(returns) <- c("Date", "Open", "High", "Low", "Close", "Adj_Close", "Volume", "Ticker")

returns <- returns %>% select("Date", "Ticker", "Open", "High", "Low", "Close")

returns <- returns %>% mutate(
  Open = as.numeric(Open),
  High = as.numeric(High),
  Low = as.numeric(Low),
  Close = as.numeric(Close),
)

returns <- returns %>% mutate(
  Movement = ifelse(Close > Open, "Up", "Down")
)

save(returns, file = "returns.RData")

returns_long <- returns %>% gather("Series", "Value", -Date, -Ticker, -Movement)
returns_long <- returns_long %>% left_join(sp500 %>% select(Ticker, Name, Sector, Industry), by = c("Ticker" = "Ticker"))

save(returns_long, file = "returns_long.RData")

## Performance calcs

performance_summary <- as.data.frame(matrix(NA, ncol = 7, nrow = 0))
names(performance_summary) <- c("Ticker", "Thirty_days", "Ninety_days", "One_year", "Three_years", "Five_years", "Ten_years")

i <- 1
for(ticker in unique(returns_long$Ticker)){
  print(ticker)
  
  returns_long_by_ticker <- returns_long %>% filter(Ticker == ticker, Series == "Close") %>% arrange(desc(Date))
  
  thrity_day <- (returns_long_by_ticker$Value[1] - returns_long_by_ticker$Value[21])/returns_long_by_ticker$Value[21]
  ninety_day <- (returns_long_by_ticker$Value[1] - returns_long_by_ticker$Value[63])/returns_long_by_ticker$Value[63]
  one_year <- (returns_long_by_ticker$Value[1] - returns_long_by_ticker$Value[253])/returns_long_by_ticker$Value[253]
  three_year <- (1 + ((returns_long_by_ticker$Value[1] - returns_long_by_ticker$Value[759])/returns_long_by_ticker$Value[759]))^(1/3)-1
  five_year <- (1 + ((returns_long_by_ticker$Value[1] - returns_long_by_ticker$Value[1265])/returns_long_by_ticker$Value[1265]))^(1/5)-1
  ten_year <- (1 + ((returns_long_by_ticker$Value[1] - returns_long_by_ticker$Value[2518])/returns_long_by_ticker$Value[2518]))^(1/10)-1
  
  performance_summary[i, 1] <- ticker
  performance_summary[i, 2] <- thrity_day
  performance_summary[i, 3] <- ninety_day
  performance_summary[i, 4] <- one_year
  performance_summary[i, 5] <- three_year
  performance_summary[i, 6] <- five_year
  performance_summary[i, 7] <- ten_year
  
  i <- i + 1
}

load("sp500.RData")

performance_summary <- performance_summary %>% left_join(sp500, by = c("Ticker" = "Ticker"))
save(performance_summary, file = "performance_summary.RData")
