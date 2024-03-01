library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
rm(list=ls())

load("stock_universe.rdata")
sectors <- read.csv("sectors.csv")

# STRATEGY DEFINITION:
# 1. Calculate MACD Line, its 9 day average (Signal Line), and MACD Histogram (MACD line - Signal line)
# 2. When MACD Line crosses 0 from 1 day to another and the MACD Histogram is above some threshold,
#   then on the next day we will either long or short that stock depending on if it crossed from + to - or - to +
#   if its + to -, then we will short the next day, if its - to + we will "go long" (buy at open sell at close)
# 3. I have assumed a max day trading amount of $10000 and no more than 12 trades per day
#   each day we spend the same amount on all trades defined as min(max day trade amount, equity / num trades on that day)

library(dplyr)
library(TTR)

threshold <- 0.01

# for each energy stock generate macd info
energy_stocks <- stock %>%
  inner_join(sectors, by = "symbol") %>%
  filter(sector == "Energy") %>%
  group_by(symbol) %>%
  
  mutate(# 12/26 day exponential moving avg (EMA)
         ema_12 = EMA(close, n = 12, wilder = TRUE),
         ema_26 = EMA(close, n = 26, wilder = TRUE),
         # macd line - difference between 12 and 26 day EMA
         macd_line = ema_12 - ema_26,
         # signal line - 9 day EMA of macd line
         signal_line = EMA(macd_line, n = 9, wilder = TRUE),
         # macd histogram for our trading signal threshold
         macd_histogram = macd_line - signal_line,
         trading_strength = abs(macd_histogram)) %>%
  
  mutate(
    # Shift MACD line values by 2 days ago
    macd_line_shifted = lag(macd_line, n = 2),
    # Generate trading signal based on MACD line crossing 0
    trading_signal = ifelse(
      macd_line_shifted < 0 & lag(macd_line) >= 0 & lag(trading_strength) > threshold, 1,
      ifelse(macd_line_shifted > 0 & lag(macd_line) <= 0 & lag(trading_strength) > threshold, -1, 0))
  ) %>%
  select(-macd_line_shifted)  # Remove the shifted column

# get rid of entries outside of backtest
energy_stocks <- energy_stocks %>%
  filter(date >= as.Date("2021-01-01") & date <= as.Date("2023-12-31"))

starting_cash <- 100000

# have max trades per day and max amount per day ex) $10000, 12 trades
max_day_trades <- 12
max_trade_amount <- 10000

applyStrategy <- function(day, equity) {
  cashin <- 0
  cashout <- 0
  return <- 1
  
  candidates <- energy_stocks %>%
    filter(date == day, !is.na(trading_signal) & trading_signal != 0) %>%
    arrange(-lag(trading_strength))
  
  num_trades <- nrow(candidates)
  if (num_trades > max_day_trades) {
    candidates <- candidates[c(1:max_day_trades), ]
    num_trades <- max_day_trades
  }
  
  trade_amount <- min(max_trade_amount, equity/num_trades)
  if (num_trades > 0) {
    candidates$buy <- NA
    candidates$sell <- NA
    candidates$return <- NA
    
    # not so sure about return calculations here
    for (i in 1:num_trades) {
      candidates$buy[i] <- ifelse(candidates$trading_signal[i] == 1, candidates$open[i], candidates$close[i])
      candidates$sell[i] <- ifelse(candidates$trading_signal[i] == 1, candidates$close[i], candidates$open[i])
      candidates$return[i] <- 1 + (candidates$sell[i] - candidates$buy[i]) / candidates$buy[i]
    }
    # not sure on these either, even though this is from his code (see ex below)
    cash_in <- num_trades * trade_amount
    return <- prod(candidates$return) * (cashin / equity) + (1 - cashin/equity)
    cash_out <- cash_in*prod(candidates$return)
  }else {
    candidates <- NULL
  }
  return(list(trades=candidates, cash_in=cash_in, cash_out=cash_out, return=return))
}

# for this day we shorted all 3 stocks and they all went down so cash_out > cash_in but return = 1???
print(applyStrategy(as.Date("2021-07-16"), 100000))