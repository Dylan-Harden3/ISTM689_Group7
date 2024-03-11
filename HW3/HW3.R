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

generate_indicators <- function(stock, threshold) {
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
  return(energy_stocks)
}
# for calculating the returns each day
applyStrategy <- function(day, equity) {
  cash_in <- 0
  cash_out <- 0
  return <- 1
  # get all the stocks with a trading signal for this day ordered by the strength (MACD histogram)
  candidates <- energy_stocks %>%
    filter(date == day, !is.na(trading_signal) & trading_signal != 0) %>%
    arrange(-lag(trading_strength))
  # make sure we only do max_day_trades trades per day
  num_trades <- nrow(candidates)
  if (num_trades > max_day_trades) {
    candidates <- candidates[c(1:max_day_trades), ]
    num_trades <- max_day_trades
  }
  # the amount we put in each trade depends on how much we have
  trade_amount <- min(max_trade_amount, equity/num_trades)
  if (num_trades > 0) {
    candidates$buy <- NA
    candidates$sell <- NA
    candidates$return <- NA
    # for long buy at open sell at close and for short but at close sell at open
    for (i in 1:num_trades) {
      candidates$buy[i] <- ifelse(candidates$trading_signal[i] == 1, candidates$open[i], candidates$close[i])
      candidates$sell[i] <- ifelse(candidates$trading_signal[i] == 1, candidates$close[i], candidates$open[i])
      candidates$return[i] <- 1 + (candidates$sell[i] - candidates$buy[i]) / candidates$buy[i]
    }

    cash_in <- num_trades * trade_amount
    return <- prod(candidates$return) * (cash_in / equity) + (1 - cash_in/equity)
    cash_out <- cash_in*prod(candidates$return)
  }else {
    candidates <- NULL
  }
  return(list(trades=candidates, cash_in=cash_in, cash_out=cash_out, return=return))
}
# calculate the overall returns for the whole backtest given the trades we made
portfolio_stats <- function(trades, portfolio_return, trading_days) {
  long_trades <- nrow(subset(trades, trades$trading_signal == -1))
  winning_long_trades <- nrow(subset(trades, trades$trading_signal == -1 & trades$return > 1))
  percent_winning_long_trades <- winning_long_trades / long_trades * 100
  avg_long_return <- mean(subset(trades, trading_signal == 1)$return, na.rm=TRUE)
  
  short_trades <- nrow(subset(trades, trades$trading_signal == 1))
  winning_short_trades <- nrow(subset(trades, trades$trading_signal == 1 & trades$return > 1))
  percent_winning_short_trades <- winning_short_trades / short_trades * 100
  avg_short_return <- mean(subset(trades, trading_signal == -1)$return, na.rm=TRUE)
  
  percent_winning_trades <- nrow(subset(trades, trades$return > 1)) / nrow(trades)
  
  sharpe_ratio <- (mean(portfolio_return, na.rm=TRUE) - 1) / sd(portfolio_return, na.rm=TRUE) * sqrt(252)
  
  cumulative_return <- rep(length(trading_days), 1)
  max_return <- rep(length(trading_days), 1)
  
  for (day in 1:length(trading_days)) {
    cumulative_return[day] <- prod(portfolio_return[c(1:day)], na.rm=TRUE)
    max_return[day] <- max(cumulative_return[c(1:day)], na.rm=TRUE)
  }
  
  drawdown <- cumulative_return - max_return
  drawdown_percent <- drawdown / max_return
  max_drawdown_percent <- min(drawdown_percent) * 100
  drawdown_period <- 0
  max_drawdown_period <- 0
  for (day in c(1:length(trading_days))){
    drawdown_period <- ifelse(drawdown[day] < 0, drawdown_period + 1, 0)
    max_drawdown_period <- ifelse(drawdown_period > max_drawdown_period, drawdown_period, max_drawdown_period)
  }
  
  plot(cumulative_return,type="l",col="black",lwd=2,xlab="Time Period",ylim=c(0.9,1.1),
       ylab="Portfolio Return",main="Portfolio Results")
  lines(max_return,co=2,lw=2)
  lines(portfolio_return,co=4,lw=2)
  
  return(list(longtrades=long_trades, percentwinninglongtrades=percent_winning_long_trades, avgreturnlongtrades=avg_long_return,
              shorttrades=short_trades, percentwinningshorttrades=percent_winning_short_trades, avgreturnshorttrades=avg_short_return,
              cumulativereturn=cumulative_return, max_return=max_return,
              maxdrawdownpercent=max_drawdown_percent, maxdrawdownperiod=max_drawdown_period,
              sharpe=sharpe_ratio, percentwinningtrades=percent_winning_trades))
}
# this is the threshold for trading strength, or the MACD histogram
# you can try higher values for less risk and eventually it will make 0 trades
# or you can make it lower or 0 for no risk adjustment
threshold <- 1.0
starting_equity <- 100000
  
# have max trades per day and max amount per day ex) $10000, 12 trades
max_day_trades <- 12
max_trade_amount <- 10000
energy_stocks <- generate_indicators(stock, threshold)

trading_days <- unique(energy_stocks$date)
current_equity <- starting_equity
trades <- NULL
portfolio_return <- rep(length(trading_days),1)
# for each day apply our strategy
for (day in 1:length(trading_days)) {
  date <- trading_days[day]
  results <- applyStrategy(date, current_equity)
  # keep track of all of the trades we have made
  if (is.null(trades)) {
    trades <- results$trades
  }else {
    trades <- rbind(trades, results$trades)
  }
  current_equity <- current_equity - results$cash_in + results$cash_out
  portfolio_return[day] <- results$return
}
stats <- portfolio_stats(trades, portfolio_return, trading_days)