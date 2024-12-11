
rm(list = ls())

# Step 1: Load necessary libraries
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(lubridate)
library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
library(kableExtra)
library(purrr)
library(derivmkts)
library(ggplot2)




############################################
########## Portfolio Construction ##########

stock_data <- read_xlsx(file.path(here::here("data"), "stock prices.xlsx"), sheet = 2)

#### Assumuned parameters
tsla_shares <- 5000
amzn_shares <- 5000
beta_tsla <- 2.30 #yahoo 5y monthly: fixed for now
beta_amzn <- 1.15 #yahoo 5y monthly: fixed for now 

### Rename some columns 
stock_data <- stock_data %>%
  rename(Tesla = TESLA,
         Amazon = AMAZON.COM)

### calculate daily portfolio value 
stock_data <- stock_data %>%
  mutate(
    Portfolio_Value = (Tesla * tsla_shares) + (Amazon * amzn_shares),
    w_tsla = (Tesla*tsla_shares)/Portfolio_Value,
    w_amzn = (Amazon*amzn_shares)/Portfolio_Value,
    beta_portfolio = w_tsla * beta_tsla + w_amzn * beta_amzn
  )

##Table of portfolio initial value 
portfolio <- data.frame(
  Asset = c("Tesla", "Amazon", "Total"),  
  Stock_Price = c(stock_data$Tesla[1], stock_data$Amazon[1], stock_data$Portfolio_Value[1]),  
  Number_of_Stocks = c(tsla_shares, amzn_shares, tsla_shares + amzn_shares),  
  Investment = c(tsla_shares * stock_data$Tesla[1], amzn_shares * stock_data$Amazon[1], stock_data$Portfolio_Value[1]),  
  Weight = c(stock_data$w_tsla[1], stock_data$w_amzn[1], (stock_data$w_tsla[1] + stock_data$w_amzn[1])),  
  Beta = c(beta_tsla, beta_amzn, stock_data$beta_portfolio[1]) 
)
kable(portfolio)



############################################
####### Index option based Insurance ####### 
getSymbols("QQQ", src = "yahoo", from = "2024-11-18", to = "2024-11-30")
NASDAQ <- na.omit(QQQ)
index_level <- as.numeric(NASDAQ$QQQ.Adjusted) 
NASDAQ_df <- data.frame(
  Date = index(NASDAQ),  
  index_level = index_level  
)

#Number of put options needed
stock_data <- merge(stock_data, NASDAQ_df, by = "Date", all.x = TRUE)
stock_data <- stock_data %>%
  mutate(
    Puts_Needed = ceiling(beta_portfolio * Portfolio_Value / (index_level * 100))
  )

#Decide minimum portfolio value (floor value)
min_portfolio_value <- stock_data$Portfolio_Value[1] ### start portfolio value 
min_return <- (min_portfolio_value - stock_data$Portfolio_Value) / stock_data$Portfolio_Value 
rf <- 0.0428*0.25  #### adjust form exercise 1 maybe???

#Minimum NASDAQ100 return from CAPM: return portfolio = risk free rate + (beta * (market return - risk free rate)) 
min_index_return <- ((stock_data$beta_portfolio-1)*rf +min_return) / stock_data$beta_portfolio
min_index_price <- stock_data$index_level*(1+ min_index_return) 

# strike price for put option as the minimum NASDAQ100 price
strike_price <- min_index_price
index_prices <- NASDAQ$QQQ.Adjusted

# #price of the puts option from BS
# time_to_maturity <- 0.25 # Assuming 3 months expiry
# sigma <- 0.2 # assuming 20%implied volatility for now, maybe calculate it using market value????
# stock_data <- stock_data %>%
#   rowwise()%>%
#   mutate(
#     strike_price = list(min_index_price[which(NASDAQ_df$Date == Date)]),# Select the correct strike price for the current row
#     index_put_price = list(bsput(s = index_level,
#                             k = strike_price,
#                             v = sigma,
#                             r = rf,
#                             tt = time_to_maturity,
#                             d= 0))
#   )%>%
#   ungroup()

#payoff put option 
stock_data <- stock_data %>%
  mutate(
    Put_Payoff = pmax(strike_price - index_level, 0) * Puts_Needed * 100, #because if the index price exceeds the strike, we won't exercise the option
    Insured_Portfolio_Value = Portfolio_Value + Put_Payoff
  )
# Fill NA with the previous value
stock_data <- stock_data %>%
  mutate(Insured_Portfolio_Value = zoo::na.locf(Insured_Portfolio_Value, na.rm = FALSE))

#Create the plot
portfolio_plot <- ggplot(stock_data, aes(x = Date)) +
  geom_line(aes(y = Portfolio_Value, color = "Uninsured Portfolio"), size = 1) +
  geom_line(aes(y = Insured_Portfolio_Value, color = "Insured Portfolio"), na.rm = TRUE, size = 1) +
  labs(
    title = "Performance of Insured vs. Uninsured Portfolio",
    x = "Date",
    y = "Portfolio Value",
    color = "Portfolio Type"
  ) +
  scale_color_manual(values = c("Uninsured Portfolio" = "blue", "Insured Portfolio" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) # NOTE: insured portfolio doesn't include the cost of options since I don't have BS price and can't calculate costs 

ggsave("portfolio_values_plot.png", plot = portfolio_plot, width = 10, height = 6, dpi = 300)

