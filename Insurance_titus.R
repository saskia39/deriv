
#################################################################
#2 - PORTFOLIO INSURANCE STRATEGY

library(quantmod)
library(PerformanceAnalytics)

#get S&P500 price on 21/12/2022
getSymbols("^GSPC", from="2022-01-01", src='yahoo')
SP <- na.omit(GSPC)
SP_price_0 <- as.numeric(SP$GSPC.Adjusted["2022-12-21"])


#get stock prices on 21/12/2022
getSymbols("GOOG", src = "yahoo", from = "2022-01-01",)
goog <- na.omit(GOOG)
goog_price_0 <- as.numeric(goog$GOOG.Adjusted["2022-12-21"])

getSymbols("AMZN", src = "yahoo", from = "2022-01-01")
amzn <- na.omit(AMZN)
amzn_price_0 <- as.numeric(amzn$AMZN.Adjusted["2022-12-21"])

getSymbols("ABNB", src = "yahoo", from = "2022-01-01")
airbnb <- na.omit(ABNB)
airbnb_price_0 <- as.numeric(airbnb$ABNB.Adjusted["2022-12-21"])

getSymbols("ZM", src = "yahoo", from = "2022-01-01")
zoom <- na.omit(ZM)
zoom_price_0 <- as.numeric(zoom$ZM.Adjusted["2022-12-21"])

#number of shares of stock purchased in time 0
shares_goog <- 3000
shares_amzn <- 3500
shares_airbnb <- 2500
shares_zoom <- 1500

#initial value of the portfolio
portfolio_value_0 <- shares_goog*goog_price_0+shares_amzn*amzn_price_0+shares_airbnb*airbnb_price_0+shares_zoom*zoom_price_0

#weight of the stocks on the portfolio
w_goog <- (goog_price_0*shares_goog)/portfolio_value_0
w_amzn <- (shares_amzn*amzn_price_0)/portfolio_value_0
w_airbnb <- (shares_airbnb*airbnb_price_0)/portfolio_value_0
w_zoom <- (shares_zoom*zoom_price_0)/portfolio_value_0


#BETA OF THE PORTFOLIO
#getting the beta of the stocks from yahoo fiance we get
beta_goog <- 1.06
beta_amzn <- 1.19
beta_airbnb <- 0.93
beta_zoom <- -0.33

#beta of the portfolio
beta_portfolio <- w_goog*beta_goog+w_amzn*beta_amzn+w_airbnb*beta_airbnb+w_zoom*beta_zoom

#table of the portfolio initial value
portfolio <- data.frame(c("Google", "Amazon", "AirBnb", "Zoom", "Total"),
                        c(goog_price_0, amzn_price_0, airbnb_price_0, zoom_price_0, "-"), 
                        c(shares_goog, shares_amzn, shares_airbnb, shares_zoom, (shares_goog+shares_amzn+shares_airbnb+shares_zoom)), 
                        c(shares_goog*goog_price_0, shares_amzn*amzn_price_0, shares_airbnb*airbnb_price_0, shares_zoom*zoom_price_0, portfolio_value_0),
                        c(w_goog, w_amzn, w_airbnb, w_zoom, (w_goog+w_amzn+w_airbnb+w_zoom)),
                        c(beta_goog, beta_amzn, beta_airbnb, beta_zoom, beta_portfolio))
colnames(portfolio) <- c("Stocks", "Price", "# stocks", "Investment", "#Weight %", "Beta")
kable(portfolio)

#number of puts to buy
puts <- as.integer(beta_portfolio*(portfolio_value_0/(SP_price_0*100))) #we need an integer number for the puts


#CALCULATE THE RELATIONSHIP BETWEEN THE INDEX PRICE AND THE VALUE OF THE PORTFOLIO
min_portfolio_value <- 890000 #protect the portfolio to a minimum value of 890 000
min_return <- (min_portfolio_value-portfolio_value_0)/portfolio_value_0 #minimum return
rf <- 0.0428*0.25

#from CAPM: return portfolio = risk free rate + (beta * (market return - risk free rate)) 
#=> market return = ((beta - 1) * risk free rate + return portfolio)/beta
min_SP_return <- ((beta_portfolio - 1) * rf + min_return)/beta_portfolio
min_SP_price <- SP_price_0*(1+min_SP_return)

#the strike price of the index option will be the minimum SP500 price
strike <- min_SP_price
index_prices <- tail(SP$GSPC.Adjusted, 8)

#price of the puts to buy
SP_put_price <- 145.6 #SP put option price for an expiration date of 2023-03-17 and strike=3840


#pay off from the put option purchased 
payoff_puts <- puts * 100 * (strike - index_prices)
payoffputs <- ifelse(payoff_puts > 0, payoff_puts, 0) #because if the index price exceeds the strike, we won't exercise the option


#stock prices now
goog_price_1 <- tail(goog$GOOG.Adjusted, 8)
amzn_prices_1 <- tail(amzn$AMZN.Adjusted, 8)
airbnb_prices_1 <- tail(airbnb$ABNB.Adjusted, 8)
zoom_prices_1 <- tail(zoom$ZM.Adjusted, 8)

portfolio_value_1 <- shares_goog * goog_price_1 + shares_amzn * amzn_prices_1 + shares_airbnb * airbnb_prices_1 + shares_zoom * zoom_prices_1 - as.numeric(puts * SP_put_price)

insurance_payoff <- portfolio_value_1+payoffputs

insurance <- data.frame(index_prices, portfolio_value_1, payoffputs, insurance_payoff, check.names = F)
colnames(insurance) <- c("Index Price", "Portfolio Value", "Puts Payoff", "Insurance Result")
kable(insurance)
