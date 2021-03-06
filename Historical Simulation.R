# Value at Risk for a European Call Option #
############################################

# This function computes the VaR for an equity
# Call option. The option is a European option,
# in that it can only be exercised at maturity.
# The expected shortfall and the option value
# are also computed.

# maturity is the maturity date of the option, measured in 
# years from today's date. interest_rate is the interest 
# rate across the period. hist_data is the number of 
# values we will use from historical data. volatility is the
# the standard deviation of the share price and the strike_price
# is the strike for the option. alpha is the level (1 - alpha) 
# at which we want to calculate the VaR for.

HistSim <- function(maturity, interest_rate, hist_data, volatility, strike_price, alpha, today = 0){
  
  # Validity checks for function parameters
  
  if (alpha > 1 | alpha < 0){
    stop("alpha is not between 0 and 1.")
  }
  
  if (maturity < 0){
    stop("Maturity cannot be negative.") 
  }
  
  if (volatility < 0){
    stop("Volatility cannot be negative.")
  }
  
  if (maturity <= today){
    stop("Maturity date must be greater than today's date.")
  }
  
  # Generate some financial data
  
  price <- c(rnorm(hist_data, 45, 3)) 
   
  # Compute the daily log returns.    
  
  returns <- c()
  
  for (i in 2:length(price)){
     
    returns[[i]] <- log(price[i] / price[i - 1])
    
  }
  
  # Remove NA
  
  returns <- na.omit(returns)
  
  # Simulating the share price
  
  simulated <- c(rep(0, length(price)))
  simulated[1] <- price[1] * exp(returns[1])
  
  for (i in 2:length(price)){
  
    simulated[[i]] <- simulated[i - 1] * exp(returns[i])
    
  }
  
  # The option price will be calculated using the Black-Scholes
  # PDE. We first need to calculate d1 and d2. We will assume we
  # are computing the option price at time t = 0. 
  
  d1 <- (log(price/strike_price) + (interest_rate - volatility ^ 2 / 2) * (maturity - today)) / sqrt(maturity - today) * volatility
  d2 <- d1 - sqrt(maturity - today) * volatility
  
  call_value <- price * pnorm(d1) - strike_price * exp(-interest_rate * (maturity - today)) * pnorm(d2)  
  
  # Suppose our portfolio consists of 100 call options. Then we can compute the
  # portfolio value
  
  portfolio_value <- 100 * call_value
  
  # Compute the (1-alpha) VaR risk #
  ##################################
  
  portfolio_change <- c()
  
  for (i in 2:length(portfolio_value)){
    
    portfolio_change[[i]] <- portfolio_value[i] - portfolio_value[i - 1]
    
  }
  
  portfolio_change <- na.omit(portfolio_change)    
  
  # Ordering the changes in portfolio before taking (1-alpha)% quantile
  
  ordered_portfolio_change <- sort(portfolio_change, decreasing = FALSE) 
  
  VaR <- ordered_portfolio_change[round(alpha * length(portfolio_change), 0)]
  
  library(ggplot2)
  
  # Histogram of the returns
  
  Days <- round((maturity - today) * 252) 
  
  portfolio_change <- data.frame(Change = portfolio_change)
  
  Hist <- ggplot(aes(x = Change), data = portfolio_change) + geom_histogram(color = "black", fill = "green3")
  Hist <- Hist + labs(x = "Portfolio Change", y = "Frequency") + ggtitle(paste("Histogram of Portfolio Changes across", Days, "days"), 
                                                                               subtitle = paste( "with", (1 - alpha) * 100, "% probability")) 
  Hist <- Hist + geom_vline(xintercept = VaR, linetype = "dashed", color = "red3",size = 1) + theme_bw()
  
  V <- list(paste("The ", (1 - alpha) * 100, "% VaR is ", "£", -round(VaR, 2), sep = ""), Hist)
  
  names(V) <- c("VaR", "Histogram")
  return(V)
}     

 
a <- HistSim(0.12, 0.03, 252, 0.05, 34, 0.05) 
a$VaR
a$Histogram
 
