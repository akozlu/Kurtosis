#Q1
my.data <- load('stocks.RData')
print(class(my.data))

daily.ret <- function(prices) {
  if (!is.numeric(prices)) {
    stop("Warning!. This column is not numeric")
  } else {
    relative_returns <- diff(prices) / prices[-length(prices)]
    return(relative_returns)
  }
  
}

ex <- daily.ret((seq(10)))
ex
stocks_excluding_dates <- stock.df[, -1]

returns.df <-
  as.data.frame(sapply(stocks_excluding_dates[colnames(stocks_excluding_dates)], daily.ret))
dates <- stock.df[, c("Date")] # extract the Date column
dates_as_chars <-
  as.character(dates) # turn it into vector of characters
dates_as_chars <- dates_as_chars[-1] # get rid of the first date
dates <- as.factor(dates_as_chars) # turn it back into a factor
returns.df <-
  cbind('Date' = dates, returns.df) # merge our new column with returns.df
colnames(returns.df)

sapply(returns.df[colnames(stocks_excluding_dates)], mean)
n <-10
b <- (3*((n-1)^2))
print(b)
kurtosis <- function(y,excess = TRUE) {
  if (!is.numeric(y)) {
    stop("Warning!. The vector you entered is not numeric")
  } else {
    if(length(y) <4){
      stop("Warning! The length of input vector is less than 4.")
    }else {
    n <-length(y)
    mean_y <- mean(y)
    sd_y <- sd(y)
    summation_term <- sum((y - mean(y))^4) 
    summation_term <- summation_term / sd_y^4
    denom <- (n-1) * (n-2) * (n-3)
    first_term <- (n * (n+1)) / denom
    denom_2 <-(n-2) * (n-3)
    last_term <- (3*((n-1)^2)) /denom_2
    sample_excess_kurtosis <- (summation_term * first_term)
    sample_excess_kurtosis <- sample_excess_kurtosis - last_term
    if(excess == TRUE) {
    return(sample_excess_kurtosis)
    }
    return (sample_excess_kurtosis + 3)
    }
   
  }
}
my.vec <- c(2.10921657, -0.89218616, -0.23085193,  0.39297494,  1.09767915, -0.13936090, -0.36960242, -0.02307942)

print(kurtosis(y = seq(10)))
print(kurtosis(y = my.vec))
print(kurtosis(y = letters))
kurtosis(y = seq(3))
print(kurtosis(y = seq(10),excess = FALSE))

justnum.df <- Filter(is.numeric, returns.df)
ncol(justnum.df) 
kurtosis_returns <- sapply(justnum.df, kurtosis)
print(kurtosis_returns)
print((max(kurtosis_returns)))
print(which.max(kurtosis_returns))

