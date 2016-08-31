---
  title: "Index Tracking with Integer Programming"
output:  
  html_document:  
  keep_md: true 
---

library(lpSolve)
library(reshape)
library(magic)

data = read.csv("N100StkPrices.csv", header = TRUE) # read in the data

data = na.omit(data) # remove na's
ticker = data$TICKER

delete = seq(1, dim(data)[1])[ticker == "MDLZ"] # spun off MDLZ
data = data[-delete, ]

date = apply(as.matrix(data$date), MARGIN=1, FUN="toString")
date = as.Date(date, "%Y%m%d")
ticker = data$TICKER
price = data$PRC
shares = data$SHROUT


# Accounting for changes in ticker names

# KFT changed to KRFT in Oct 2012.
ticker[ticker == "KFT"] = "KRFT"

# SXCI changed to CTRX in Jul 2012.
ticker[ticker == "SXCI"] = "CTRX"

# HANS changed to MNST in Jan 2012.
ticker[ticker == "HANS"] = "MNST"




# convert prices to a matrix, arranged by rows of dates and columns of tickers
unique_dates = sort(unique((date)))
unique_tickers = sort(unique(ticker))

priceMat = matrix(NA, length(unique_dates), length(unique_tickers))
sharesMat = matrix(0, length(unique_dates), length(unique_tickers))
head(sharesMat)



for (i in 1:length(unique_tickers)) {
  tic = unique_tickers[i]
  idx = is.element(unique_dates, date[ticker == tic])
  
  priceMat[idx, i] = price[ticker == tic]
  sharesMat[idx, i] = shares[ticker == tic]
}

rownames(priceMat) = as.character(unique_dates)
rownames(sharesMat) = as.character(unique_dates)

rm(list = c("data", "delete", "i", "idx", "price", "shares", "tic", "ticker", "date"))

############ problem 1 #######################
returnMat = matrix(NA, length(unique_dates)-1, length(unique_tickers)) #initialize matrix for daily returns
returnMat[1:5, 1:5]
for (i in 1:length(unique_tickers)) { #loop through each unique ticker symbol
  prices = priceMat[, i] #get the prices for that symbol
  daily_returns = diff(prices)/prices[-length(prices)] #calculate the return using (new - old)/(old) formula
  return_dates = unique_dates[2:length(unique_dates)] #remove the first date which we cannot calculate return for
  returnMat[, i] = daily_returns #update return matrix
}

rownames(returnMat) = as.character(return_dates) #set rownames as return dates
colnames(returnMat) = unique_tickers #set colnames as tickers

############ problem 2 #######################
corrMat = cor(returnMat, use="pairwise.complete.obs") #calculate correlation matrix, rho

head(corrMat)
tickers = unique_tickers[1:10]

############ problem 3 #######################
tiny_corrMat = corrMat[1:10, 1:10]
tiny_corrMat_metl = melt(tiny_corrMat, id = c('X2', 'X1'))
tiny_corrMat_metl
tiny_priceMat = priceMat[1:10, 1:10]
tiny_sharesMat = sharesMat[1:10, 1:10]

constructFund <- function(rho, q, sharesMat, priceMat, unique_tickers, unique_dates) {
  n = length(unique_tickers)
  reshaped_rho = melt(rho, id = c('unique_tickers', 'unique_tickers')) # reshape similar matrix into vector
  C = c(reshaped_rho$value, rep(0, n))
  A1 = c(rep(0, n*n), rep(1, n)) # constraint 1, only 1 constraint
  # A2 = c(rep(1, length(unique_tickers) * length(unique_tickers)), rep(0, length(unique_tickers)))
  A2x = matrix(0, n, n*n)
  for (j in 1:n) {
    A2x[j, (n*(j-1)+1):(n*j)] = 1
  }
  A2y = matrix(0, n, n)
  A2 = cbind(A2x, A2y) # constraint 2, 100 constraints
  A3x = diag(1, n*n)
  A3y1 = diag(-1, n)
  A3y = NULL
  for (j in 1:n) {
    A3y = rbind(A3y, A3y1)
  }
  A3 = cbind(A3x, A3y) # constraint 3, 10000 constraints
  A = rbind(A1, A2, A3)
  b = c(q, rep(1, n), rep(0, n*n))
  dir = c(rep('=', n+1), rep('<=', n*n))
  s = lp("max", C, A, dir, b, all.bin=TRUE)
  solutionMat = matrix(s$solution, n, n, byrow = TRUE)
  
  price = priceMat[dim(priceMat)[1], ]
  shares = sharesMat[dim(sharesMat)[1], ]
  value = price * shares
  weights = NULL
  for (j in 1:n) {
    weights = c(weights, sum(solutionMat[, j] * value))
  }
  sum_weights = sum(weights)
  real_weights = weights / sum_weights
  return (real_weights)
}

# a small test on a 10 by 10 matrix, picking 5 of them
test_solution = constructFund(tiny_corrMat, 5, tiny_priceMat, tiny_sharesMat, tickers, unique_dates)
test_solution # looks good!

########## problem 4 ############################################
weights = constructFund(corrMat, 25, priceMat, sharesMat, unique_tickers, unique_dates)



# Read Monthly Data -------------------------------------------------------

# read in the data
mdata = read.csv("N100Monthly.csv", header = TRUE, stringsAsFactors = FALSE)

# clean up data
mdate = apply(as.matrix(mdata$date), MARGIN = 1, FUN = "toString")
mdate = as.Date(mdate, "%Y%m%d")

mticker = mdata$TICKER
mprice = mdata$PRC
mshares = mdata$SHROUT
mticker[mticker == "FOXA"] = "NWSA"


unique_mdates = sort(unique((mdate)))
unique_mtickers = sort(unique(mticker))

idx = is.element(unique_mtickers, unique_tickers)

# if (!all(idx)) {
#   print("Warning: Some tickers seem to be missing")
# }

monthlyPriceMat = matrix(NA, length(unique_mdates), length(unique_tickers))

for (i in 1:length(unique_tickers)) {
  tic = unique_tickers[i]
  idx = is.element(unique_mdates, mdate[mticker == tic])
  monthlyPriceMat[idx, i] = mprice[mticker == tic]
}

monthlyPriceMat = rbind(priceMat[250,], monthlyPriceMat)

################### calculate monthly return #####################################################
returnMonthlyMat = matrix(NA, length(unique_mdates), length(unique_tickers)) #initialize matrix for daily returns

for (i in 1:length(unique_tickers)) { #loop through each unique ticker symbol
  prices = monthlyPriceMat[, i] #get the prices for that symbol
  monthly_returns = diff(prices) #calculate the return using (new - old)/(old) formula #remove the first date which we cannot calculate return for
  returnMonthlyMat[, i] = monthly_returns #update return matrix
}
################### our fund return ##################################################
investment_vector = weights * total_investment
share_vector = investment_vector / priceMat[250,]
total_value = share_vector * monthlyPriceMat[1,]

total_investment = 1000000

real_return = NULL
for (i in 1:dim(returnMonthlyMat)[1]) {
  r = sum(share_vector * returnMonthlyMat[i,])
  real_return = c(real_return, r)
}
real_return
#################### nasdaq return ####################################
nasdaq_2013 = c(2660.93, 2731.53, 2738.58, 2818.69, 2887.44, 2981.76, 2909.60, 3090.19, 3073.81, 3218.20, 3377.73, 3487.82, 3592.00)
num_share_nasdaq = total_investment / nasdaq_2013[1]
nasdaq_monthly_return = NULL
for (i in 1:length(nasdaq_2013)) {
  monthly_return = nasdaq_2013[i+1] - nasdaq_2013[i]
  nasdaq_monthly_return = c(nasdaq_monthly_return, monthly_return)
}

nasdaq_return = nasdaq_monthly_return[1:12] * num_share_nasdaq

################### plot the two return
par(mfrow=c(1, 1))
plot(nasdaq_return, type = 'b', xlab = 'month', ylab = 'nasdaq return', col = 'red')
lines(real_return, type = 'b', col = 'blue')
legend(8.8, 1,legend = c('nasdaq', 'our fund'), col = c('red', 'blue'), lty = c(1, 1))
lines(real_return, type = 'b', xlab = 'month', ylab = 'our fund return')

################## matrix #########################
# read in the data
ageMat = read.csv("Age Corr Matrix.csv", header = TRUE)
head(data)
corrMat[1:5, 1:5]
ageMat[1:5, 1:5]
weights = constructFund(as.matrix(ageMat), 25, priceMat, sharesMat, unique_tickers, unique_dates)
