# Queuing Optimization with Simulation

![](proj4_header.PNG)
![](proj4_q1.PNG)

```r
rm(list=ls())
library(MASS)

load('queue.rdata')

inter_A = diff(A, lag=1) # calculate inter-arrival times

# fit arrival times to distributions
A_gamma = fitdistr(x=inter_A, densfun="gamma")
A_exp = fitdistr(x=inter_A, densfun="exponential")
A_weibull = fitdistr(x=inter_A, densfun="weibull")

# fit service times to distributions
S_gamma = fitdistr(x=S, densfun="gamma")
S_exp = fitdistr(x=S, densfun="exponential")
S_weibull = fitdistr(x=S, densfun="weibull")

# estimate inter-arrival times
A_gamma_est = rgamma(n=length(A), shape=A_gamma$estimate[1],rate=A_gamma$estimate[2])
A_exp_est = rexp(n=length(A), rate=A_exp$estimate[1])
A_weibull_est = rweibull(n=length(A), shape=A_weibull$estimate[1], scale=A_weibull$estimate[2])

# estimate service times
S_gamma_est = rgamma(n=length(S), shape=S_gamma$estimate[1], rate=S_gamma$estimate[2])
S_exp_est = rexp(n=length(S), rate=S_exp$estimate[1])
S_weibull_est = rweibull(n=length(S), shape=S_weibull$estimate[1], scale=S_weibull$estimate[2])
```

The gamma distribution appears to be the best fit of the data. We will use the gamma distribution to estimate inter-arrival times and service times for the following problems.

```r
qqplot(inter_A, A_gamma_est, xlab='Past Inter-Arrival Time Observations', ylab='Estimated Inter-Arrival Distribution', main='qq plot for Gamma Estimate of Inter-Arrival Time')
```

![](project4_files/figure-html/unnamed-chunk-2-1.png)

```r
qqplot(inter_A, A_exp_est, xlab='Past Inter-Arrival Time Observations', ylab='Estimated Inter-Arrival Distribution', main='qq plot for Exp Estimate of Inter-Arrival Time')
```

![](project4_files/figure-html/unnamed-chunk-2-2.png)

```r
qqplot(inter_A, A_weibull_est, xlab='Past Inter-Arrival Time Observations', ylab='Estimated Inter-Arrival Distribution', main='qq plot for Weibull Estimate of Inter-Arrival Time')
```

![](project4_files/figure-html/unnamed-chunk-2-3.png)
  
![](proj4_q2.PNG)
  
To minimize cost, the store should have 15 counters open for black friday. The estimated cost is inidicated below under min(total_cost).
  
Cost was calculated using the formula: Overall Cost = [$1 X (Customers with Wait Time > 10)] +[$40/60 X No. Of minutes all counters are open]

```r
n = 10000 # 10,000 simulations
final_cost = rep(NA, 20) # vector to hold costs for each possible # of counters open
set.seed(400)

for (counter in 1:20) 
  {
    counter_choice = sample(1:counter, n, replace=TRUE) # simulate random selection of queue
    line = matrix(0, n, counter) # matrix with customer as row and counter as column; 1 indicates customer's counter choice
      for (j in (1:length(counter_choice))) # populate matrix
        {
        line[j,counter_choice[j]] = 1
      }
    # use gamma dist to simulate arrival and service times
    A_gamma_inter = rgamma(n=n-1, shape=A_gamma$estimate[1], rate=A_gamma$estimate[2])
    A_gamma_arrival = c(0, cumsum(A_gamma_inter))
    S_gamma_service = rgamma(n=n, shape=S_gamma$estimate[1], rate=S_gamma$estimate[2])
    cost = rep(NA, counter) # vector to hold cost of each counter
    D_max = rep(NA, counter) # max duration of a line
    for (k in (1:counter)) 
      {
        A_line = matrix(0, n, counter) # simulated arrival times
        S_line = matrix(0, n, counter) # simulated service times
        A_line[,k] = line[,k] * A_gamma_arrival
        serial = min(which(A_line[,k] != 0))
        A_line = A_line[,k][A_line[,k] > 0]
        S_line[,k] = line[,k] * S_gamma_service
        S_line = S_line[,k][S_line[,k] > 0]
        T = rep(NA,length(A_line)) # service start times
        D = rep(NA,length(A_line)) # durations
        W = rep(NA,length(A_line)) # wait times
        
        T[1] = min(A_line) # set first service start time as the first arrival time in the line
        D[1] = T[1] + S_line[serial] # set duration as the first arrival time + the first service time
        W[1] = 0 # set first wait time to 0
    
          for (i in 2:length(A_line)) # loop to calculate service times and durations
            {
              T[i] = max(D[i-1], A_line[i])
              D[i] = T[i] + S_line[i]
            }
        W = T - A_line # calculate wait times
        cost[k] = sum(W > 10) # calculate cost per line
        D_max[k] = max(D) # save max duration to use as minutes all counters are open for calculating final cost
      }
    final_cost[counter] = sum(cost, na.rm=TRUE) + 40 * max(D_max)/60 * counter
}

final_cost
```

```
##  [1] 20959.70 20979.67 21224.65 21228.00 21209.89 21192.66 21142.94
##  [8] 21360.74 21232.27 21129.16 20848.61 19689.50 16896.01 15640.94
## [15] 15013.73 15568.33 16421.14 16543.13 17485.63 18367.58
```

```r
plot(1:20, final_cost, type='line', xlab='# of Counters Open', ylab='Estimated Cost', main='Multiple Queues w/ Random Selection of Queue')
```

```
## Warning in plot.xy(xy, type, ...): plot type 'line' will be truncated to
## first character
```

![](project4_files/figure-html/unnamed-chunk-3-1.png)

```r
min(final_cost)
```

```
## [1] 15013.73
```

```r
which.min(final_cost)
```

```
## [1] 15
```
  
![](proj4_q3.PNG)
  
With 1 queue, the optimal number of counters is lower at 13. The estimated cost is inidicated below under min(cost_all). This tells us that 1 queue is more efficient than multiple queues with random selection.

```r
n = 5000 # simulate 5,000 customers
cost_all = rep(NA, 20) # vector to hold costs for each possible # of counters open

for (counter in 1:20) 
  {
    A_gamma_inter = rgamma(n=n-1, shape=A_gamma$estimate[1], rate=A_gamma$estimate[2])
    A_gamma_arrival = c(0, cumsum(A_gamma_inter))
    S_gamma_service = rgamma(n=n, shape=S_gamma$estimate[1], rate=S_gamma$estimate[2])
  
    D = rep(0, counter) # vector to hold service end times
    W = rep(NA, n) # wait times
    T = 0 # service start time
    D[1] = T + S_gamma_service[1] # first service end time = start time + first service time
    W[1] = 0 # first wait time is 0
    
    for (p in 2:n) # loop through customers
      {
        x = which.min(D) # assign each customer the counter with the least service end time
        old_D = D[x] # counter's previous service end time
        T = max(old_D, A_gamma_arrival[p]) # service start time = max of customer arrival time and old_D
        D[x] = T + S_gamma_service[p]
    
        W[p] = max((old_D - A_gamma_arrival[p]), 0) # if the customer arrives after the previous service end time, the wait time is 0
      }
    cost_all[counter] = sum(W > 10) + (max(D)/60 * 40 * counter)
}

cost_all
```

```
##  [1] 10491.214 10434.977 10452.022 10463.061 10475.817 10446.696 10337.401
##  [8] 10397.435 10157.102 10193.431  9926.731  9182.091  5985.524  6452.072
## [15]  6890.361  7196.143  7967.293  8373.518  8903.717  9265.664
```

```r
plot(1:20, cost_all, type='line', xlab='# of Counters Open', ylab='Estimated Cost', main='1 Queue')
```

```
## Warning in plot.xy(xy, type, ...): plot type 'line' will be truncated to
## first character
```

![](project4_files/figure-html/unnamed-chunk-4-1.png)

```r
min(cost_all)
```

```
## [1] 5985.524
```

```r
which.min(cost_all)
```

```
## [1] 13
```
  
![](proj4_q4.PNG)
  
The optimal number of counters for a multiple queuing system with shortest queue selection is 13, lower than it was with random selection. The cost is shown below under min(cost_all) This makes sense because it is more efficient for customers to choose the shortest queue.

```r
n = 5000
cost_all = rep(NA,20)

for (counter in 1:20) 
  {
    A_gamma_inter = rgamma(n=n-1, shape=A_gamma$estimate[1], rate=A_gamma$estimate[2])
    A_gamma_arrival = c(0, cumsum(A_gamma_inter))
    S_gamma_service = rgamma(n=n, shape=S_gamma$estimate[1], rate=S_gamma$estimate[2])
    
    qlen = rep(0,counter) # length of queues for each of the counters
    D = rep(0,counter) # service end times
    qlen[1] = 1
    W = rep(NA,n)
    T = 0
    D[1] = T + S_gamma_service[1]
    W[1] = 0
    
    for (p in 2:n) 
    {
      qlen[(D<=A_gamma_arrival[p]) & (qlen!=0)] = qlen[(D<=A_gamma_arrival[p]) & (qlen!=0)] - 1 # if a customer arrives after the service end times of some counters, then qlen decreases by 1
      x = which.min(qlen) # customer chooses queue with shortest length
      qlen[x] = qlen[x]+1 # after customer joins the line, qlen increases by 1
      T = max(D[x], A_gamma_arrival[p])
      old_D = D[x]
      D[x] = T + S_gamma_service[p]
      
      W[p] = max((old_D - A_gamma_arrival[p]),0)
      
    }
    cost_all[counter] = sum(W > 10) + (max(D)/60 * 40 * counter)
}

cost_all
```

```
##  [1] 10473.700 10548.236 10470.248 10590.062 10501.554 10476.105 10469.235
##  [8] 10393.676 10595.286 10327.068 10093.760  7542.163  7490.357  6498.926
## [15]  6958.481  7286.670  7900.865  7794.266  8611.109  8880.348
```

```r
plot(1:20, cost_all, type='line', xlab='# of Counters Open', ylab='Estimated Cost', main='Multiple Queues w/ Shortest Selection')
```

```
## Warning in plot.xy(xy, type, ...): plot type 'line' will be truncated to
## first character
```

![](project4_files/figure-html/unnamed-chunk-5-1.png)

```r
min(cost_all)
```

```
## [1] 6498.926
```

```r
which.min(cost_all)
```

```
## [1] 14
```
