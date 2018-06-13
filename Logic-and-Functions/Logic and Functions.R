# Review if, else, while, for, sapply, lapply, mapply, apply, etc.
# Emphasis on for vs apply loops and what each type of loop does


# Function that goes through every entry in a data frame and removes NA for example
# Better than na.omit that removes entire rows

# A plotting function?
# Simulation?
# Making functions robust, commenting code, etc.

# Fun functions about infinite sums?
# Difference between starting at 0 and 1
x <- 2  # Converges for |x| < 1
n <- 100
GeometricSeries <- function(x, n, plot = TRUE){
  sum <- cumsum(x^(0:n))
  if(plot){
    plot(1:length(sum), sum)
  }
  return(sum)
}

Euler <- function(n, plot = TRUE){
  Result <- sapply(1:n, function(i){
    value <- (1 + (1/i))^i
    return(value)
  })
  if(plot){
    plot(1:n, Result)
    abline(h = 2.7182818281828, col = "green")
  }
  return(Result)
}

# Fibonacci recursion (slow-ish)
# Use generating functions to find the nth Fibonacci (yay!)

n <- 1000000
x <- rnorm(n)
y <- rep(NA, n)
ForLoopsSuck <- function(x, y){
  for(i in 1:length(x)){
    y[i] <- x[i] + 1
  }
}
system.time(ForLoopsSuck(x, y))
system.time(y <- x + 1)


setwd("C:/Users/Christopher/Desktop/Statistics Club/Logic and Functions/")
data <- readRDS("Fibonacci data")

Fibonacci = function(n){
  if (n == 0){
    return(0)
  } else if (n == 1){
    return(1)
  } else return(Fibonacci(n - 1) + Fibonacci(n - 2))
}

FibonacciTimer <- function(start, end){
  Result <- sapply(start:end, function(i){
      num <- round(Fibonacci(i))
      time <- system.time(Fibonacci(i))[1:3]
    return(c(num, time))
  })
  Result <- as.data.frame(Result)
  rownames(Result) <- c("Fibonacci number", rownames(Result)[2:4])
  colnames(Result) <- start:end
  return(Result)
}

data <- FibonacciTimer(0, 35)
saveRDS(data, "Fibonacci data")

plot(0:(ncol(data)-1), data["elapsed", ],
     main = expression("Time vs. "*n^th*" Fibonacci number"),
     xlab = "Index (n)",
     ylab = "Time elapsed (seconds)")


plot(0:(ncol(data)-1), data["Fibonacci number", ],
     main = "Fibonacci number vs Index",
     xlab = "Index (n)",
     ylab = "Time elapsed (seconds)")




DataSampler = function(data){
  # Take a random sample from your data
  #
  # Args:
  #   data: data to be subsetted
  #
  # Returns:
  #   A random subset of your data (adjust probabilities as needed)
  row.sample = sapply(1:nrow(data), function(i){
    decision = sample(c(TRUE, FALSE), 1, prob = c(1/100, 99/100))
    return(decision)
  })
  newdata <- data[c(row.sample), ]
  # Optional row name cleaning
  rownames(newdata) <- 1:nrow(newdata)
  return(newdata)
}




# For example, let's write a function that calculates the mean between pairs of values in airmiles:
airmiles
#We will ignore the time series aspect of the data and focus on the numeric values

PairMean = function(data){
  Result = sapply(1:(length(data)-1), function(i){
    pair.mean = sum(data[i+1], data[i]) / 2
    return(pair.mean)
  })
  return(Result)
}

# Now we have a function that can be called. We could have done this example using only the sapply loop, but creating a function allows us to call it for other data not limited to only airmiles.


CardValue <- rep(c("A", 2:10, "J", "Q", "K"), times = 4)
CardSuit <- rep(c("S", "C", "H", "D"), each = 13)
CardDeck <- t(rbind(CardValue, CardSuit))

DrawTwoCardsPair = function(n){
  # Draws n card pairs
  #
  # Args:
  #   n: number of card pairs desired
  #
  # Returns:
  #   Probability of drawing a pair
  Results <- sapply(1:n,function(i) {
    TwoCards <- CardDeck[sample(1:52, 2, replace = FALSE), ]
    TorF <- ifelse (TwoCards[1, "CardValue"] == TwoCards[2, "CardValue"], TRUE, FALSE)
    return(TorF)
  })
  ProbOfPair <- sum(Results) / length(Results)
  return(ProbOfPair)
}



pois.sample <- function(n, vector.length, parameter) {
  # Samples from a Poisson distribution, plots the sample means, then overlays a normal distribution.
  #
  # Args:
  #   n: number of samples desired
  #   vector.length: number of observations per sample
  #   parameter: desired parameter of the Poisson distribution
  #
  # Returns:
  #   Well, nothing. This function plots.
  Result = sapply(1:n, function(i) {
    poisson.sample <- rpois(vector.length, parameter)
    sample.mean <- mean(poisson.sample)
    return(sample.mean)
  })
  hist(Result, prob = TRUE, main = "Histogram of Sample Means", xlab = "Sample Means")
  x <- seq(parameter - 3 * sd(poisson.sample), parameter + 3 * sd(poisson.sample), length = 1000)
  y <- dnorm(x, mean = parameter, sd = sqrt(parameter) / sqrt(vector.length))
  lines(x, y, col = "blue")
}



ConfIntervals <- function(n, vector.length, true.mean, standard.deviation) {
  # Calculates confidence intervals from a normal distribution given parameters
  #
  # Args:
  #   n: number of confidence intervals desired
  #   vector.length: number of observations per confidence interval
  #   true.mean: true mean of the normal distribution
  #   standard.deviation: true standard deviation of the normal distribution
  #
  # Returns:
  #   A 2 x n data frame of confidence interval bounds.
  Result <- sapply(1:n, function(i) {
    sample.vector <- rnorm(vector.length, true.mean, standard.deviation)
    error <- qnorm(1 - (alpha / 2)) * (sd(sample.vector) / sqrt(length(sample.vector)))
    CI <- c(mean(sample.vector) - error, mean(sample.vector) + error)
    return(CI)
  })
  return(Result)
}


PlotIntervals <- function(data, true.mean) {
  # Plots the input 2 x n confidence interval data frame
  #
  # Args:
  #   data: Data frame of size 2 x n of confidence intervals from ConfIntervals
  #
  # Returns:
  #   A useless vector that will not be seen. This function does plotting.
  lower <- min(data) - (max(data) - min(data)) * 0.5
  upper <- max(data) + (max(data) - min(data)) * 0.25
  plot(1, true.mean, type = "n", xlab = "Index", ylab = "Confidence Intervals",
       xlim = c(1, ncol(data)), ylim = c(lower, upper))
  abline(h = true.mean)
  Result <- sapply(1:ncol(data), function(i) {
    if (data[1, i] <= true.mean && true.mean <= data[2, i]) {
      points(i, data[1, i], col = "green", pch = 16)
      points(i, data[2, i], col = "green", pch = 16)
      segments(x0 = i, y0 = data[1, i], x1 = i, y1 = data[2, i], col = "green")
    } else {
      points(i, data[1, i], col = "red", pch = 16)
      points(i, data[2, i], col = "red", pch = 16)
      segments(x0 = i, y0 = data[1, i], x1 = i, y1 = data[2, i], col = "red")
    }
    return("")
  })
  Result2 <- sapply(1:ncol(data), function(i) {
    prop.vector = data[1, i] <= true.mean && true.mean <= data[2, i]
    return(prop.vector)
  })
  prop.captured <- paste0(signif((mean(Result2) * 100), digits = 6),
                          "% of intervals capture true mean")
  legend(x = "bottomleft", legend = c("Captures true mean", 
                                      "Does not capture true mean", prop.captured), pch = 15, 
         col = c("green", "red", "transparent"), cex = 1.3)
}



RussianRoulette = function(data){
  # F's your data up
  #
  # Args:
  #   data: Tell me what to shoot boss
  #
  # Returns:
  #   Something similar to a Cadillac after a drive by
  primed.gun = sapply(1:ncol(data), function(j){
    load.gun = sapply(1:nrow(data), function(i){
      is.it.dead = sample(c("It dead.", "How did it miss"), 1, prob = c(1/10, 9/10))
      return(is.it.dead)
    })
    return(load.gun)
  })
  # Firing holes into the data
  indices = which(primed.gun == "It dead.", arr.ind = TRUE)
  replace(data, indices, values = NA)
}


OutlierFinder = function(data, type){
  # Finds outliers in the input data and is robust to dirty data.
  #
  # Args:
  #   data: Input data, can be a vector or data frame
  #   type: Is your data a vector or a data frame?
  #
  # Returns:
  #   Outliers in your data
  if(type == "vector"){
    outlier.range = c(quantile(data, 0.25, na.rm = TRUE) - (1.5 * IQR(data, na.rm = TRUE)), quantile(data, 0.75, na.rm = TRUE) + (1.5 * IQR(data, na.rm = TRUE)))
    clean.data = data[!is.na(data)]
    outliers = c(subset(clean.data, subset = clean.data < outlier.range[1]), subset(clean.data, subset  = clean.data > outlier.range[2]))
    return(outliers)
  }
  if(type == "data frame"){
    sapply(1:ncol(data), function(i){
      outlier.range = c(quantile(data[, i], 0.25, na.rm = TRUE) - (1.5 * IQR(data[, i], na.rm = TRUE)), quantile(data[, i], 0.75, na.rm = TRUE) + (1.5 * IQR(data[, i], na.rm = TRUE)))
      outliers = c(subset(data[, i], subset = data[, i] < outlier.range[1]), subset(data[, i], subset  = data[, i] > outlier.range[2]))
      return(outliers)
    })
  }
}