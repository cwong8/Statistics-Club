# PRNG manipuation
set.seed(1234)

RussianRoulette = function(data){
  # Load up the bullets
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

ZimbabweInflation = function(data){
  # Randomly select values
  primed.gun = sapply(1:ncol(data), function(j){
    load.gun = sapply(1:nrow(data), function(i){
      blow.up = sample(c("Now it's a 100-Trillion-Dollar Bill", "German hyperinflation", "Meh"), 1, prob = c(2/50, 6/50, 42/50), replace = TRUE)
      return(blow.up)
    })
    return(load.gun)
  })
  # More inflation than a balloon
  indices = which(primed.gun == "Now it's a 100-Trillion-Dollar Bill", arr.ind = TRUE)
  indices.hyper = which(primed.gun == "German hyperinflation", arr.ind = TRUE)
  replace(data, indices, values = data[indices]*30)
  replace(data, indices.hyper, values = data[indices.hyper]*15)
}


OutlierFinder = function(data, type){
  # Robust against dirty data
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

ToInfinityAndBeyond = function(data){
  # Randomly select values
  primed.gun = sapply(1:ncol(data), function(j){
    load.gun = sapply(1:nrow(data), function(i){
      blow.up = sample(c("Buzz Lightyear'd", "Meh"), 1, prob = c(1/50, 49/50), replace = TRUE)
      return(blow.up)
    })
    return(load.gun)
  })
  # More inflation than a balloon
  indices = which(primed.gun == "Buzz Lightyear'd", arr.ind = TRUE)
  replace(data, indices, values = data[indices] / 0)
}

Fibonacci = function(n){
  if (n == 1){
    return(0)
  } else if (n == 2){
    return(1)
  } else return(Fibonacci(n - 1) + Fibonacci(n - 2))
}