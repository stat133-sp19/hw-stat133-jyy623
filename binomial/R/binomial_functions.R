# The title is check_prob.
# This function tells if a input is a valid probabilit or not.
# The input parameter is a numeric value p.
# The output is logical value TRUE.
check_prob = function(p){
  if(class(p) == "numeric" & p >= 0 & p <= 1){
    return(TRUE)
  } else {
    stop("p has to be a number between 0 and 1")
  }
}

# The title is ckeck_trials.
# This function tells if a input is a valid value for number of trails or not.
# The input parameter is a numeric value trails.
# The output is logical value TRUE.
check_trials = function(trials){
  if(class(trials) != "numeric"){
    stop("invalid trials value")
  } else if(trials %% 1 != 0){
    stop("invalid trials value")
  } else{
    return(TRUE)
  }
}

# The title is ckeck_success.
# This function tells if a input is a valid value for number of success or not.
# The input parameters are a numeric value trails and a numeric value success
# The output is logical value TRUE.
check_success = function(trials, success){
  if(check_trials(trials) == FALSE){
    stop("invalid trial values")
  } else if(mode(success) != "numeric"){
    stop("invalid success values")
  } else if(sum(success %% 1 == rep(0, length(success))) != length(success)){
    stop("invalid success values")
  }else if(sum(trials < success) > 0){
    stop("success can't be greater than trials")
  } else{
    return(TRUE)
  }
}

# The title is aux_mean.
# This function tells the mean value of a binomial distribution.
# The input parameters are a numeric value trials and a numeric value porb
# The output is a numeric value of the mean of the distribution.
aux_mean = function(trials = 10, prob = 0.3){
  mean = trials*prob
  return(mean)
}

# The title is aux_variance.
# This function tells the variance of a binomial distribution.
# The input parameters are a numeric value trials and a numeric value porb
# The output is a numeric value of the variance of the distribution.
aux_variance = function(trials = 10, prob = 0.3){
  variance = trials*prob*(1-prob)
  return(variance)
}

# The title is aux_mode.
# This function tells the mode of a binomial distribution.
# The input parameters are a numeric value trials and a numeric value porb
# The output is a numeric value of the mode of the distribution.
aux_mode = function(trials = 10, prob = 0.3){
  m = trials*prob + prob
  if(m %% 1 == 0){
    return(c(m, m-1))
  } else {return(floor(m))
  }
}


# The title is aux_skewness.
# This function tells the skewness of a binomial distribution.
# The input parameters are a numeric value trials and a numeric value porb
# The output is a numeric value of the skewness of the distribution.
aux_skewness = function(trials = 10, prob = 0.3){
  sd = sqrt(aux_variance(trials, prob))
  skewness = (1 - 2 * prob) / sd
  return(skewness)
}

# The title is aux_kurtosis.
# This function tells the kurtosis of a binomial distribution.
# The input parameters are a numeric value trials and a numeric value porb
# The output is a numeric value of the kurtosis of the distribution.
aux_kurtosis = function(trials = 10, prob = 0.3){
  kurtosis = (1 - 6 * prob * (1-prob))/aux_variance(trials, prob)
  return(kurtosis)
}
aux_kurtosis()

#' @title bin_choose
#' @description  this function calculates the number of combinatons in which k successes can occer in n trials
#' @param n numeric vaule of number of trials
#' @param k numeric value of number of success
#' @export
#' @examples
#' Calculate the number of different ways to get 2 successes out of 5 trails.
#' bin_choose(n = 5, k = 2)
bin_choose = function(n = 5, k){
   if(sum(k > n) > 0){
    stop("k cannot be greater than n")
  } else{
    nfac = factorial(n)
    kfac = factorial(k)
    fac = factorial(n-k)
    choose = nfac/(kfac*fac)
    return(choose)
  }
}
bin_choose(5,1:5)

#' @title bin_probability
#' @description  this function calculates the probabilty of getting k successes in n trails with prob p of success
#' @param success numeric vaule of number of success
#' @param trials numeric value of number of trials
#' @param prob numeric value of probability of success of each trial
#' @export
#' @examples
#' Calculate the probability of have 2 successes in 5 trials with a probabiity 0.5 of success.
#' bin_probability(success = 2, trials = 5 prob = 0.5)
bin_probability = function(success, trials, prob){
  if(check_success(trials = trials, success = success) != TRUE){
    stop("invalid success values")
  } else if(check_trials(trials) != TRUE){
    stop("invalid trial value")
  } else if(check_prob(prob) != TRUE){
    stop("invalid prob value")
  } else{
  choose1 = choose(trials, success)
  probability = choose1 * prob ^ success * (1 - prob)^(trials - success)
  return(probability)
 }
}

#' @title bin_bin_distribution
#' @description  this function calculates the probabilty of getting successes in n trails with prob p of success
#' @param trials numeric value of number of trials
#' @param prob numeric value of probability of success of each trial
#' @export
#' @examples
#' Generate a dataframe of differnet number of successes and the corresponding probability within 5 trials of 0.5
#' probability of success.
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution = function(trials, prob){
   success1 = 0:trials
   probability = bin_probability(success = success1, trials = trials, prob = prob)
   df = data.frame(success = success1, probability = probability)
   class(df) = c("bindis", "data.frame")
   return(df)
}
#' @export
plot.bindis = function(x, ...){
  barplot(height = x$probability, names.arg = x$success, xlab = "successes", ylab = "probability")
}

#' @title bin_bin_cumulative
#' @description  this function calculates the probabilty of getting successes in n trails with prob p of success
#' and the cumulative probability
#' @param trials numeric value of number of trials
#' @param prob numeric value of probability of success of each trial
#' @export
#' @examples
#' Generate a dataframe of differnet number of successes and the cumulative probability within 5 trials of 0.5
#' probability of success.
#'   bin_distribution(trials = 5, prob = 0.5)
bin_cumulative = function(trials, prob){
  success2 = 0:trials
  probability2 = bin_probability(success = success2, trials = trials, prob = prob)
  cumulative = rep(0, (trials + 1))
  for (i in 0:trials) {
    cumulative[(i+1)] = sum(probability2[1:(i+1)])
  }
  df1 = data.frame(
    success = success2, probability = probability2, cumulative = cumulative)
  class(df1) = c("bincum", "data.frame")
  return(df1)
}
bin_cumulative(trials = 5, prob = 0.5)
#' @export
plot.bincum = function(x,...){
  plot(x = x$success, y = x$cumulative, type = "o", xlab = "successes", ylab = "probability")
}

#' @title bin_variable
#' @description this function returns a list with named elements trials and prob
#' @param trials numeric value of number of trials
#' @param prob numeric value of probability of success of each trial
#' @export
#' @examples
#' Generate a list of the number of trials and the probability of success.
#' bin_variable(trials = 5, prob = 0.5)
bin_variable = function(trials, prob){
  if(check_prob(prob) != TRUE){
    stop("invalid probability")
  } else if(check_trials(trials) != TRUE){
    stop("invalid trials")
  } else{
    list = list(trials = trials, prob = prob)
    class(list) = "binvar"
    return(list)
  }
}

#' @export
print.binvar = function(x, ...) {
  cat('"Binomial variable"\n\n')
  cat("Parameters\n")
  cat("- number of trials:", x$trials,"\n")
  cat("- prob of success:", x$prob, "\n")
  invisible(x)
}

#' @export
summary.binvar = function(x, ...){
  list(trials = x$trials,
       prob = x$prob,
       mean = aux_mean(x$trials, x$prob),
       variance = aux_variance(x$trials, x$prob),
       mode = aux_mode(x$trials, x$prob),
       skewness = aux_skewness(x$trials, x$prob),
       kurtosis = aux_kurtosis(x$trials, x$prob))
}

#' @export
print.summary.binvar = function(x, ...){
  cat('"Summary Binomial"\n\n')
  cat("Parameters")
  cat("- number of trials :", x$trials,"\n")
  cat("- prob of success : ", x$prob, "\n\n")
  cat("Measutes\n")
  cat("- mean :", aux_mean(x$trials, x$prob), "\n")
  cat("- variance :",aux_variance(x$trials, x$prob),"\n")
  cat("- mode :", aux_mode(x$trials, x$prob), "\n")
  cat("- skewness :", aux_skewness(x$trials, x$prob), "\n")
  cat("- kurtosis :", aux_kurtosis(x$trials, x$prob), "\n")
}

#' @title bin_mean
#' @description this function returns the mean of the binomial distribution
#' @param trials numeric value of number of trials
#' @param prob numeric value of probability of success of each trial
#' @export
#' @examples
#' Calculate the mean of the binomial distribution with 5 trials and probability of
#' 0.5 of success.
#' bin_mean(trials = 5, prob = 0.5)
bin_mean = function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  bin_mean = aux_mean(trials, prob)
  return(bin_mean)
}

#' @title bin_variance
#' @description this function returns the variance of the binomial distribution
#' @param trials numeric value of number of trials
#' @param prob numeric value of probability of success of each trial
#' @export
#' @examples
#' Calculate the variance of the binomial distribution with 5 trials and probability of
#' 0.5 of success.
#' bin_variance(trials = 5, prob = 0.5)
bin_variance = function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  bin_variance = aux_variance(trials, prob)
  return(bin_variance)
}

#' @title bin_mode
#' @description this function returns the mode of the binomial distribution
#' @param trials numeric value of number of trials
#' @param prob numeric value of probability of success of each trial
#' @export
#' @examples
#' Calculate the mode of the binomial distribution with 5 trials and probability of
#' 0.5 of success.
#' bin_mode(trials = 5, prob = 0.5)
bin_mode = function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  bin_mode = aux_mode(trials, prob)
  return(bin_mode)
}

#' @title bin_skewness
#' @description this function returns the skewness of the binomial distribution
#' @param trials numeric value of number of trials
#' @param prob numeric value of probability of success of each trial
#' @export
#' @examples
#' Calculate the skewness of the binomial distribution with 5 trials and probability of
#' 0.5 of success.
#' bin_skewness(trials = 5, prob = 0.5)
bin_skewness = function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  bin_skewness = aux_skewness(trials, prob)
  return(bin_skewness)
}

#' @title bin_kurtosis
#' @description this function returns the kurtosis of the binomial distribution
#' @param trials numeric value of number of trials
#' @param prob numeric value of probability of success of each trial
#' @export
#' @examples
#' Calculate the kurtosis of the binomial distribution with 5 trials and probability of
#' 0.5 of success.
#' bin_kurtosis(trials = 5, prob = 0.5)
bin_kurtosis = function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  bin_kurtosis = aux_kurtosis(trials, prob)
  return(bin_kurtosis)
}

