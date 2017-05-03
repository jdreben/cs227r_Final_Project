library(tigris)
library(acs)
library(dplyr)
library(leaflet)
library(htmlwidgets)

options(scipen=999)

## Laplace Mechanism Implementation
source("./Laplace.R")

# Procedure
census_grid <- commute$id
commute_range <- 0:100
eps_commute <- 1

# Commute Dist
commute <- read.csv('./commute_dist.csv', header=TRUE)

# second median method
rank <- function(X, o) {
  dominated <- 0
  for(element in X) {
    if(element < o) {
      dominated <- dominated + 1
    }
  }
  return(dominated)
}

# started reading through http://www4.ncsu.edu/~tyu/pubs/icde-15.pdf
# and began utilizing their implementation insights 
median_score <- function(d_i, r) {
  i = rank(d_i, r)

  d_prime <- d_i
  if(i < m) {
    d_prime[i:m-i] <- d_prime[i:m-i] + (m-i) * sample(c(-1, 1), 1)
  }
  if(i > m) {
    d_prime[i-m:i] <- d_prime[i-m:i] + (m-i) * sample(c(-1, 1), 1)
  }

  n = length(d_i)
  m = rank(d_i, r)
  median_rank = floor(n / 2)
  sorted = sort(d_i)
  
  A <- d_prime

  # initial method
  # find A where median(A) == r
  return(-min(length(xor(d_i, A))))
}

# efficient_perturbations <- function(d_i, r) {
#   # need to find a way to more efficiently generate noisy outputs
#   # for all possible As in R where median == r
#.  # return(As)
# }

R <- 1:100 # range of candidate median values

expo_median <- function(d, score_function, epsilon) {
    probabilities <- exp(epsilon / 2 * score_function(d, R))
    dpmedian <- sample(commute_range, 1, prob=probabilities)
    return(dpmedian)
}

gen_expo_synth_data <- function(numsamples, median) {
  lambda <- median / log(2)
  exponential_distribution <- function(x) {
    return(lambda * exp(-1 * lambda * x))
  }
  probabilities <- exponential_distribution(median)
  return(sample(R, numsamples, prob=probabilities, replace=TRUE))
}

# generate random uniform numcases numbers from min to max
sample_uniformly <- function(numcases, min, max) {
  return(as.integer(runif(numcases, min, max+1))) 
}

find_percentiles <- function(x) {
  return(quantile(x, probs = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95)))
}

# cast Census data into specific users with specific distances but same expectation across bins
aggregate_user_commutes <- function() {
  all_user_commutes <- data.frame(home = numeric(0), commute = numeric(0))

  for (cell_id in census_grid) {

    cell_users <- data.frame(home = numeric(0), commute = numeric(0))

    # Since Census / OnTheMap artificially binned this data, first I split 
    # it uniformly then I recount the separated distances

    num_users = commute[commute$id==cell_id,]['s000'][1,] # Total number of jobs
    less_than_ten <- commute[commute$id==cell_id,]['s000_d0'][1,] # Total number of jobs, Less than 10 miles
    ten_to_twentyfour <- commute[commute$id==cell_id,]['s000_d1'][1,] # Total number of jobs, 10 - 24 miles
    twentyfive_to_fifty <- commute[commute$id==cell_id,]['s000_d2'][1,] # Total number of jobs, 25 - 50 miles
    more_than_fifty <- commute[commute$id==cell_id,]['s000_d3'][1,] # Total number of jobs, More than 50 miles

    # cast
    if (less_than_ten != 0) { 
      for(dist in sample_uniformly(less_than_ten, 0, 9)) 
      { cell_users[nrow(cell_users)+1,] <- c(cell_id, dist) }
    } else {
      # per their suggestion
      cell_users[nrow(cell_users)+1,] <- c(cell_id, 0)
      cell_users[nrow(cell_users)+1,] <- c(cell_id, 0.1)
    }
    if (ten_to_twentyfour != 0) { 
      for(dist in sample_uniformly(ten_to_twentyfour, 10, 24)) 
      { cell_users[nrow(cell_users)+1,] <- c(cell_id, dist) }
    } else {
      # per their suggestion
      cell_users[nrow(cell_users)+1,] <- c(cell_id, 0)
      cell_users[nrow(cell_users)+1,] <- c(cell_id, 0.1)
    }
    if (twentyfive_to_fifty != 0) { 
      for(dist in sample_uniformly(twentyfive_to_fifty, 25, 49)) 
      { cell_users[nrow(cell_users)+1,] <- c(cell_id, dist) }
    } else {
      # per their suggestion
      cell_users[nrow(cell_users)+1,] <- c(cell_id, 0)
      cell_users[nrow(cell_users)+1,] <- c(cell_id, 0.1)
    }
    if (more_than_fifty != 0) { 
      for(dist in sample_uniformly(more_than_fifty, 50, 99)) 
      { cell_users[nrow(cell_users)+1,] <- c(cell_id, dist) }
    } else {
      # per their suggestion
      cell_users[nrow(cell_users)+1,] <- c(cell_id, 0)
      cell_users[nrow(cell_users)+1,] <- c(cell_id, 0.1)
    }

    # all possible 
    R <- unique(unlist(all_user_commutes, use.names = FALSE))
    

    # differentially private commute distribution
    frequencies <- table(cell_users)
    sigma <- sum(frequencies)
    normalized_frequencies <- frequencies / sigma ## THIS IS D_i

    # step 1
    dpmedian <- expo_median(normalized_frequencies, median_score, eps_commute / 2)

    # step 2
    synth_data <- gen_expo_synth_data(num_users, dpmedian)
    bins <- find_percentiles(synth_data)

    # Add Laplace noise for true counts in new d.p. bins
    left_edge <- 0
    for (right_edge in bins) {
      true_count <- sum(cell_users[cell_users$commute >= left_edge & cell_users$commute < bin]) 
      noisy_count <- slaplace(true_count, 2, eps_commute / 2)
    }

    # save to (D)
    all_user_commutes <- rbind(all_user_commutes, cell_users)

  }
  return all_user_commutes
}



