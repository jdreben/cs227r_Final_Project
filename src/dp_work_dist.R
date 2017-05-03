library(tigris)
library(acs)
library(dplyr)
library(leaflet)
library(htmlwidgets)

options(scipen=999)
## Laplace Mechanism Implementation
library("Rmpfr")
library("gmp")
library("plyr")

BinToDec <- function(x, y) 
  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))*2^y
clamp <- function(x, u, l){
  if(x>u)
    x<-u
  else if(x<l)
    x<-l
  else
    x
}
epsilon <- function(eps,l,u){
  B<-(u-l)
  eta<-2^-53
  pre<-mpfr(B*eta*exp(16)/eps,52)
  y<-asNumeric(ceiling(log2(pre)))
  pre<-2^y
  eprime<-log((exp(eps*(2*B*eta+pre))-exp(-3.1*B*eta*eps))/(exp(pre*eps)-1))*2^4
  eprime
}

#' @title Snapping Mechanism
#' @description [Ilya Mironov. "On Significance of the Least Significant Bits For Differential Privacy"]
#' @param x The original value
#' @param s The sensitivity
#' @param eps epsilon value for DP
#' @param l The minimum possible value
#' @param u The maximum possible value
#' @return The x value perturb with Laplace noise of scale s/eps  
#' @export
#' @examples slaplace(50,1,1)
slaplace <- function(x, s, eps, l=-2^20, u=2^20){
  S<-c(1,-1)
  l<-l/s
  u<-u/s
  B<-(u-l)
  eta<-2^-53
  pre<-mpfr(B*eta*exp(16)/eps,52)
  x<-x/s
  y<-asNumeric(ceiling(log2(pre)))
  eps<-(eps-epsilon(eps,l,u))
  clamp(round_any(clamp(x,u,l)+asNumeric(1.0/eps*log(mpfr(BinToDec(c(1,rbinom(52,1,0.5)),-rgeom(1,0.5)-53),52))*sample(S,1)),2^y)*s,u*s,l*s)
}
## 

# work dist
work <- read.csv('./work_dist.csv', header=TRUE)

eps_work <- 1
global_sensivity <- 2
# lowest_home_count = 1
# highest_home_count = 2561

# census <- read.csv('~/Dropbox/Harvard/Senior Spring/CS227/Final Project/src/census_ids.csv')
# census_grid <- census[,1] # cast to numeric
census_grid = work$id

# new_home_vals = vector('list', length(census_grid))
# old_home_vals = vector('list', length(census_grid))

i <- 0
count <- 0
for (cell_id in census_grid) 
{	
	i <- i + 1
	# print(class(cell_id))
	# print(toString(cell_id))
	# print("cell is ", toString(i))
	print(cell_id)
	# print(home[row.names('id')==toString(cell_id)])
	tryCatch(
	{
		# print(val)
		val <- work[work$id==cell_id,][3][1,]
		# old_home_vals[i - 1] <- val
		new_val <- slaplace( val, global_sensivity, eps_work) #, eps_home, lowest_home_count, highest_home_count)
		work[work$id==cell_id,][3][1,] <- new_val
		# new_home_vals[i - 1] <- new_val
		# # print(new_count)
		# count <- count + new_count

		# home_cdf[i] <- count 
	}, error=function(e) {
		# print("No data")
		# home_cdf[i] <- count
	})
}

write.csv(file='./laplace_work_dist.csv', x=work, row.names=F)
print("got to end")
