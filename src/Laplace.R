
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