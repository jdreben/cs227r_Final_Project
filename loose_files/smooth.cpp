#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;
using namespace std;
// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
double our_max(std::vector<double> input, int d){
  double max=0.0;
  for(int i=0;i<d;i++)
    if(max<input[i])
      max=input[i];
    return max;
}
 
double our_min(std::vector<double> input, int d){
  double min=DBL_MAX;
  for(int i=0;i<d;i++)
    if(min>input[i])
      min=input[i];
    return min;
}
 
 
//' @title Monotonicity enforcement
//' @description When CDFs get out of line, we call the enforcer
//' @param x A numeric vector to be enforced
//' @export
//' @return A monotonized vector
// [[Rcpp::export]]
NumericVector Smooth(NumericVector x) {
  NumericVector data=as<NumericVector>(x);
  int d=data.size();
  std::vector<double> input(d);
  std::vector<double> output(d);
  
  for(int i=0;i<d;i++)
    input[i]=data[i];
   
  std::vector<double> prefix(d+1);
  

  prefix[0]=0.0;
  for(int i=0;i<d;i++){
    prefix[i+1]=prefix[i]+input[i];
  }

  std::vector<std::vector<double> > means(d, std::vector<double>(d));
  
  for(int i=0;i<d;i++)
    for(int j=i;j<d;j++)
      means[j][i]=(prefix[j+1]-prefix[i])/(double)(j-i+1);
   
   std::vector<double> maxv(d);
  
  for(int j=0;j<d;j++)
    maxv[j]=our_max(means[j],j+1);
   
  for(int k=0;k<d;k++){
    std::vector<double> temp(d-k);
   
    for(int j=k;j<d;j++){
      temp[j-k]=maxv[j];
    }
    output[k]=std::max(min(our_min(temp,d-k),1.0),0.0);
    
  }
 
   
  NumericVector out(d);
   
  for(int i=0;i<d;i++)
    out[i]=output[i];
   
  return out;
   
}
 
 
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//