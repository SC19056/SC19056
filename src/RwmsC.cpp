#include <Rcpp.h>
using namespace Rcpp;
//' @title A random walk Metropolis sampler via Rcpp
//' @description A random walk Metropolis sampler Rcpp
//' @param sigma  Standard deviation of normal distribution
//' @param x_0 The initial value of iteration
//' @param n  The number of samples
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' x<-RwmsC(2,5,1e5)
//' hist(x)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix RwmsC(double sigma, double x_0,int N) {
  NumericMatrix x(N,2);
  NumericVector U=runif(N,0,1);
  x(0,0)=x_0;x(0,1)=0;
  double y=0;double u=0;double c1;double c2;
  for (int i=1;i < N;i++){
    y = rnorm(1,x(i-1,0),sigma)[0];
    u = U[i];
    c1=0.5*exp(-abs(y));
    c2=0.5*exp(-abs(x(i-1,0)));
    if (u <= c1/c2){
      x(i,0) = y;
      x(i,1)=1;
    }
    else{
      x(i,0)=x(i-1,0);
      x(i,1)=0;
    }
  }
  return x;
}
