#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<double> DIFF(NumericVector x)
{
  double n = x.size(), i;
  std::vector<double> out(n);
  out[0] = 0; // Just in case.
  for (i = 1; i < n; ++i)
  {
    out[i] = x[i] - x[i - 1];
  }
  return out;
}

// [[Rcpp::export]]
std::vector<double> CUMSUM(NumericVector x)
{
  double n = x.size(), i;
  std::vector<double> out(n);
  out[0] = x[0];
  for (i = 1; i < n; ++i)
  {
    out[i] = NumericVector::is_na(x[i]) ? out[i - 1] : x[i] + out[i - 1];
  }
  return out;
}
