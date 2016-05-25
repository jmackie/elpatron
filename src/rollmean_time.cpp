#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
std::vector<double> ROLLMEAN_TIME(NumericVector x, NumericVector time, double window, bool na_rm) {

  double n = x.size(), i, win_i, sum, len;
  std::vector<double> out(n);

  // Find first index.
  int start = 0; while (time[start + 1] <= window) ++start;

  if (na_rm)
  {
    for (win_i = i = start; i < n; win_i = ++i, sum = 0, len = 0)
    {
      while(time[win_i] > (time[i] - window)) // Working backwards.
      {
        if (!NumericVector::is_na(x[win_i])) // Not empty.
        {
          sum += x[win_i];
          ++len; // Track the size of this window.
        }
        // Keep going down; stop if we hit the bottom of x.
        if (!win_i--) break;
      }
      // Did we capture anything?
      out[i] = !len ? NA_REAL : sum / len;
    }
  }

  else  // (!na_rm)
  {
    for (win_i = i = start; i < n; win_i = ++i, sum = 0, len = 0)
    {
      while(time[win_i] > (time[i] - window)) // Working backwards.
      {
        sum += x[win_i];
        ++len;

        // Keep going down; stop if we hit the bottom of x.
        if (!win_i--) break;
      }
      // Did we capture anything?
      out[i] = !len ? NA_REAL : sum / len;
    }
  }
  return out;
}
