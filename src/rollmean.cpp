#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
std::vector<double> EMA_WEIGHTS(double len)
{
  double alpha = 2 / (len + 1), sum = 0;
  std::vector<double> s(len), out(len);
  for (unsigned int i = 1; i < (len + 1); ++i)
    s[i - 1] = i; // 1:len
  for (unsigned int i = 0; i < s.size(); ++i)
    sum += alpha * pow(1 - alpha, 1 - s[i]);
  for (unsigned int i = 0; i < len; ++i)
    out[i] = alpha * pow(1 - alpha, 1 - s[i]) / sum;
  return out;
}

// [[Rcpp::export]]
std::vector<double> ROLLMEAN(NumericVector x, double window, bool na_rm, bool ema)
{

  double start = window - 1, n = x.size();
  std::vector<double> out(n);

  // ------------------------------------------------- //
  if (na_rm && ema)
  {
    double len, weight_count;
    std::vector<double> weights;

    for (double i = start; i < n; ++i, len = 0)
    {
      // Loop over window: get length.
      for (double win_i = (i - start); win_i <= i; ++win_i)
      {
        if (NumericVector::is_na(x[win_i])) continue;
        else ++len; // na.rm
      }

      // Empty window.
      if (!len)
      {
        out[i] = NA_REAL;
        continue;
      }

      // Create weights for this window.
      weights = EMA_WEIGHTS(len);
      weight_count = 0; // Reset.

      // Loop over window (again): get weighted mean.
      for (double win_i = (i - start); win_i <= i; ++win_i)
      {
        if (NumericVector::is_na(x[win_i])) continue;
        else out[i] += (x[win_i] * weights[weight_count++]);
      }
    }
  }

  // ------------------------------------------------- //
  else if (na_rm && !ema)
  {
    double len, sum;
    for (double i = start; i < n; ++i, len = 0, sum = 0)
    {
      // Loop over window: get mean without NAs.
      for (double win_i = (i - start); win_i <= i; ++win_i)
      {
        if (NumericVector::is_na(x[win_i]))
        {
          continue;
        }
        else
        {
          sum += x[win_i];
          ++len;
        }
      }

      if (!len) out[i] = NA_REAL; // Empty (all NAs).
      else out[i] = sum / len;
    }
  }

  // ------------------------------------------------- //
  else if (!na_rm && ema)
  {
    double weight_count;
    std::vector<double> weights = EMA_WEIGHTS(window);
    for (double i = start; i < n; ++i, weight_count = 0)
    {
      for (double win_i = (i - start); win_i <= i; ++win_i)
      {
        out[i] += (x[win_i] * weights[weight_count++]);
      }
    }
  }
  // ------------------------------------------------- //
  else  // (!na_rm && !ema)
  {
    double sum;
    for (double i = start; i < n; ++i, sum = 0)
    {
      for (double win_i = (i - start); win_i <= i; ++win_i)
      {
        sum += x[win_i];
      }
      out[i] = sum / window;
    }
  }
  /****************************************/
  return out;
}


// std::vector<double> MEAN_WEIGHTS(double len)
// {
//   std::vector<double> out(len);
//   for (int i = 0; i < len; ++i) out[i] = 1 / len;
//   return out;
// }
