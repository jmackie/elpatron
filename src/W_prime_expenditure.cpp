#include <Rcpp.h>
#include <array>
using namespace Rcpp;


// [[Rcpp::export]]
std::vector<double> WEXPEND(NumericVector t, NumericVector P, double CP)
{
  if (t.size() != P.size())
  {
    stop("Inputs are different lengths.");
  }

  const int N = P.size();

  // supra- and sub-CP sections
  std::vector<int> section(N);
  for (int i = 0; i < N; ++i)
  {
    /*
     * Sections:
     *  0 == recovery (sub-CP)
     *  1 == work     (supra-CP)
     *  2 == NA       (to prevent errors)
     */
    section[i] = NumericVector::is_na(P[i]) ? 2 : P[i] <= CP ? 0 : 1;
  }

  // arrays required for W' expenditure calculation
  std::vector<double> dt(N), dWexp(N), tu(N), tau(N);

  for (int i = 1; i < N; ++i)  // NOTE: starts at the *second* element
  {
    // delta time
    // ----------
    if (NumericVector::is_na(t[i]))
    {
      stop("NAs not allowed in time values.");
    }
    else
    {
      dt[i] = t[i] - t[i - 1];
    }

    // initial check
    // -------------
    // NA power value: retain previous value
    if (NumericVector::is_na(P[i]))
    {
      dWexp[i] = dWexp[i - 1];
    }
    // supra-CP section
    // ----------------
    else if (section[i])
    {
      // in case there is a long pause (> 10 s), and data starts
      // recording with non-zero power, use next delta time value.
      if (dt[i] > 10)
      {
        dWexp[i] = dt[i + 1] * (P[i] - CP);
      }
      else
      {
        dWexp[i] = dt[i] * (P[i] - CP);
      }

      // *delta* W' expended is cumulative; check previous value
      if (section[i - 1])
      {
        dWexp[i] += dWexp[i - 1];
      }
    }
    // sub-CP section
    // --------------
    else
    {
      tu[i]  = tu[i - 1] ? dt[i] + tu[i - 1] : dt[i];
      tau[i] = 546 * std::exp(-0.01 * (CP - P[i])) + 316;
    }
  }

  // generate W' expenditure from the above
  std::vector<double> Wexp(N);
  double last_work = 0, last_recovery = 0;

  for (int i = 0; i < N; ++i)
  {
    // supra-CP section
    // ----------------
    if (section[i])
    {
      Wexp[i] = last_work = dWexp[i] + last_recovery;
    }

    // sub-CP section
    // ----------------
    else if (last_work)  // anything to recover?
    {
      Wexp[i] = last_recovery = last_work * std::exp(-tu[i] / tau[i]);
    }
  }

  return Wexp;  // Joules
}
