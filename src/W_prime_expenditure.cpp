#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<double> WEXPEND(NumericVector t, NumericVector P, double CP)
{
  if (t.size() != P.size())
  {
    stop("Inputs are different lengths.");
  }

  double n = P.size(), i;

  // Vector of supra- and sub-CP sections.
  std::vector<double> section(n);
  for (i = 0; i < n; ++i)
  {
    /*
     * # Sections
     * 0 == recovery
     * 1 == work (supra-CP)
     * 2 == NA (to prevent errors)
     */
    section[i] = (NumericVector::is_na(P[i])) ? 2 : (P[i] <= CP) ? 0 : 1;
  }

  // Vectors required for W' expenditure calculation.
  std::vector<double> dt(n), dWexp(n), tu(n), tau(n);

  for (i = 1; i < n; ++i) // NB: Starts at the **second** element.
  {

    // Delta time.
    if (NumericVector::is_na(t[i]))
    {
      stop("NAs not allowed in time values.");
    }
    else
    {
      dt[i] = t[i] - t[i - 1];
    }

    // NA power value: retain previous value.
    if (NumericVector::is_na(P[i]))
    {
      dWexp[i] = dWexp[i - 1];
    }

    // **** Supra-CP section ****
    else if (section[i])
    {
      // In case there is a long pause (> 10 s), and data starts
      // recording with non-zero power, use next delta time value.
      if (dt[i] > 10)
      {
        dWexp[i] = dt[i + 1] * (P[i] - CP);
      }
      else
      {
        dWexp[i] = dt[i] * (P[i] - CP);
      }

      // *Delta* W' expended is cumulative; check previous value.
      if (section[i - 1])
      {
        dWexp[i] += dWexp[i - 1];
      }
    }

    // **** Sub-CP (recovery) section ****
    else
    {
      tu[i]  = tu[i - 1] ? dt[i] + tu[i - 1] : dt[i];
      tau[i] = 546 * std::exp(-0.01 * (CP - P[i])) + 316;
    }
  }

  // Generate W' expenditure from the above.
  std::vector<double> Wexp(n);
  double Wtmp = 0;

  for (i = 0; i < n; ++i)
  {

    // **** Supra-CP section ****
    if (section[i])
    {
      Wexp[i] = dWexp[i] + Wtmp;

      // Store value.
      if (!section[i + 1])
      {
        Wtmp = Wexp[i];
      }
    }

    // **** Sub-CP (recovery) section ****
    else
    {
      if (Wtmp != 0)  // Anything to recover?
      {
        Wexp[i] = Wtmp * std::exp(-tu[i] / tau[i]);

        // Store value.
        if (section[i + 1])
        {
          Wtmp = Wexp[i];
        }
      }
    }

  }

  return Wexp;  // Joules.
}
