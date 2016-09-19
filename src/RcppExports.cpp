// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// DIFF
std::vector<double> DIFF(NumericVector x);
RcppExport SEXP elpatron_DIFF(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    __result = Rcpp::wrap(DIFF(x));
    return __result;
END_RCPP
}
// CUMSUM
std::vector<double> CUMSUM(NumericVector x);
RcppExport SEXP elpatron_CUMSUM(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    __result = Rcpp::wrap(CUMSUM(x));
    return __result;
END_RCPP
}
// EMA_WEIGHTS
std::vector<double> EMA_WEIGHTS(double len);
RcppExport SEXP elpatron_EMA_WEIGHTS(SEXP lenSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< double >::type len(lenSEXP);
    __result = Rcpp::wrap(EMA_WEIGHTS(len));
    return __result;
END_RCPP
}
// ROLLMEAN
std::vector<double> ROLLMEAN(NumericVector x, double window, bool na_rm, bool ema);
RcppExport SEXP elpatron_ROLLMEAN(SEXP xSEXP, SEXP windowSEXP, SEXP na_rmSEXP, SEXP emaSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type window(windowSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type ema(emaSEXP);
    __result = Rcpp::wrap(ROLLMEAN(x, window, na_rm, ema));
    return __result;
END_RCPP
}
// ROLLMEAN_TIME
std::vector<double> ROLLMEAN_TIME(NumericVector x, NumericVector time, double window, bool na_rm);
RcppExport SEXP elpatron_ROLLMEAN_TIME(SEXP xSEXP, SEXP timeSEXP, SEXP windowSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type time(timeSEXP);
    Rcpp::traits::input_parameter< double >::type window(windowSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    __result = Rcpp::wrap(ROLLMEAN_TIME(x, time, window, na_rm));
    return __result;
END_RCPP
}
// WEXPEND
std::vector<double> WEXPEND(NumericVector t, NumericVector P, double CP);
RcppExport SEXP elpatron_WEXPEND(SEXP tSEXP, SEXP PSEXP, SEXP CPSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type t(tSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type P(PSEXP);
    Rcpp::traits::input_parameter< double >::type CP(CPSEXP);
    __result = Rcpp::wrap(WEXPEND(t, P, CP));
    return __result;
END_RCPP
}
// PARSE_PWX
Rcpp::List PARSE_PWX(const char* file_path);
RcppExport SEXP elpatron_PARSE_PWX(SEXP file_pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const char* >::type file_path(file_pathSEXP);
    __result = Rcpp::wrap(PARSE_PWX(file_path));
    return __result;
END_RCPP
}
// PWX_START_TIME
Rcpp::CharacterVector PWX_START_TIME(const char* file_path);
RcppExport SEXP elpatron_PWX_START_TIME(SEXP file_pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const char* >::type file_path(file_pathSEXP);
    __result = Rcpp::wrap(PWX_START_TIME(file_path));
    return __result;
END_RCPP
}
// PARSE_TCX
Rcpp::List PARSE_TCX(const char* file_path);
RcppExport SEXP elpatron_PARSE_TCX(SEXP file_pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const char* >::type file_path(file_pathSEXP);
    __result = Rcpp::wrap(PARSE_TCX(file_path));
    return __result;
END_RCPP
}
// TCX_LAPS
Rcpp::List TCX_LAPS(const char* file_path);
RcppExport SEXP elpatron_TCX_LAPS(SEXP file_pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const char* >::type file_path(file_pathSEXP);
    __result = Rcpp::wrap(TCX_LAPS(file_path));
    return __result;
END_RCPP
}
// PARSE_GPX
Rcpp::List PARSE_GPX(const char* file_path);
RcppExport SEXP elpatron_PARSE_GPX(SEXP file_pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const char* >::type file_path(file_pathSEXP);
    __result = Rcpp::wrap(PARSE_GPX(file_path));
    return __result;
END_RCPP
}
