// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// simpsonC
arma::vec simpsonC(const arma::vec& x, const arma::mat& fx);
RcppExport SEXP _DstarM_simpsonC(SEXP xSEXP, SEXP fxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type fx(fxSEXP);
    rcpp_result_gen = Rcpp::wrap(simpsonC(x, fx));
    return rcpp_result_gen;
END_RCPP
}
// dunifc
arma::vec dunifc(const arma::vec& x, const double& a, const double& b);
RcppExport SEXP _DstarM_dunifc(SEXP xSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const double& >::type a(aSEXP);
    Rcpp::traits::input_parameter< const double& >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(dunifc(x, a, b));
    return rcpp_result_gen;
END_RCPP
}
// convolveC
arma::vec convolveC(const arma::vec& x, const arma::vec& y);
RcppExport SEXP _DstarM_convolveC(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(convolveC(x, y));
    return rcpp_result_gen;
END_RCPP
}
// convolveC2
arma::mat convolveC2(arma::mat& x, arma::mat& y);
RcppExport SEXP _DstarM_convolveC2(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(convolveC2(x, y));
    return rcpp_result_gen;
END_RCPP
}
// chisqC
double chisqC(const arma::vec& tt, const arma::vec& a, const arma::vec& b);
RcppExport SEXP _DstarM_chisqC(SEXP ttSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type tt(ttSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type a(aSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(chisqC(tt, a, b));
    return rcpp_result_gen;
END_RCPP
}
// rObjC3
double rObjC3(arma::vec& r, arma::vec& tt, arma::vec& a, arma::vec& bb, arma::vec& lenPre, arma::vec& lenPost);
RcppExport SEXP _DstarM_rObjC3(SEXP rSEXP, SEXP ttSEXP, SEXP aSEXP, SEXP bbSEXP, SEXP lenPreSEXP, SEXP lenPostSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type r(rSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type tt(ttSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type a(aSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type bb(bbSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type lenPre(lenPreSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type lenPost(lenPostSEXP);
    rcpp_result_gen = Rcpp::wrap(rObjC3(r, tt, a, bb, lenPre, lenPost));
    return rcpp_result_gen;
END_RCPP
}
// rObjC2
double rObjC2(arma::vec& r, arma::vec& tt, arma::vec& a, arma::vec& bb, arma::vec& lenPre);
RcppExport SEXP _DstarM_rObjC2(SEXP rSEXP, SEXP ttSEXP, SEXP aSEXP, SEXP bbSEXP, SEXP lenPreSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type r(rSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type tt(ttSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type a(aSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type bb(bbSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type lenPre(lenPreSEXP);
    rcpp_result_gen = Rcpp::wrap(rObjC2(r, tt, a, bb, lenPre));
    return rcpp_result_gen;
END_RCPP
}
// rObjC1
double rObjC1(arma::vec& r, arma::vec& tt, arma::vec& a, arma::vec& bb, arma::vec& lenPost);
RcppExport SEXP _DstarM_rObjC1(SEXP rSEXP, SEXP ttSEXP, SEXP aSEXP, SEXP bbSEXP, SEXP lenPostSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type r(rSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type tt(ttSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type a(aSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type bb(bbSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type lenPost(lenPostSEXP);
    rcpp_result_gen = Rcpp::wrap(rObjC1(r, tt, a, bb, lenPost));
    return rcpp_result_gen;
END_RCPP
}
// rObjC0
double rObjC0(arma::vec& r, arma::vec& tt, arma::vec& a, arma::vec& bb);
RcppExport SEXP _DstarM_rObjC0(SEXP rSEXP, SEXP ttSEXP, SEXP aSEXP, SEXP bbSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type r(rSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type tt(ttSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type a(aSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type bb(bbSEXP);
    rcpp_result_gen = Rcpp::wrap(rObjC0(r, tt, a, bb));
    return rcpp_result_gen;
END_RCPP
}
// nthMomentSC
double nthMomentSC(const arma::vec& x, const arma::vec& fx, const int& nth);
RcppExport SEXP _DstarM_nthMomentSC(SEXP xSEXP, SEXP fxSEXP, SEXP nthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type fx(fxSEXP);
    Rcpp::traits::input_parameter< const int& >::type nth(nthSEXP);
    rcpp_result_gen = Rcpp::wrap(nthMomentSC(x, fx, nth));
    return rcpp_result_gen;
END_RCPP
}
// nthCMomentSC
double nthCMomentSC(const arma::vec& x, const arma::vec& fx, const int& nth);
RcppExport SEXP _DstarM_nthCMomentSC(SEXP xSEXP, SEXP fxSEXP, SEXP nthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type fx(fxSEXP);
    Rcpp::traits::input_parameter< const int& >::type nth(nthSEXP);
    rcpp_result_gen = Rcpp::wrap(nthCMomentSC(x, fx, nth));
    return rcpp_result_gen;
END_RCPP
}
// getVarC
arma::vec getVarC(arma::mat Pdf, const arma::vec& tt, const arma::mat& mm2);
RcppExport SEXP _DstarM_getVarC(SEXP PdfSEXP, SEXP ttSEXP, SEXP mm2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type Pdf(PdfSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type tt(ttSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mm2(mm2SEXP);
    rcpp_result_gen = Rcpp::wrap(getVarC(Pdf, tt, mm2));
    return rcpp_result_gen;
END_RCPP
}
// oscCheckC
bool oscCheckC(const arma::mat& x);
RcppExport SEXP _DstarM_oscCheckC(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(oscCheckC(x));
    return rcpp_result_gen;
END_RCPP
}
// getVoss
arma::mat getVoss(arma::vec& rt, arma::mat& pars, const double& precision);
RcppExport SEXP _DstarM_getVoss(SEXP rtSEXP, SEXP parsSEXP, SEXP precisionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type rt(rtSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type pars(parsSEXP);
    Rcpp::traits::input_parameter< const double& >::type precision(precisionSEXP);
    rcpp_result_gen = Rcpp::wrap(getVoss(rt, pars, precision));
    return rcpp_result_gen;
END_RCPP
}
// imposeFixationsC
void imposeFixationsC(arma::vec& pars, const arma::mat fixed);
RcppExport SEXP _DstarM_imposeFixationsC(SEXP parsSEXP, SEXP fixedSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type pars(parsSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type fixed(fixedSEXP);
    imposeFixationsC(pars, fixed);
    return R_NilValue;
END_RCPP
}
// getPdfC
arma::mat getPdfC(arma::vec& tt, arma::mat pars, const arma::mat& mm, const bool& DstarM, const bool& oscPdf, const double& precision);
RcppExport SEXP _DstarM_getPdfC(SEXP ttSEXP, SEXP parsSEXP, SEXP mmSEXP, SEXP DstarMSEXP, SEXP oscPdfSEXP, SEXP precisionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec& >::type tt(ttSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type pars(parsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mm(mmSEXP);
    Rcpp::traits::input_parameter< const bool& >::type DstarM(DstarMSEXP);
    Rcpp::traits::input_parameter< const bool& >::type oscPdf(oscPdfSEXP);
    Rcpp::traits::input_parameter< const double& >::type precision(precisionSEXP);
    rcpp_result_gen = Rcpp::wrap(getPdfC(tt, pars, mm, DstarM, oscPdf, precision));
    return rcpp_result_gen;
END_RCPP
}
// totalobjectiveC
double totalobjectiveC(arma::vec pars, arma::vec& tt, const arma::vec& ql, const arma::vec& ii, const arma::vec& jj, const arma::vec& varData, const arma::mat& g, arma::mat restr, const arma::mat& mm, const arma::mat& mm2, const bool& DstarM, const bool& oscPdf, const bool& forceRestriction, double precision, const bool& anyFixed, arma::mat fixed);
RcppExport SEXP _DstarM_totalobjectiveC(SEXP parsSEXP, SEXP ttSEXP, SEXP qlSEXP, SEXP iiSEXP, SEXP jjSEXP, SEXP varDataSEXP, SEXP gSEXP, SEXP restrSEXP, SEXP mmSEXP, SEXP mm2SEXP, SEXP DstarMSEXP, SEXP oscPdfSEXP, SEXP forceRestrictionSEXP, SEXP precisionSEXP, SEXP anyFixedSEXP, SEXP fixedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type pars(parsSEXP);
    Rcpp::traits::input_parameter< arma::vec& >::type tt(ttSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type ql(qlSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type ii(iiSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type jj(jjSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type varData(varDataSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type g(gSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type restr(restrSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mm(mmSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mm2(mm2SEXP);
    Rcpp::traits::input_parameter< const bool& >::type DstarM(DstarMSEXP);
    Rcpp::traits::input_parameter< const bool& >::type oscPdf(oscPdfSEXP);
    Rcpp::traits::input_parameter< const bool& >::type forceRestriction(forceRestrictionSEXP);
    Rcpp::traits::input_parameter< double >::type precision(precisionSEXP);
    Rcpp::traits::input_parameter< const bool& >::type anyFixed(anyFixedSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type fixed(fixedSEXP);
    rcpp_result_gen = Rcpp::wrap(totalobjectiveC(pars, tt, ql, ii, jj, varData, g, restr, mm, mm2, DstarM, oscPdf, forceRestriction, precision, anyFixed, fixed));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_DstarM_simpsonC", (DL_FUNC) &_DstarM_simpsonC, 2},
    {"_DstarM_dunifc", (DL_FUNC) &_DstarM_dunifc, 3},
    {"_DstarM_convolveC", (DL_FUNC) &_DstarM_convolveC, 2},
    {"_DstarM_convolveC2", (DL_FUNC) &_DstarM_convolveC2, 2},
    {"_DstarM_chisqC", (DL_FUNC) &_DstarM_chisqC, 3},
    {"_DstarM_rObjC3", (DL_FUNC) &_DstarM_rObjC3, 6},
    {"_DstarM_rObjC2", (DL_FUNC) &_DstarM_rObjC2, 5},
    {"_DstarM_rObjC1", (DL_FUNC) &_DstarM_rObjC1, 5},
    {"_DstarM_rObjC0", (DL_FUNC) &_DstarM_rObjC0, 4},
    {"_DstarM_nthMomentSC", (DL_FUNC) &_DstarM_nthMomentSC, 3},
    {"_DstarM_nthCMomentSC", (DL_FUNC) &_DstarM_nthCMomentSC, 3},
    {"_DstarM_getVarC", (DL_FUNC) &_DstarM_getVarC, 3},
    {"_DstarM_oscCheckC", (DL_FUNC) &_DstarM_oscCheckC, 1},
    {"_DstarM_getVoss", (DL_FUNC) &_DstarM_getVoss, 3},
    {"_DstarM_imposeFixationsC", (DL_FUNC) &_DstarM_imposeFixationsC, 2},
    {"_DstarM_getPdfC", (DL_FUNC) &_DstarM_getPdfC, 6},
    {"_DstarM_totalobjectiveC", (DL_FUNC) &_DstarM_totalobjectiveC, 16},
    {NULL, NULL, 0}
};

RcppExport void R_init_DstarM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
