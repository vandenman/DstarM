#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;

// [[Rcpp::export]]
vec convolveC(vec x, vec y) {

  return conv(x, y);

}

// [[Rcpp::export]]
mat convolveC2(mat x, mat y) {

  const int nr = x.n_rows;
  const int nc = x.n_cols;

  mat out(2*nr - 1, nc);

  for (int i = 0; i < nc; i++) {

    out.col(i) = conv(x.col(i), y.col(i));

  }

  return out;

}

// [[Rcpp::export]]
double chisqC(vec tt, vec a, vec b) {

  vec vals = pow(a - b, 2) / (a + b + 1e-10);
  return as_scalar(trapz(tt, vals));

}

// [[Rcpp::export]]
double rObjC2(vec r, vec tt, vec a, vec bb, vec lenPre, vec lenPost) {

  // both lenPre and lenPost
  vec bb0 = join_cols(lenPre, r);
  vec bb1 = join_cols(bb0, lenPost);
  vec bb2 = conv(bb1, bb);
  vec bb3 = bb2.rows(0, a.size()-1);
  return chisqC(tt, a, bb3);

}

// [[Rcpp::export]]
double rObjC1(vec r, vec tt, vec a, vec bb, vec lenPre) {

  // no lenPost
  vec bb1 = join_cols(lenPre, r);
  vec bb2 = conv(bb1, bb);
  vec bb3 = bb2.rows(0, a.size()-1);
  return chisqC(tt, a, bb3);

}

// [[Rcpp::export]]
double rObjC0(vec r, vec tt, vec a, vec bb, vec lenPost) {

  // no lenPre
  vec bb1 = join_cols(r, lenPost);
  vec bb2 = conv(bb1, bb);
  vec bb3 = bb2.rows(0, a.size()-1);
  return chisqC(tt, a, bb3);

}

// [[Rcpp::export]]
double nthMomentSC(vec x, vec fx, int nth) {

	return as_scalar(trapz(x, pow(x, nth) % fx));
}

// [[Rcpp::export]]
double nthCMomentSC(vec x, vec fx, int nth) {

	double ex = as_scalar(trapz(x, x % fx));
	vec dif = x - ex;
	return as_scalar(trapz(x, pow(dif, nth) % fx));

}

// [[Rcpp::export]]
vec getVarC(mat Pdf, vec tt, mat mm2) {

	// get

	const int nc = Pdf.n_cols;
	//	vec normConsts(nc);
	vec out(nc);
	Pdf = Pdf * mm2;
	Pdf = Pdf * diagmat(1.0 / trapz(tt, Pdf));

	for (int i = 0; i < nc; i++) {

		out[i] = nthCMomentSC(tt, Pdf.col(i), 2);

	}

	return out;

}

// [[Rcpp::export]]
bool oscCheckC(mat x) {

	// x: matrix where every column is a pdf.
	// checks if any pdf is multimodal, stops at first encounter.

	const int nr = x.n_rows;
	const int nc = x.n_cols;
	int i, j;

	for (int c = 0; c < nc; c++) {

		// (re)set i
		i = 1;

		// loop and check if still ascending the density
		while ((i < nr) && (x(i-1, c) <= x(i, c))) i++;

		// first mode found at i
		j = i;

		// loop and check if still descending the density
		while ((j < nr) && (x(j-1, c) >= x(j, c))) j++;

		// if true implies multimodality, therefore exit
		if (j != nr) {
			return false;
		}
	}

	// if execution reaches here then only one mode was found for all the densities.
	return true;

}

// [[Rcpp::export]]
mat getPdfC(mat Pdf, vec tt, mat mm, bool DstarM, bool oscPdf) {

	// Pdfs, time grid, condition matrix, yes/ no DstarM analysis, check for oscillations.
	// note that the 'A' matrix is used to report errors (instead of previous NULL.

	// some reference to voss code for pdfs


	if (oscCheckC(Pdf)) {
		mat A(1, 1, fill::ones);
     	return A;
	}

	vec cor = 1.0 / trapz(tt, Pdf * mm);
	if (cor.has_inf()) {
		mat A(1, 1, fill::ones);
     	return A;
	}

	vec cor2(2*cor.n_elem);
	int j = 0;
	for (int i = 0; i < cor.n_elem; i ++) {
		cor2[j] = cor[i];
		j++;
	}

	Pdf = Pdf * diagmat(cor2);

	return Pdf;

}

// [[Rcpp::export]]
double totalobjectiveC(vec pars, vec tt, vec ql, vec ii, vec jj, vec varData,
                       mat g, mat restr, mat mm, mat mm2,
                       bool DstarM, bool oscPdf, bool forceRestriction) {

	double out = 0.0;

	// convert restr to matrix of parameters
	for (int i = 0; i < restr.n_rows; i++) {
		for (int j = 0; j < restr.n_cols; j++) {
			restr(i, j) = pars[restr(i, j)];
		}
	}

	mat pdf = getPdfC(restr, tt, mm, DstarM, oscPdf);
	if (pdf.n_elem == 1) {
		return 1e9;
	}

	if (!DstarM) {

		for (int i = 0; i < pdf.n_cols; i++) {

			out += chisqC(tt, pdf.col(i), g.col(i)) * 100 * ql[i] / sum(ql);

		}

	} else {

		if (forceRestriction) {

			vec varModel = getVarC(pdf, tt, mm2);
			vec varNonDec = varData - varModel;
			if (any((varNonDec < 0) | (abs(varNonDec) < 1.110223e-16))) {
				return 1e9;
			}

		}

		vec a;
		vec b;

		for (int l = 0; l < ii.n_elem; l++) {

			a = conv(g.col(ii[l]), pdf.col(jj[l]));
			b = conv(g.col(jj[l]), pdf.col(ii[l]));
			out += chisqC(tt, a, b)  * 100 * (ql[ii[l]] + ql[jj[l]]) / sum(ql);

		}

	}

	return out;

}














