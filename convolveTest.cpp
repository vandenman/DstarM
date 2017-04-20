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
  mat out = trapz(tt, vals);
  return out(0, 0);

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


/*** R
library(microbenchmark)
x <- seq(-3, 2, .01)
f <- dunif(x, -.5, .5)
g <- f

f2 <- matrix(f, length(x), 16)
g2 <- f2

# 1D ----
r0 <- DstarM:::customConvolveO(f, rev(g))
c0 <- convolveC(f, g)
length(r0)
length(c0)
matplot(seq_along(r0), cbind(r0, c0), type = "l")
all.equal(r0, c(c0))

timing <- microbenchmark(
  r0 <- DstarM:::customConvolveO(f, rev(g)),
  c0 <- convolveC(f, g)
)
print(timing, unit = "relative")

# 2D ----
foo <- function(x, y) {

  out <- matrix(nrow = 2*nrow(x) - 1, ncol = ncol(x))
  y <- y[nrow(y):1, ]
  for (i in 1:ncol(x)) {

    out[, i] <- DstarM:::customConvolveO(x[, i], y[, i])

  }

  return(out)

}
r2 <- foo(f2, g2)
c2 <- convolveC2(f2, g2)
all.equal(r2, c2)

timing <- microbenchmark(
  r2 <- foo(f2, g2),
  c2 <- convolveC2(f2, g2),
  times = 50
)
print(timing, unit = "relative")

# chisq ----
x <- seq(-5, 5, .01)
f <- dnorm(x)
g <- dnorm(x, 1, 1.5)
r3 <- chisq(x, f, g)
c3 <- chisqC(x, f, g)
all.equal(r3, c3)
timing <- microbenchmark(
  r3 <- chisq(x, f, g),
  c3 <- chisqC(x, f, g)
)
print(timing, unit = "relative")

# r.obj ----
tt <- seq(0, 1, .01)
c <- dunif(tt, .4, .6)
b <- dunif(tt, .4, .6)
a <- convolveC(c, b)[seq_along(tt)]

r4 <- DstarM:::r.obj(tt = tt, a = a, bb = b, lenPre = 0, lenPost = 0, r = c[-c(1, length(c))])
c4 <- rObjC2(tt = tt, a = a, bb = b, lenPre = 0, lenPost = 0, r = c[-c(1, length(c))])
all.equal(r4, c4)

timing <- microbenchmark(
  r4 <- DstarM:::r.obj(tt = tt, a = a, bb = b, lenPre = 0, lenPost = 0, r = c[-c(1, length(c))]),
  c4 <- rObjC2(tt = tt, a = a, bb = b, lenPre = 0, lenPost = 0, r = c[-c(1, length(c))])
)
print(timing, unit = "relative")
*/
