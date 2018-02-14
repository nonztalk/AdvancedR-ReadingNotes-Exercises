#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
int one() {
  return 1;
}

// [[Rcpp::export]]
int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}

// [[Rcpp::export]]
double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}

// [[Rcpp::export]]
NumericVector pdistC(double x, NumericVector ys) {
  // get the length of ys
  int n = ys.size(); 
  // generate a new vector
  // Another way to generate a new vector is using clone()
  // NumericVector zs = clone(ys)
  NumericVector out(n);
  for (int i = 0; i < n; ++i) {
    out[i] = sqrt(pow(ys[i] - x, 2.0));
  }
  return out;
}


// [[Rcpp::export]]
NumericVector rowSumsC(NumericMatrix x) {
  // use .nrow, .ncol method to get number of row and column
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(nrow);
  
  // for each row
  for (int i = 0; i < nrow; i++) {
    // set initial total value
    double total = 0;
    // for each column in one row
    for (int j = 0; j < ncol; j++) {
      // add the element to total
      // Note that using () to index in C++ 
      total += x(i, j);
    }
    out[i] = total;
  }
  return out;
}

// Some other functions rewritten in Rcpp
// all()
// [[Rcpp::export]]
bool allC(LogicalVector x) {
  int n = x.size();
  for (int i = 0; i < n; i++) {
    if (!x[i]) return false;
  }
  return true;
}

// cumprod(), cummin()
// [[Rcpp::export]]
NumericVector cumprodC(NumericVector x) {
  int n = x.size();
  NumericVector out(n);
  
  out[0] = x[0];
  for (int i = 0; i < n; i++) {
    out[i] = out[i - 1] + x[i];
  }
  return out;
}
// [[Rcpp::export]]
NumericVector cumminC(NumericVector x) {
  int n = x.size();
  NumericVector out(n);
  
  out[0] = x[0];
  for (int i = 1; i < n; i++) {
    out[i] = std::min(out[i - 1], x[i]);
  }
  return out;
}

// diff() with lag n
// [[Rcpp::export]]
NumericVector difflagC(NumericVector x, int lag) {
  int n = x.size();
  NumericVector out(n - lag);
  for (int i = lag; i < n; i++) {
    out[i - lag] = x[i] - x[i - lag];
  }
  return out;
}

// range()
// [[Rcpp::export]]
NumericVector rangeC(NumericVector x) {
  double omin = x[0], omax = x[0];
  int n = x.size();
  NumericVector out(2);
  
  for (int i = 0; i < n; i++) {
    omin = std::min(x[i], omin);
    omax = std::max(x[i], omax);
  }
  out[0] = omin;
  out[1] = omax;
  return out;
}

// other objects and attribution
// List and DataFrame
// Here is a example to calculate mean percentage error mpe
double mpe(List mod) {
  // use .inherit to test if mod is a linear model
  if (!mod.inherits("lm")) stop("Input must be a linear model");
  
  // use as<> to convert the target data form
  NumericVector resid = as<NumericVector>(mod["residuals"]);
  NumericVector fitted = as<NumericVector>(mod["fitted.values"]);
  
  int n = resid.size();
  double err = 0;
  for (int i = 0; i < n; ++i) {
    err += resid[i] / (fitted[i] + resid[i]);
  }
  return err / n;
  // Actually, this is not a good C++ example
}

// Function
// [[Rcpp::export]]
// call R function in C++ code
RObject callWithOne(Function f) { // We don't know R function return exactly,
                                  // so just use RObject
  return f(1);
}
// Then we can call it with R function: callWithOne(function(x) x + 1)

// C++ version of lapply
// [[Rcpp::export]]
List lapply1(List input, Function f) {
  int n = input.size();
  List out(n);
  
  for (int i = 0; i < n; i++) {
    out[i] = f(input[i]);
  }
  
  return out;
}
// Call in R using position: lapply1(x, f)
// or using name: lapply1(_["input"] = x, _["f"] = f)

// operation on missing value in R

