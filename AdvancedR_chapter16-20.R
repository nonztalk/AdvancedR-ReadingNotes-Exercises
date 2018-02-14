# ------------------ Performance ------------------- 
# R is slow...
# We can use microbenchmark to measure time consumption at a very accurate level
library(microbenchmark)
# test different ways of calculating square root
x <- runif(100)
microbenchmark(
  sqrt(x),
  x ^ 0.5,
  x ^ (1 / 2),
  exp(log(x) / 2)
)
# test basic calculator signal speed, each for integer and double
microbenchmark(
  1 + 1,
  1 * 1,
  1 - 1,
  1 / 1,
  1L + 1L,
  1L * 1L,
  1L - 1L,
  1L / 1L, 
  times = 1000000
)

# ----------------- code modification ---------------
# Performance test (use lineprof)
devtools::install_github("hadley/lineprof")
# Example code from homework
source("~/Desktop/R/ExampleCode.R")
# Emmmmm... It seems useless...
# Actually it will display the time consumption for each step
# but ... hard to understand ... 
l <- lineprof(simulation())

# A good way to accelarate your code is let the function do as less as
# possible

# For example:
# rowSums(); colSums(); rowMeans(); colMeans() is faster than 
# apply(df, 0/1, sum/mean) because they are vectorized
# vapply() is faster than sapply() because it define the output type
# any(x == 10) is faster than 10 %in% x because exam on equation is 
# easier than on inclusion

# Avoid method despatch can also speed up our functions
# For example, when operating on small vector, we can use mean.default()
# to replace mean
# This could be dangerous if you are not sure the input type
x <- runif(1e5)
microbenchmark::microbenchmark(
  mean(x),
  mean.default(x)
)
# Unit: microseconds
# expr   min    lq    mean median     uq    max neval
# mean(x) 3.264 3.390 3.86967  3.449 3.5400 39.639   100
# mean.default(x) 1.187 1.279 1.36577  1.337 1.4125  2.752   100

# When you make sure what you are inputting in, you can simplify some
# source code even faster, like as.data.frame()
# Suppose we have a named list, and each vector in it has same length.
# Then we can simplify as.data.frame()
# But you should make sure the input type!!!
quickdf <- function(l) {
  class(l) <- "data.frame"
  attr(l, "row.names") <- .set_row_names(length(l[[1]]))
  l
}
l <- lapply(1:26, function(x) runif(1e3))
names(l) <- letters
microbenchmark::microbenchmark(
  quickdf(l),
  as.data.frame(l)
)
# Unit: microseconds
# expr      min        lq       mean    median        uq      max neval
# quickdf(l)    7.376   11.3555   16.75137   15.2875   19.6055   55.968   100
# as.data.frame(l) 1187.249 1337.0015 1633.91378 1503.1430 1807.7445 3032.928   100



# A case study: t.test() [We just need it to calculate the t value]
# Suppose we have 1000 experiments(row), and 50 data in each experiment(column)
# Previous 25 data are grouped as one and the other two
m <- 1000
n <- 50
X <- matrix(rnorm(m*n, mean = 10, sd = 3), nrow = m)
grp <- rep(1:2, each = n / 2)

# We can use formula or two vectors to compute t.test()
# Vector is much faster than formula
system.time(
  for (i in 1:m)
    t.test(X[i, ] ~ grp)$stat
)
# user  system elapsed 
# 0.914   0.019   0.953
system.time(
  for (i in 1:m)
    t.test(X[i, grp == 1], X[i, grp == 2])$stat
)
# user  system elapsed 
# 0.155   0.001   0.156 

# Since for loop will not save the result for every step, so we may replace it
# with apply()
compT <- function(x, grp) {
  t.test(x[grp == 1], x[grp == 2])$stat
}
system.time(t1 <- apply(X, 1, compT, grp = grp))

# To make the function faster, we may modify the source code of t.test()
# stats:::t.test.default() shows this function also calculates p value and 
# generate output form. So we can eliminate these functions
my_t <- function(x, grp) {
  t_stat <- function(x) {
    m <- mean(x)
    n <- length(x)
    var <- sum((x - m) ^ 2) / (n - 1)
    list(m = m, n = n, var = var)
  }
  g1 <- t_stat(x[grp == 1])
  g2 <- t_stat(x[grp == 2])
  se_total <- sqrt(g1$var / g1$n + g2$var / g2$n)
  (g1$m - g2$m) / se_total
}
system.time(t2 <- apply(X, 1, my_t, grp = grp))
# all.equal(t1, t2) == TRUE
#  user  system elapsed 
# 0.037   0.001   0.038 

# More faster! Vectorize!
# This operation enable our function to work on matrix
rowstat <- function(X, grp) {
  t_stat <- function(X) {
    m <- rowMeans(X)
    n <- ncol(X)
    var <- rowSums((X - m) ^ 2) / (n - 1)
    list(m = m, n = n, var = var)
  }
  g1 <- t_stat(X[, grp == 1])
  g2 <- t_stat(X[, grp == 2])
  se_total <- sqrt(g1$var / g1$n + g2$var / g2$n)
  (g1$m - g2$m) / se_total
}
system.time(t3 <- rowstat(X = X, grp = grp))
#  user  system elapsed 
# 0.018   0.001   0.018 

# Final modification, bytecode intepret
rowstat_bc <- compiler::cmpfun(rowstat)
system.time(t4 <- rowstat_bc(X = X, grp = grp))
#  user  system elapsed 
# 0.001   0.000   0.001

# ------------------- RAM ----------------------
# object_size() in pryr can tell us how much RAM the object has occupied
pryr::object_size(data) #200MB...
# let's see how can a vector occupy the RAM
sizes <- sapply(0:50, function(n) pryr::object_size(seq_len(n)))
plot(0:50, sizes, xlab = "Length", ylab = "Size (bytes)", type = "s")
# We can notice that the empty integer vector occupies 40B RAM
# Actually, every type of empty vector will occupy 40B RAM

# mem_used() can tell us all objects in memory
pryr::mem_used() # 153MB, when global is empty but have attached many packages

# mem_change() can tell us how memory change with code
pryr::mem_change(x <- 1:1e6) # 3.98 MB
pryr::mem_change(rm(x)) # -4.01 MB
pryr::mem_change(NULL) # 864 B, memory will change even you do nothing

# ------------------ Rcpp!!! ----------------------
# 1. No input, Scalar output
cppFunction(
  'int one()
  {
    return 1;
  }'
)

# 2. Scalar input, Scalar output
cppFunction('int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}'
)

# 3. Vector input, Scalar output. For example: loop sum
cppFunction('double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')

# 4. Vector input, Vector output
cppFunction('NumericVector pdistC(double x, NumericVector ys) {
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
}')

# 5. Matrix input, Vector output. For example, rowSums()
sourceCpp(file = "~/Desktop/R/AdvancedR_Rcpp.cpp")
rowSumsC()





