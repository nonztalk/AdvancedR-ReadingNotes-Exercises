# ----------------------- Functional ----------------------

# function as input and vector as output
# most common: lapply, apply, tapply and other "apply" series
# Functional is a supplement of closure, which is usually used to replace loops

# a simple example:
randomise <- function(f) f(runif(1e3))
randomise(mean) # mean(runif(1e3))

# simplify lapply: a comprehension on principles
lapply2 <- function(x, f, ...) {
  out <- vector(mode = "list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}
# lapply can make data frame operation easier
mtcars[] <- lapply(mtcars, function(x) x / mean(x))

# loop mode
# There three modes to use for() loop
# 1. for every element: for (x in xs) {x}
# 2. for index: for (i in seq_along(xs)) {xs[i]}
# 3. for element names: for (nm in names(xs)) {xs[nm]}
# The second one is the best as when you use for loop without explicit length,
# you may generate an empty vector before and expand it step by step, which
# leads to memory ineffective. And with the second one, you can generate an
# vector with length before like numeric(length(xs)) and then use indexes in
# for loop.
# This is same for lapply
# lapply(xs, function(x) {}); lapply(seq_along(xs), function(i) {xs[i]}); lapply(names(xs) function(nm) {xs[nm]})

# use lapply to use different lm
formula <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp)
  mpg ~ disp + wt
  mpg ~ I(1 / disp) + wt
)
lapply(formula, function(x) lm(formula = x, data = mtcars))

# functional of for loop
# sapply and vapply
# vapply could be safer than sapply because we can define the output
# an example
df <- data.frame(x = 1:10,y = Sys.time() + 1:10)
sapply(df, class) # second class have two types
vapply(df, class, character(1)) # return an error as the second class isn't single

# differences implementation between sapply and vapply
sapply2 <- function(x, f, ...) {
  res <- lapply2(x, f, ...)
  simplify2array(res)
}

vapply2 <- function(x, f, f.value, ...) {
  out <- matrix(rep(f.value, length(x)), nrow = length(x))
  for (i in seq_along(x)) {
    res <- f(x[i], ...)
    stopifnot(
      length(res) == length(f.value)
      typeof(res) == typeof(f.value)
    )
    out[i, ] <- res
  }
  out
}

# Multiple input, Map and mapply
# when we want to operate upon two or more lists (data frames), using Map()
# would be a great choice
# Map may perform better than mapply
# An example: counting weighted mean
Map(weighted.mean, xs, ws) # xs and ws are two parameters in weighted.mean
# Another equivalent example between lapply and Map
mtcars[] <- lapply(mtcars, function(x) x / mean(x))
# ==>
mtmeans <- lapply(mtcars, mean)
mtcars[] <- Map(`/`, mtcars, mtmeans)
# if some parameters are constant, you can use anonymous function in Map
Map(function(x, w) weighted.mean(x, w, na.rm = T), xs, ws)

# parallel computation
# parallel::mclapply or mcMap
# same as lapply and Map, just add mc.core = ...
parallel::mclapply(df, mean, mc.cores = 2)

# Questions
# use vapply to compute sd for every numeric columns
vapply(iris[vapply(iris, is.numeric, logical(1))], sd, numeric(1))

# if we have following trials, please use sapply to get p-value for every trial
trials <- replicate(100, t.test(rpois(10, 10), rpois(7, 10)), simplify = F)
sapply(trials, function(x) x[["p.value"]])
# and a challenge: do not use anonymous function
sapply(trials, "[[", "p.value")
# replicate() is also an alternation of lapply() and for()

# Implement an version of lapply() that can supply FUN with both the name and the value
# of each component
lapply_nms <- function(x, FUN, ...) {
  Map(FUN, x, names(FUN), ...)
}

# Implement a combination of Map() and vapply() to create an lapply(), which can operate in 
# parallel and store the results in an vector.
# The core algorithm is to treat the objects as several lists
lmapply <- function(X, FUN, FUN.VALUE, simplify = FALSE) {
  out <- Map(function(x) vapply(x, FUN, FUN.VALUE), X)
  if (simplify == TRUE) { return(simplify2array(out)) }
  out
}
testlist <- list(mtcars, iris, cars)
lmapply(testlist, mean, numeric(1))

# Operation on matrix and data frame
# apply(X, MARGIN, FUN, ...(parameters for FUN))
# One should pay attention that apply is not idempotent
a <- matrix(1:20, nrow = 5)
a1 <- apply(a, 1, identity)
identical(a, a1) #FALSE
identical(a, t(a1)) #TRUE
# but you can use aperm() or plyr::aapply() to put high-dimentionl arrays back
# in the right order

# sweep() sweep out the values of a summary statistic
# usually used with apply() for normalization
x <- matrix(rnorm(20, 0, 10), nrow = 4)
x1 <- sweep(x, 1, apply(x, 1, min), `-`)
x2 <- sweep(x1, 1, apply(x1, 1, max), `/`)

# outer()
# accept input of multiple vectors and output a matrix or an array
# the input function will operate on all possible combination
outer(1:3, 1:10, "*")

# group apply
# tapply(): a combination of split() and sapply()
tapply2 <- function(x, group, f, ..., simplify = TRUE) {
  pieces <- split(x, group)
  sapply(pieces, f, simplify = simplify)
}
# tapply seperate the whole data into multiple unequivalent lists accroding to the
# given groups, and sapply each groups with given function

# Questions
# split() + vapply()
v_tappl7 <- function(x, group, FUN, FUN.VALUE, ..., USE.NAMES = TRUE) {
  pieces <- split(x, group)
  vapply(pieces, FUN, FUN.VALUE, ..., USE.NAMES = USE.NAMES)
}

# implement a pure R version of split
split2 <- function(x, f, drop = FALSE, ...) {
  # there are relavent cases of f:
  # f is a character;
  # f is a factor and all levels occur;
  # f is a factor and some levels don't occur;
  
  # if f is a factor
  fact <- is.factor(f)
  
  # if set drop TRUE, drop the non-occuring levels
  if (drop) {
    f <- f[, drop = TRUE]
  }
  
  # give all unique levels of f
  levs <- ifelse(fact, unique(levels(f)), as.character(unique(f)))
  
  setNames(lapply(levs, function(lv) x[f == lv, , drop = FALSE]), levs)
}


# list operation
# every functional programming language has three important functions:
# Map(), Reduce(), Filter()

# Reduce()
# Iteratly call the function f
# Reduce(f, 1:3) ==> f(f(1, 2), 3)
# Reduce(sum, 1:3) ==> sum(sum(1, 2), 3)
# An example: if we want to get the intersection of multiple vector
# (Notice that intersect can only be given two vectors at a time)
l <- replicate(5, sample(1:10, 15, replace = T), simplify = F)
Reduce(intersect, l)

# predicate functionals
# Filter(), Find() and Position()
# Filter() only choose the elements satisfying specific condition
# Find() return the first element that satisfies the condition
# Position() return the position of the first satisfied element 
# Another predicate functional is where()
where <- function(f, x) {
  vapply(x, f, logical(1))
}

# An example:
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
where(is.factor, df)
Filter(is.factor, df)
Find(is.factor, df)
Position(is.factor, df)

# Questions
# use Filter and vapply to construct a function to summarize every numeric column
# in data frame
vapply_num <- function(X, FUN, FUN.VALUE) {
  vapply(Filter(is.numeric, X), FUN, FUN.VALUE)
}

# Construct a function Any(), given parameters as a list and a predicate function
# if predicate function returns TRUE, Any() returns TRUE.
# Construct a similar All()
Any <- function(l, pred) {
  stopifnot(is.list(l))
  for (i in seq_along(l)) {
    if (pred(l[[i]])) return(TRUE)
  }
  return(FALSE)
}

All <- function(l, pred) {
  stopifnot(is.list(l))
  for (i in seq_along(l)) {
    if (! pred(l[[i]])) return(FALSE)
  }
  return(TRUE)
}

# Implement a span() function: 
# given a list x and a predicate function f
# return the location of the longest sequential run of elements where predicate is true
span <- function(l, pred) {
  stopifnot(is.list(l))
  test <- vector("logical", length(l))
  for (i in seq_along(l))
    test[i] <- (pred(l[[i]]))
  
  if (! any(test))
    return(NA_integer_)
  
  # rle() return the length and values of runs of equal values in a vector
  # z <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
  # rle(z)
  # lengths: int [1:5] 2 2 1 1 3
  # values : logi [1:5] TRUE FALSE TRUE FALSE TRUE
  rle_test <- rle(test)
  rle_test <- data.frame(lengths = rle_test[["lengths"]],
                         values = rle_test[["values"]],
                         cumsum = cumsum(rle_test[["lengths"]]))
  # this is a tricky...
  rle_test[["first_index"]] <- rle_test[["cumsum"]] - rle_test[["lengths"]] + 1
  rle_test[["max"]] <- max(rle_test[rle_test[, "value"] == TRUE, ][, "lengths"])
  rle_test[rle_test$lengths == rle_test$max & rle_test$value == TRUE, ]$first_index
}


# Mathematical Functionals
# integrate() calculate integration
# uniroot() calculate solutions of f() = 0 in a given interval
# optimise() calculate min or max of f() in a given interval
integrate(sin, 0, pi)
uniroot(sin, pi * c(1/2, 3/2))
optimise(sin, c(0, pi), maximum = T)

# Maximum Likelihood Estimation
# This problem is suitable for closures
possion_nll <- function(x) {
  n <- length(x)
  sum_x <- sum(x)
  function(lambda) {
    n * lambda - sum_x * log(lambda)
  }
}
x1 <- c(6, 4, 4, 7, 3, 4, 2, 3, 5, 2, 5, 3, 2, 5, 1)
nll1 <- possion_nll(x1)
optimise(nll1, c(0, 100))$minimum

# Question
# Implement arg_max(), given an vector and a function
# Return the input where reaches the maximum
arg_max <- function(v, f) {
  x[f(x) == max(f(x))]
}


# loops should be kept:
# Modifying in place
trans <- list(
  disp = function(x) x * 0.016387
  am = function(x) factor(x, levels = c("auto", "manual"))
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
# Recurrence
# It's hard to convert a loop into a functional when the relationships between
# elements are dependent
exps <- function(x, alpha) {
  s <- numeric(length(x) + 1)
  for (i in seq_along(s)) {
    if (i == 1) {
      s[i] <- x[i]
    } else {
      s[i] <- alpha * x[i - 1] + (1 - alpha) * s[i - 1]
    }
  }
  s
}
# while loop

# generate a new series of function with combination of functionals
# 1. a simple add()
add <- function(x, y, na.rm = FALSE) { # 2. add na.rm parameters
  stopifnot(length(x) == 1, length(y) == 1, is.numeric(x), is.numeric(y))
  # 2. add the NA operation function
  if (na.rm && (is.na(x) || is.na(y)))
    rm_na(x, y, 0)
  else
    x + y
}
# 2. an NA operation function for add()
rm_na <- function(x, y, identity) {
  if (is.na(x) && is.na(y)) {
    identity
  } else if (is.na(y)) {
    y
  } else {
    x
  }
}
# 3. Use Reduce to implement multiple adding
r_add <- function(xs, na.rm = T) {
  Reduce(function(x, y) add(x, y, na.rm = na.rm), xs, init = 0)
}
# 3.1 vectorized add
v_add1 <- function(x, y, na.rm = T) {
  stopifnot(length(x) == length(y), is.numeric(x), is.numeric(y))
  if (length(x) == 0) return(numeric())
  # Implement by Map
  simplify2array(
    Map(function(x, y) add(x, y, na.rm), x, y)
  )
}
v_add2 <- function(x, y, na.rm = F) {
  stopifnot(length(x) == length(y), is.numeric(x), is.numeric(y))
  vapply(seq_along(x), function(i) add(x[i], y[i], na.rm))
}
# 3.2 calculate accumlation
c_add <- function(xs, na.rm = T) {
  Reduce(function(x, y) add(x, y, na.rm = na.rm), xs, init = 0, accumulate = T)
}
# 3.3 calculate in high-dimention data
arr_sum <- function(x, dim, na.rm = F) {
  apply(x, dim, add, na.rm = na.rm)
}