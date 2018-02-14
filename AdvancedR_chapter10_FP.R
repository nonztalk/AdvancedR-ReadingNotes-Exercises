# ------------------------ Functional Progamming -------------------------
# Anonymous function
lapply(mtcars, function(x) sd(x) / mean(x))
integrate(function(x) sin(x) + cos(x), -pi, pi)
integrate(function(x) exp(x)/x, 10, 20)
# match.fun() can help you find a function with its name
match.fun("mean")(x) # even you can use it directly

# Closures
power <- function(exponent) { # parent layer
# parent layer play as a environment for the functions in children layer where they find
# the related variables, like exponent.
  function(x) { # children layer
    x ^ exponent
  }
# When tranform closure to a variable, this variable become a function pointing to the children
# functions, but the variables in children functions have been replaced by those defined in the
# parent layer.
}

square <- power(2) # similar to square <- function(x) x ^ 2
cube <- power(3) # similar to cube <- function(x) x ^ 3
# Actually the body of function has not changed but the closure environment, where exponent = 2
cube
# function(x) { # children layer
#   x ^ exponent
# }
# <bytecode: 0x18df1cb88>
# <environment: 0x18de48470>
pryr::unenclose(cube)
# function (x) 
# {
#   x^3
# }

# Mutable state
new_count <- function() {
  i <- 0
  function() {
    i << i + 1
    i
  }
}
counter <- new_count() # generate a enclosing environment, which has i = 0, and function() {i << i + 1; i}
counter() # 1 
counter() # 2 In this enclosing environemnt, i will not reset, even though the execution has changed

# This is also viable
i <- 0
new_count2 <- function() {i <<- i + 1; i}
new_count2()
# But ... 
new_count3 <- function() {
  i <- 0
  function() {i <- i + 1; i}
}
count <- new_count3()
count()
# This generate the environment of i = 0, and function() {i < i + 1; i}
# Every execution function() will find i on this environment, and every time i = 0

# An example of closure: box_cox_transformation
# This will return different function depending on lambda
box_cox_trans <- function(lambda) {
  if (lambda == 0) function(x) log(x)
  else function(x) (x ^ lambda - 1) / lambda
}

# A costum function to calculate ith central moment
centralMoment <- function(i) {
  function(x) sum((x - mean(x)) ^ i) / length(x)
}

# Another closure: operate subset index
pick <- function(i) {
  function(x) x[[i]]
}
stopifnot(identical(lapply(mtcars, pick(5)), lapply(mtcars, function(x) x[[5]])))



# Lists of functions
# An example: comparison among calculation of average
x <- runif(1e6)
compute_mean <- list(
  base = function(x) mean(x),
  sum = function(x) sum(x) / length(x),
  manual = function(x) {
    total <- 0
    n <- length(x)
    for (i in seq_along(x)) {
      tatal <- total + x[i] / n
    }
    total
  }
)
# call function as list indexing
system.time(compute_mean$base(x))
system.time(compute_mean[[2]](x))
system.time(compute_mean[['manual']](x))
# use lapply
lapply(compute_mean, function(f) system.time(f(x)))
# or define a new function
call_fun <- function(f, ...) system.time(f(...))
lapply(compute_mean, call_fun, x)

# list of functions can also combine multiple functions to operate the same 
# objects
funs <- list(
  sum = sum,
  mean = mean,
  median = median
)
lapply(funs, function(f) f(x))
# ignore missing value
lapply(funs, function(f) f(x, na.rm = TRUE))

# another example (more complex) of list of functions
simple_tag <- function(tag) {
  force(tag)
  function(...) {
    paste0("<", tag, ">", paste0(...), "</", tag, ">")
  }
}
tags <- c("p", "b", "i")
html <- lapply(setNames(tags, tags), simple_tag)
lapply(html, function(x) pryr::unenclose(x))
html$p("This is ", html$b("bold"), " text.")
# if we want use the function in html without using $ every time
# we have three mehods to reach this
# 1. attach and detach
attach(html); p("...", b("...")); detach(html)
# 2. with()
with(html, p("This is ", b("bold"), " text."))
# 3. list2env() and rm()
list2env(html, environment()); ...; rm(list = names(html), envir = environment())

# A custom summary()
summary2 <- function(character_functions = NULL, integer_functions = NULL,
                     double_functions = NULL, logical_functions = NULL, 
                     factor_functions = NULL, date_functions = NULL) {
  
  default_1 <- list(Table = table)
  default_2 <- list(Min = min, `1st Qu.` = function(x) quantile(x)[[2]],
                   Median = median, Mean = mean, 
                   `3rd Qu.` = function(x) quantile(x)[[4]], Max = max)
  
  if(is.null(character_functions)) {character_functions = default_1}
  if(is.null(integer_functions))   {integer_functions   = default_2}
  if(is.null(double_functions))    {double_functions    = default_2}
  if(is.null(logical_functions))   {logical_functions   = default_1}
  if(is.null(factor_functions))    {factor_functions    = default_1}
  if(is.null(date_functions))      {date_functions      = default_2}
  
  apply_typefunction <- function(df, pred, functions) {
    lapply(df[vapply(df, pred, logical(1))],
           function(x) unlist(lapply(functions, function(f) f(x))))
  }
  
  # Main closure and list of functions
  function(df) {
    characters <- apply_typefunction(df, is.character, character_functions)
    integers   <- apply_typefunction(df, is.integer  , integer_functions  )
    doubles    <- apply_typefunction(df, is.double   , double_functions   )
    logicals   <- apply_typefunction(df, is.logical  , logical_functions  )
    
    factors    <- apply_typefunction(df, is.factor   , factor_functions   )
    dates      <- apply_typefunction(df, function(x) inherits(x, 'Date'), 
                                     date_functions)
    
    out <- list(characters, integers, doubles, logicals, 
                factors, dates)
    out[lengths(out) != 0]
  }
  
}

# Case study: numeric integration
# 1. A simple try, use both midpoint and trapezoid
midpoint <- function(f, a, b) {
  (b - a) * f((a + b) / 2)
}
trapezoid <- function(f, a, b) {
  (b - a) / 2 * (f(a) + f(b))
}
# This abusolutly give wrong answer. So we go to next step
# 2. composite integration
midpoint_comp <- function(f, a, b, n = 10) {
  points <- seq(a, b, length = n + 1)
  h <- (b - a) / n
  area <- 0
  for (i in seq_len(n)) {
    area <- area + h * f((points[i] + points[i + 1]) / 2)
  }
  area
}
# same structure as trapezoid, so we can notice they may have same structure,
# so we can combine them together
# 3. combine midpoint and trapezoid
composite <- function(f, a, b, n = 10, rule) {
  points <- seq(a, b, length = n + 1)
  
  area <- 0
  for (i in seq_len(n)) {
    area <- area + rule(f, points[i], points[i + 1])
  }
  area
}
# 4. better rules
# Newton-Cotes rule
newton_cotes <- function(coef, open = FALSE) {
  n <- length(coef) + open
  function(f, a, b) {
    pos <- function(i) a + i * (b - a) / n
    points <- pos(seq.int(0, length(coef) - 1))
    (b - a) / sum(coef) * sum(f(points) * coef)
  }
}
# all other rules can generate from it, like
miline <- newton_cotes(c(2, -1, 2), open = T)
composite(sin, 0, pi, n = 10, rule = miline)

