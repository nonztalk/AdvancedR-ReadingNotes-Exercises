# -------------- Functional Operator (FO) --------------------
# Multiple functions as input and one function as output
# Remeber: 
# Functional: function as input, vector as output
# Functional Operator: functions as input, function as output

# Behavioural FOs
# BFOs wno't change inputs and outputs of functions but attach some behaviour
# For example: three implementation
# 1. add a delay to avoid swamping on server with requests
# 2. print to console every n invocations to check a long process
# 3. cache previous computations to improve performance

# The first example:
# Suppose we would like to download from a lot of urls
download_file <- function(url, ...) {
  download.file(url, basename(url), ...)
}
lapply(urls, download_file)
# try to add something new: print "." every 10 files and insert a delay between
# every two requests.
delay_by <- function(delay, f) { # this is a model
  function(...) {
    Sys.sleep(delay)
    f(...)
  }
}
dot_every <- function(n, f) {
  i <- 1 # here is a counting, shown in chapter about environment
  function(...) {
    if (i %% 10 == 0) cat(".")
    i <<- i + 1
    f(...)
  }
}
lapply(urls, dot_every(10, delay(1, download_file)))

# The third example: cache
# A method to check if the download has been repeated is using memoisation
# It can store the results of functions previously called so the next running will
# be much master
# A real example is fibonacci:
fib <- memoise(function(n) {
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
})
# In problem of downloading files, we can use memoisation as this:
download <- dot_every(10, memoise(delay_by(1, download_file)))
lapply(urls, download)

# The second example:
# We want to capture the inner invocation when functionals are running.
# We can use FOs and tee() to implement this
# It seems as the decorator in Python
tee <- function(f, on_input = function(...) NULL, on_output = function(...) NULL) { 
  # f: function to be modified
  # on_input: function invocated with input of f
  # on_output: function invocated with output of f
  function(...) {
    on_input(...) 
    output <- f(...)
    on_output(output)
    output
  }
}
# An example: uniroot
g <- function(x) cos(x) - x
show_x <- function(x, ...) cat(sprintf("%+.08f", x), "\n")
# location where the function is evaluated
zero <- uniroot(tee(g, on_input = show_x), c(-5, 5))
# The value of the function
zero <- uniroot(tee(g, on_output = show_x), c(-5, 5))

# try to store the results during the process of uniroot
# implement a function remember()
remember <- function() {
  memory <- list()
  f <- function(...) {
    memory <<- append(memory, list(...))
    invisible()
  }
  structure(f, class = "remember")
}
as.list.remember <- function(x, ...) {
  environment(x)$memory
}
print.remeber <- function(x, ...) {
  cat("Remebering...\n")
  str(as.list(x))
}
# now we can draw to find how uniroot give the solution
locs <- remember()
vals <- remember()
zero <- uniroot(tee(g, locs, vals), c(-5, 5))
x <- unlist(as.list(locs))
error <- unlist(as.list(vals))
plot(x, type = "b"); abline(h = 0.739, col = "grey50")
plot(error, type = "b"); abline(h = 0, col = "grey50")

# A common module of FO:
funop <- function(f, otherargs) {
  force(f) # because of the side-effect of laziness
  function(...) {
    # do sth.
    res <- f(...)
    # do sth. else
    res
  }
}

# Question:
# Implement a FO which can log time stamp and message to a file every time a 
# function is running
logger <- function(f, filename) {
  force(f)
  filename_tmp <- paste(filename, basename(tempfile()), sep = "_")
  write(paste("created at: ", Sys.time(), filename_tmp, append = T))
  function(..., message = "you can add a message at each call") {
    write(paste0("used at: ", Sys.time(), ", ", message), filename_tmp, append = TRUE)
    f(...)
  }
}
mean2 <- logger(mean, "mean_log")
mean2(1:10, message = "first call of mean")
mean2(1:10, message = "second call of mean")

# Modify delay to ensure a certain amount of time has elapsed since the function
# was last called
delay_by2 <- function(delay, f) {
  force(f)
  last_runtime <- Sys.time() - (delay + 42)
  function(...) {
    while (Sys.time() < last_runtime + delay) {}
    on.exit(last_runtime <<- Sys.time())
    return(f(...))
  }
}

# Implement wail_until() which delays execution until a specific time
wait_until <- function(time, f) {
  force(f)
  function(...) {
    while (Sys.time() < time) {}
    return(f(...))
  }
}

# This function works unexpectedly in previous R version
# It will work well in this version
f <- function(a, b) function(x) a * x + b
fs <- Map(f, a = c(0, 1), b = c(0, 1))
fs[[1]](3)
# This is because of laziness, as a and b are looped to the final value before
# used in the equation
f <- function(a, b) {force(a); force(b); function(x) a * x + b}


# Output FOs
# Modification on function output. It can be simple or changing function entirely

# Simple modification Negate() and failwith()
# Negate() receive a predication function and return the inverse of its result
# a simple implementation
Negate <- function(f) {
  force(f)
  function(...) !f(...)
}
# One can use this to implement compact(), a function to elimiate null in list
# Filter can't receive !is.null
compact <- function(x) Filter(Negate(is.null), x) 

# plyr::failwith() is a function that converts the error function into anthor 
# function returning default value. It is an encapsulation of try()
failwith <- function(default = NULL, f, quiet = F) {
  force(f)
  function(...) {
    out <- default
    try(out <- f(...), silent = quite)
    out
  }
}
# failwith() is usually used with functionals: error won't expand and the outside 
# loop won't stop; we can review the errors.
# We can implement this on glm where we want to fit a data frame on a GLM:
models <- lapply(datasets, plyr::failwith(NULL, glm), 
                 formula = y ~ x1 + x2 * x3) # Error will be returned as null
ok_models <- compact(models) # return the non-null value
fail_data <- datasets[vapply(models, is.null, logical(1))] # index for the failed data columns

# change function output
# capture_it() returns print()'s output text
capture_it <- function(f) {
  force(f)
  function(...) {
    capture.output(f(...))
  }
}
(capture_it(str))(1:10) # a vector of text
# time_it() returns the run time of a function
time_it <- function(f) {
  force(f)
  function(...) {
    system.time(f(...))
  }
}
# using time_it() now we can replace system.time()
compute_mean <- list(
  base = function(x) mean(x)
  sum = function(x) sum(x) / length(x)
)
x <- runif(1e6)
call_fun <- function(f, ...) f(...)
lapply(compute_mean, time_it(call_fun), x)

# Question
# Implement a FO named negative() which can inverse the signal of functional output
negative <- function(f) {
  force(f)
  function(...) {
    -f(...)
  }
}

# Implement a similar function like capture_it() so it can capture the warnings
# and errors of functions
capture_message <- function(f) {
  force(f)
  function(...) {
    capture.output(
      tryCatch(f(...), error = function(e) e, warning = function(w) w)
    )
  }
}

# Implement a function tracking the file construction and deletion in working 
# direction
track_dir <- function(f) {
  force(f)
  function(...) {
    dir_old <- dir() # return all files and folders in current direction
    
    # at the end of invocating f, these codes will scan the direction and find the 
    # file differences
    on.exit(
      if (! setequal(dir(), dir_old)) {
        message("Files in your working directory were deleted or added by this function.")
      },
      add = T
    )
    on.exit(
      if (length(setdiff(dir(), dir_old)) != 0) {
        message(paste0("The following files were added: ",
                       paste(setdiff(dir(), dir_old), collapse = ", ")))
      },
      add = T
    )
    on.exit(
      if (length(setdiff(dir_old, dir())) != 0) {
        message(paste0("The following files were deleted: ",
                       paste(setdiff(dir_old, dir()), collapse = ", ")))
      },
      add = T
    )
    
    f(...)
  }
}


# Input FOs
# Modify inputs of functions. 
# prefilling function arguments: partial function application
# partial can be used to replace anonymous function, like:
library(pryr)
function_list <- list(
  # replace function(...) f(..., na.rm = T)
  mean = partial(mean, na.rm = T) 
  sum = partial(sum, na.rm = T)
  median = partial(median, na.rm = T)
)

# Change inputs of function to enable functions operate on different types of data
# base::Vectorize() can convert scalar function to a vector function
# for example: sample()
sample2 <- Vectorize(FUN = sample, vectorize.args = "size", 
                     SIMPLIFY = FALSE) # ensure it will return a list
sample2(1:5, 3:5) # run sample() three times in which the size equals to 3, 4, 5

# splat() can convert the function receive multiple parameters into one receive
# a list of parameters
splat <- function(f) {
  force(f)
  function(args) {
    do.call(f, args)
  }
}
# It should be useful when we want to call a function with diversity parameters
x <- c(NA, runif(100), 1000)
args <- list(
  list(x),
  list(x, na.rm = T),
  list(x, na.rm = T, trim = 0.1)
)
lapply(args, splat(mean))

# plyr::colwise() convert vector functions into these operating on data frames
plyr::colwise(median)(mtcars)

# Combining FOs
# FOs can also receive multiple functions as input
# For example: plyr::each(), it can combine a list of function as a function
summaries <- plyr::each(mean, sd, median, max, min)
summaries(runif(1e5))
# Function composition
# A simple example is: (plyr provides a more complicate one)
compose <- function(f, g) {
  function(...) f(g(...))
}
sapply(mtcars, plyr::compose(length, unique)) # replace for function(x) length(unique(x))
# Mathematically, composition of function is usually represented by infix operator:
"%o%" <- plyr::compose
sapply(mtcars, length %o% unique) # same as compose(length, unique)
# And composition provides a simple way to implement Negate()
Negate <- partial(compose, `!`)
# We can also use composition to calculate standard error
# (But it could be very hard for reading, not recommend)
square <- function(x) x^2
deviation <-  function(x) x - mean(x)
sd2 <- sqrt %o% mean %o% square %o% deviation
# This type of programming is called point-free programming where you need not to
# care the parameters but focus the combination of functions
# It can be seen in Haskell, but not very elegant in R (neither for me...)

# Logical predicates and boolean algebra
# When using Filter and other functions that work with logical predicates, you can
# define some FOs combining logical predicates to replace anonymous functions
# An example:
and <- function(f1, f2) {
  force(f1); force(f2)
  function(...) {
    f1(...) && f2(...)
  }
}
# Now you can use it in Filter:
Filter(and(is.character, is.factor), iris)

# Question
# use Reduce() and %o% implement a compose() function without using function()
compose2 <- function(fs) {
  Reduce(function(f, g) function(...) f(g(...)), fs)
}
compose2_without_function <- function(fs) {
  Reduce(partial(partial("%o%")), fs)
}
# Expand and() to receive multiple parameters
and_expand <- function(fs) {
  Reduce(function(f, g) and(f, g), fs)
}
