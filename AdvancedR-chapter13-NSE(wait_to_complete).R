# ---------------------------- Non-standard Evaluation (NSE) ------------------------
# Capture expressions
# substitute() can return the expressions used in functions that have not been 
# calculated, such as substitute(x + y ^ 2) will return x + y^2 even x and y have
# been defined
# substitute() is usually used with deparse(), which can convert the expressions from
# substitute() into a character vector. Many functions in base use them to avoid
# qutation marks

# Question
# If the input is too long, deparse will return multiple strings:
# Because deparse() has a width.cutoff arguments where the threshold is 60 bytes
# You can use paste0 to encapsulate deparse() to generate only one string

# NSE in subset
# We can also operate more on un-calculated code, like subset()
# It does not need to input names of data frame many times
# It is special because the expression of indexing subsets run on the given data frame
# rather than the global environment
# To illustrate how subset() work, we have already know how to capture the expression
# but not their results. So we just need to define correct context for computation

# This is eval(). It can receive expression and calculate in specific environment
# Another function is quote(). Like substitute(), it also can capture expressions but
# without further conversion

# The first parameter in eval() receive an expression and the second receive the 
# defined environment. It can be list or data frame
subset2 <- function(x, condition, drop = FALSE) {
  condition_call <- substitute(condition)
  # tell eval() if it cannot find objects in x, where else it can search
  # We want to search x in the environment where subset2 is invocated
  # enclose = parent.frame()
  r <- eval(condition_call, x, enclos = parent.frame())
  x[!is.na(r) & r, , drop = drop]
}

# NSE could be troublesome when called by other functions. subset() can be very
# hard to use in non-interactive programing, such as:
# We want to implement a function which can randomly sort the data frame at specific
# rows:
scramble <- function(x) x[sample(nrow(x)), ]
subscramble <- function(x, condition) {
  scramble(subset2(x, condition))
}
# But it cannot work when using as subscramble(sample_df, a >= 4)
# So the system provide a alternative standard evaluation. As for subset2(), we can
# 

