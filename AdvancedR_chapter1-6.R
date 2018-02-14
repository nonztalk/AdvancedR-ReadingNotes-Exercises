# Atomic Vector
dbl_var <- c(1, 2.6, 3.5)
int_var <- c(1L, 2L, 10L)
log_var <- c(T, F, TRUE, FALSE)
chr_var <- c("Pass")
NA_var <- c(NA, NA_real_, NA_integer_, NA_character_)
# type and examination typeof()

# list is recursive
x <- list(list(list(list())))

# c() will convert atomic vectors to lists
x <- c(list(1, 2), c(3, 4)) # list of 4

# Attribution of objects
# attr() find every attribution of a object, or even add one
y <- 1:10
attr(y, "My attribute") <- "This is a vector"
attr(y, "My attribute")
# attributes() find all attributions of an object
attributes(ArchiveNewest)
# structure() can return a new object with modification attributions
structure(1:10, "My attribute" = "This is a vector")

# Three most important attributes
# name: names(); dimention: dim(); class: class()
x <- setNames(1:3, c("a", "b", "c"))
x <- unname(x)
names(x) <- NULL

# construct list array or matirx
l <- list(1:3, "a", TRUE, 1.0)
dim(l) <- c(2, 2)

# Data Frame
typeof(df) == "list"
# combine data frame
plyr::rbind.fill() # two data frame have different columns
# one column of a data frame can be consist of list
df <- data.frame(x = 1:3)
df$y <- list(1:2, 1:3, 1:4)
# and you can add I() to regard a list as an element
data.frame(x = 1:3, y = I(list(1:2, 1:3, 1:4))) # same as df$y's result
# with I(), you even can add a matrix or array in a column with same row length
data.frame(x = 1:3, y = I(matrix(1:9, nrow = 3)))
# x y.1 y.2 y.3
# 1 1   1   4   7
# 2 2   2   5   8
# 3 3   3   6   9
# It doesn't mean it have three columns named y.1, y.2 and y.3
# You should also use "y" to index
data.frame(x = 1:3, y = I(matrix(1:9, nrow = 3)))[, "y"]

# index operation
# for atomic vector
x[c(2.1, 2.9)] == x[c(2, 2)]
x[c(-1, 2)] # Error
x[c(1, 2, NA)] == c(x[1], x[2], NA) # will generate NA at the corresponding position where given NA in index
x[] == x
x[0] == numeric(0)

# construct an array with outer or %o%
vals <- outer(1:5, 1:5, FUN = "^")
# also you can select by using matrix
select <- matrix(ncol = 2, byrow = T, c(1, 1, 3, 1, 2, 4))
vals[select] == c(vals[1, 1], vals[3, 1], vals[2, 4])

# select columns from data frame
# like a list vs. like a matrix
df[c("X", "Z")] == df[, c("X", "Z")] 
# Custom diag
diagCustom <- function(x) {
  if (nrow(x) != ncol(x)) 
    stop("row number not equal to column number")
  select <- matrix(ncol = 2, byrow = T, rep(1:nrow(x), each = 2))
  return(x[select])
}

# [[ and $
a <- list(a=1, b=2, c=3)
b <- list(a = list(b = list(c = list(d = 1))))
a[1] # will give $a [1] 1
a[[1]] == a[["a"]] == a$a# will give 1
b[[c("a", "b", "c", "d")]] == b[["a"]][["b"]][["c"]][["d"]]
# in data frame, it is equal:
df[[1]] == df[[colnames(df[1])]] == df[colnames(df[1])] == df[, colnames(df)[1]]

# when using $, you should pay attention:
df$cyl != df$var # var <- "cyl"
df$cyl == df[[var]] # var <- "cyl"
df$a == df$abc # if one of column is abc

# using empty index will keep original data structure
df[] <- lapply(df, function) # df is still data frame
df <- lapply(df, function) # df now is a list
# remove element in list
l[["b"]] <- NULL
# add NULL in list
l["b"] <- list(NULL)

# convert element to their corresponding values
element <- rep(letters[1:4], each = 5)
value <- c(a = 1, b = 2, c = 3, d = 4)
element2value <- unname(value[element])
element2value <- c(a = 1, b = 2, c = 3, d = 4)[element]

# select in data frame with a vector of pattern
pattern <- c("A", "B")
info <- data.frame(grade = 3:1, desc = c("A", "B", "C"), fail = c(F, F, T))
id <- match(pattern, info$desc)
info[id, ]
# with duplicate element in pattern, index operation will also copy the records as new rows

# unfold duplicate records
df[rep(1:nrow(df), timesForEachRecord), ]

# which() can convert bool to int
x <- sample(10) < 4
which(x) # position

# Function
# all functions in base
objs <- mget(ls("package:base"), inherits = T)
funs <- Filter(is.function, objs)
funs_numberofparameter <- sapply(funs, function(x) length(formals(x)))
funs_numberofparameter[which.max(funs_numberofparameter)] # scan 22
funs_noparameter <- sum(sapply(funs, function(x) length(formals(x)) == 0 | is.null(x)))
funs_primitive <- Filter(is.primitive, objs)

# parameters defined or used in a function can come from upper environment
x <- 1
y <- 2
f <- function() {
  y <- 4
  return(x + y) # x come from the global
}

# codetool::findGlobals() can help find the connection
# between function and global environment
codetools::findGlobals(f)
# "{"      "+"      "<-"     "return" "x"

# you can rewrite the operation even the "("
# do not do this !!!!
`(` <- function(e1) {
  if (is.numeric(e1) && runif(1) < 0.1){
    e1 + 1
  } else {
    e1
  }
}

# everything in R is function!
x + y == `+`(x, y)
for (i in 1:2) print(i) == `for`(i, 1:2, print(i))
if (i == 1) print("yes") else print("no") == `if`(i==1, print("yes"), print("no"))
x[3] == `[`(x, 3)
{ print(1); print(2); print(3) } == `{`(print(1), print(2), print(3))
# an example of this character
sapply(1:3, `+`, 5) # also you can use "+"
# an more useful index operation
x <- list(1:3, 4:6, 7:9)
sapply(x, `[`, 2) # choose the second element in each list

# using paramter list to call function
args <- list(1:10, na.rm = TRUE)
do.call(mean, args = args) == mean(1:10, na.rm = T)

# parameter definition
f <- function(a = 1, b = 2) c(a, b) # default
f <- function(a = 1, b = a * 2) c(a, b) # default value defined from other parameters
f <- function(a = 1, b = d) {d <- (a + 1) ^ 2; c(a, b)} # defined from inner built parameters
# use missing() to determine if a parameter has been defined
i <- function(a, b) c(missing(a), missing(b))

# lazy evaluation
f <- function(x) 10 # f will not raise error because x is not used
f <- function(x) {force(x); 10} # now it will raise error
# default parameter will generate value in the function body
# thus!!!:
f <- function(x = ls()) {a <- 1, x}
f() # ls() give the environment in f()
f(ls()) # ls() give the environment of the global
# another example of lazy evaluation
# in if statement
# only when the first is TRUE, it will test the second
x <- NULL
if (!is.null(x) && x > 0) # The first is False, so it will not calculate the second
NULL > 0 == logical(0) # this is not a required if input
# sometimes you can use this to replace if
if (!is.null(x)) stop("x is null")
# can be changed as:
!is.null(x) || stop("x is null")

# ... parameter will match all parameters that have not matched

# infix function
parameter1 %function% parameter2
# custom infix function must have % at the start and end
`%+%` <- function(a, b) paste(a, b, sep = "") # like Python +
# a useful custom infix function when setting default value
`%||%` <- function(a, b) ifelse(!is.null(a), a, b)
funtion_will_return_null() %||% default_value

# replacement function
# every function you can used as f() <- x is replacement function like row.names()
# even like [<-, [[<-, $<-
# an example of custom replacement function:
`modify<-` <- function(x, position, value) {
  x[position] <- value # this is also a replacement function operation
  x
}
modify(x, 1) <- 10
# same as
x <- `modify<-`(x, 1, 10)
get("x") # get() will return the value of variable which have similar character given to it. ah...this is just a reminder
# the inner progress when you use names(x)[n] <- y
names(x)[2] <- "two" # equals to
`*tmp*` <- names(x)
`*tmp*`[2] <- "two"
names(x) <- `*tmp*`
# This can explain the error containing `*tmp*`

# find all replacement functions and determine which are primitive
repls <- funs[grep("[^<]<-$", names(funs))]
repls <- Filter(is.primitive, repls)
# write infix function of xor, intersect, union, setdiff
`%xor_%` <- function(a, b) (a | b) & !(a & b)
`%intersect_%` <- function(a, b) unique(c(a[a %in% b], b[b %in% a]))
`%union_%` <- function(a, b) unique(c(a, b))
`%setdiff_%` <- function(a, b) a[! a %in% b]
# write a replacement function to change an element in a vector randomly
`random<-` <- function(x, value) {
  x[sample(length(x), 1)] <- value
  x
}

# invisible return value
f <- function() invisible(1)
# but you can force them to be visible
(f())
# an example is <-
(a<-2) # will return 2
# this character enable us to use
a<-b<-c<-d<-e<-2

# Write a function that opens a graphics device, runs the supplied code, 
# and closes the graphics device (always, regardless of whether or not 
# the plotting code worked).
plot_pdf <- function(code) {
  pdf("test.pdf")
  on.exit(dev.off())
  code
}




