# ------------------------- S3 -------------------------- 
# in S3 class, methods are belong to functions
# This function is called generic function, like mean, print, t, etc..
pryr::ftype(print) # [1] "s3"      "generic"
# The format of method is same as calling method in traditional class
# like print.factor, mean.Data, t.data.frame
pryr::ftype(t.data.frame) # [1] "s3"     "method"

# classrification of generic, method and class in R
# for a specific function, like plot: 
# plot is a generic function;
# plot.ts is a method for class ts;
# ts is the class.

# use method to see methods belong to a generic
methods("mean")
# and use class to see all generic functions that supply corresponding methods
methods(class = "ts")

# define a S3 object is very simple
foo <- structure(list(), class = "foo") # one step
foo <- list(); class(foo) <- "foo" # two step, just give a base object a new class name
# most of S3 objects will provide a construction function
foo <- function(x) {
  if (!is.numeric(x)) stop("X must be numeric")
  structure(list(x), class = "foo")
} # make sure the custom class will have correct elements.

# define new S3 method, generic function and method dispatch
f <- function(x) UseMethod("f") # a new generic function
f.a <- function(x) "Class A" # a new method of generic function f
f.default <- function(x) "Unknown class" # another method of f
f(structure(list(), class = "a")) # automatically use f.a
f(structure(list(), class = "c")) # no method for c, thus using f.default
# can use method directly, because it is also a function
# this can generate errors since it will return same result if class is c
# however, skip method dispatch can improve program performance
f.a(structure(list(), class = "a")) 

# If we define a class test, and use t(),
# it will cause R to call t.test.default(), unless you have created a
# method for t() as t.test()
# (Emmmmm... very interesting results...)

# Which base generics has the greatest number of defined method?
objs <- mget(ls("package:base"), inherits = T)
funs <- Filter(is.function, objs)
generics <- Filter(function(x) {"generic" %in% pryr::ftype(x)}, funs)
sort(
  lengths(sapply(names(generics), function(x) methods(x), USE.NAMES = T)),
  decreasing = T
)[1]

# It will be confusing when facing internal generic and implicit class
# for example:
f <- function() 1
g <- function() 2
class(g) <- "function" # explicit

class(f) # "function"
class(g) # "function"

length.function <- function(x) "function" # define a new method for length()
length(f) # 1...
length(g) # "function"
# Now you see even the class() shows that f and g have same class "function", but
# length() will not dispatch length.function method for f.
# This is because the internal generic will not dispatch method for 
# implicit class of base objects.
# You can use attribute() to see that g has an explicit class $class "function",
# but f is implicit. so length() can not find f's class though f have class
# "function" shown by class().

# ------------------------- S4 -------------------------- 
# define class and construct objects
setClass(
  "Person", # class name
  slots = list(name = "character", age = "numeric") # slot list
  # do not have inheritance
)
setClass(
  "Employee",
  slots = list(boss = "Person"),
  contains = "Person" # parent class
)
alice <- new("Person", name = "Alice", age = 40) # construct new object
john <- new("Employee", name = "John", age = 20, boss = alice) # a new object inherit from other class
# if one S4 object inherit from a S3 class or base class:
setClass(
  "RangedNumeric",
  contains = "numeric",
  slots = list(min = "numeric", max = "numeric")
)
rn <- new("RangedNumeric", 1:11, min = 1, max = 10)
# it will contain a .Data field which include base or S3 objects
# once you modify the definition of one class, you should re-generate all its objects

# construct new methods and generic functions
# convert existed functions to the generic:
setGeneric("union")
# construct a brand new generic function
setGeneric("myGeneric", function(x) {
  standardGeneric("myGeneric")
})
# add new method, for example, for union:
setMethod(
  "union",
  c(x = "data.frame", y = "data.frame")
  function(x, y) {
    unique(rbind(x, y))
  }
)

# ----------------------------- RC --------------------------
# R base packages do not have RC system, so we construct one first
# RC is suitable to describe state objects
Account <- setRefClass("Account", # define name
                       fields = list(balance = "numeric"), # define fields
                       methods = list(
                         withdraw = function(x) {
                           # use <<- to change field's value
                           balance <<- balance - x
                         },
                         deposit = function(x) {
                           balance <<- balance + x
                         }
                       ) # define methods
                    ) # new RC class
a <- Account$new(balance = 100) # new object
a$balance <- 200 # retrive and change value of one field by $
b <- a # RC is mutable
a$balance <- 150 # so when you change a
b #150 # b will change to the same, not same as traditional <- operation
c <- a$copy() # thus RC provide a method copy()
a$balance <- 300 # this time when a change
b; c # 300; 150, c will not change
a$deposit(100) # also use $ to call methods
# A new class with inheritance
NoOverdraft <- setRefClass(
  "NoOverdraft", # class name
  contains = "Account", # Inheritance
  methods = list(
    withdraw = function(x) {
      if (balance < x) stop("Not enough money")
      balance <<- balance - x
    }
  ) # define method
)
accountJohn <- NoOverdraft$new(balance = 100) # new object
accountJohn$withdraw(300)
