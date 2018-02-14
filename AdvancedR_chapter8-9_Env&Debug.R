# ----------------------- Environment Basic -------------------------
# Some special environment
# 1. globalenv(), default shown in RStudio, its parent env is the last 
# package that library() loads.
# 2. baseenv(), it is env of base package. Its parent env is empty env.
# 3. emptyenv(), the ancestor of all envs, and the only one that dose not have
# parent env.
# 4. environment(), current env
# We usually work under the global env, and when functions running in this
# env, they will search variables from global env, to the loaded packages (from 
# the latest to earliest), and then to base env and empty env. 

# Environment operations
# creat a new env:
e <- new.env()
parent.env(e) # find parent env of e
ls(e)
# change name-object bind in an env
e$a <- 1
e$b <- 2
e$.a <- 3
ls(e, all.names = T) # By default, ls() won't show variable name with .
ls.str(e, all.names = T) # also can use ls.str(), more useful than str()
e$a; e[["b"]]; get(".a", envir = e) # use these methods to get the bound values
rm("a", envir = e) # delete a object in the environment
# determine whether an object has existed
# use inherits = F to suppress finding in parent envs.
exists("x", envir = e, inherits = F) 
identical(globalenv(), environment()) # compare two envs.

ancestors <- function(env = globalenv()) {
  if (identical(env, emptyenv())) {
    print(environmentName(env))
  } else {
    print(environmentName(env))
    ancestor(parent.env(env))
  }
}

search2 <- function(env = globalenv()) {
  envs <- character()
  while (!identical(env, emptyenv())) {
    ename <- environmentName(env)
    if (ename == "base") ename <- "package:base"
    if (ename == "R_GlobalEnv") ename <- ".GlobalEnv"
    envs <- c(envs, ename)
    env <- parent.env(env)
  }
  return(envs)
}

# ---------------------------------- Environment recursion -----------------------------------
# pryr::where template
f <- function(..., env = parent.frame()) {
  if (identical(env, emptyenv())) {
    # base case
  } else if (success) {
    # success case
  } else {
    # recursive case
    f(..., env = parent.env())
  }
}

# modify where() in order to all environments bound by a specific name
where2 <- function(name, env = parent.frame()) {
  # collect all envs where name has binding
  env_list <- list()
  env <- pryr:::to_env(env)
  # define the recursive function
  where2.internal <- function(name, env = parent.frame()) {
    stopifnot(is.character(name), length(name) == 1)
    # when reach empty env:
    if (identical(env, emptyenv())) {
      if (length(env_list) != 0) {
        return(env_list)
      }
      stop("Can't find ", name, call. = F)
    }
    # if we find a binding:
    if (exists(name, env, inherits = F)) {
      env_list <<- c(env_list, env)
      where2.internal(name, parent.env(env))
    } else {
      where2.internal(name, parent.env(env))
    }
  }
  # start recursion
  where2.internal(name, env = parent.frame())
}

# custom get()
get2 <- function(name, env = parent.frame()) {
  stopifnot(is.character(name), length(name) == 1)
  env <- pryr:::to_env(env)
  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = F)
  }
  if (exists(name, env, inherits = F)) {
    return(env[[name]])
  } else {
    get2(name, env = parent.env(env))
  }
}

# custom a fget() function to find function object and set parameter "inherits" to determine 
# whether find in parent environment recursivly.
fget2 <- function(name, env = parent.frame(), inherits = TRUE) {
  stopifnot(is.character(name), length(name) == 1)
  env <- pryr:::to_env(env)
  if (identical(env, emptyenv())) {
    stop("Can't find function object called ", name, call. = F)
  }
  if (exists(name, env, inherits = F) && is.function(env[[name]])) {
    return(env[[name]])
  }
  if (inherits == FALSE) {
    stop("Can't find function object called ", name, " within ", environmentName(env), call. = F)
  } else {
    fget2(name, env = parent.env(env))
  }
}

# custom exists(inherits = FALSE)
exists2 <- function(name, env = parent.frame()) {
  stopifnot(is.character(name), length(name) == 1)
  env <- pryr:::to_env(env)
  name %in% ls(env, sorted = F)
}
# custom exists(inherits = TRUE)
exists3 <- function(name, env = parent.frame()) {
  stopifnot(is.character(name), length(name) == 1)
  env <- pryr:::to_env(env)
  if (identical(name, emptyenv())) return(FALSE)
  if (name %in% ls(env, sorted = F)) {
    TRUE
  } else {
    exists3(name, env = parent.frame(
    ))
  }
}

# ------------------------ Error handling -----------------------------
# change messages to errors
message2error <- function(code) {
  withCallingHandlers(code, message = function(e) stop(e))
}

# use try() to ignore errors
elements <- list(1:10, c(-1, 10), c(T, F), letters)
results <- lapply(elements, function(x) try(log(x))) # error will be generated and stored in the list
# customize a function to detect "try-error" class
is.error <- function(x) inherits(x, "try-error")
succeeded <- !sapply(results, is.error)
str(results[succeeded]) # look at successful results
str(elements[!succeeded])

# use tryCatch() handle conditions
show_condition <- function(code) {
  tryCatch(
    code,
    error = function(c) "error",
    warning = function(c) "warning",
    message = function(c) "message",
  )
}
# use tryCatch() to implement try()
try2 <- function(code, silent = F) {
  tryCatch(
    code,
    error = function(c) {
      msg <- conditionMessage(c)
      if (!silent) message(c)
      invisible(structure(msg, class = "try-error"))
    }
  )
}
# tryCatch can return more useful message
read.csv2 <- function(file, ...) {
  tryCatch(read.csv(file, ...), error = function(c) {
    c$message <- paste0(c$message, " (in ", file, ")")
    stop(c)
  })
}
# add some special operation when users try to interrupt code
# No! You can never interrupt it!
i <- 1
while (i < 3) {
  tryCatch({
    Sys.sleep(0.5)
    message("Try to escape")
  }, interrupt = function(x) {
    message("Try again")
    i <<- i + 1
  })
}

# a col_means function:
col_means <- function(df) {
  numeric <- sapply(df, is.numeric)
  numeric_cols <- df[, numeric]
  data.frame(lapply(numeric_cols, mean))
}
# a more robust one:
col_means <- function(df) {
  numeric <- vapply(df, is.numeric, logical(1))
  numeric_cols <- df[, numeric, drop = F]
  data.frame(lapply(numeric_cols, mean))
}