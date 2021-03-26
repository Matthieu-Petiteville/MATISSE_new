#Basic function starting the chrono for benchmark of solutions
ChrStart <- function(){
  chrono_timest <<- Sys.time()
}

#Basic function printing the chrono for benchmark of solutions
ChrPrint <- function(){
  print(Sys.time()-chrono_timest)
}

#Basic function stopping the chrono and removing the variable for benchmark of solutions
ChrEnd <- function(){
  print(Sys.time()-chrono_timest)
  glob_var <- ls(pos = ".GlobalEnv")
  rm(list = glob_var[grep("chrono_timest", glob_var)] , pos=".GlobalEnv")
}


#Similar to dplyr mutate
mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}
