# This function grabs data from a data.frame object falling within manually imposed limits
# and drops if in a new time.series object
# USE: extractTS(03042.Lena.Tabaga)
# ATTENTION__1: no assignment needed
# ATTENTION__2: function produces numerous errors if some yaers are NA or absent!

extractTS <- function (obj) {                              ##obj: a data frame, created with extractSummary function
  require(stats)
  
  attribass <- function (i, j) c(obj[i, 1], obj[j, 1], 1)

##    if (anyNA(obj[, 2]) == FALSE) {
##    i3 <- i2 <- i1 <- 1
##    j3 <- j2 <- j1 <- dim(obj)[1]
##    print(c(i1, j1))
##    }
  
##  else {
  i1 <- as.numeric(readline(prompt = "First row, avg Q "))
  j1 <- as.numeric(readline(prompt = "Last row, avg Q "))
  i2 <- as.numeric(readline(prompt = "First row, max Q "))
  j2 <- as.numeric(readline(prompt = "Last row, max Q "))
  i3 <- as.numeric(readline(prompt = "First row, min Q "))
  j3 <- as.numeric(readline(prompt = "Last row, min Q "))
##  }
  
  ##print(c(i1, j1, i2, j2, i3, j3))
  ##print(c(attribass(i1, j1), attribass(i2, j2), attribass(i3, j3)))
  
  avg.ts <- na.fail(as.ts(obj[i1:j1, 2]))
  attr(avg.ts, 'tsp') <- attribass(i1, j1)
  max.ts <- na.fail(as.ts(x = obj[i2:j2, 3]))
  attr(max.ts, 'tsp') <- attribass(i2, j2)
  min.ts <- na.fail(as.ts(x = obj[i3:j3, 4]))
  attr(min.ts, 'tsp') <- attribass(i3, j3)
  
  if (mean(min.ts) == 0) tmp <<- list(avg.ts, max.ts)
  else tmp <<- list(avg.ts, max.ts, min.ts)
  assign(paste(deparse(substitute(obj)), ".ts", sep = ""), tmp, envir = globalenv())
  rm(tmp, envir = globalenv())
  
  T <- matrix(nrow = 3, ncol = 3)
  f1 <- function (i) return(obj[i, 1])
  T <- cbind(c(f1(i1), f1(i2), f1(i3)), c(f1(j1), f1(j2), f1(j3)))
  rownames(T) <- c('avg Q', 'max Q', 'min Q')
  return(T)
  invisible()
  
}
