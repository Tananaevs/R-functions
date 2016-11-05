# This function grabs daily data for a given month and station for all years available
# and drops it to a data.frame object
# USE : xxx.MD <- extractMonthData(M)
# ATTENTION : folder must be set as 'Working' in order to proceed

extract.MonthData <- function(mon) {
  dir_filelist <- list.files("./", ignore.case = TRUE)          ## Read the directory, extract filenames
  yearCounter <- numeric(length(dir_filelist))

  for (i in 1:length(dir_filelist)) {
    yearCounter[i] <- extractYear(i)       ## Year list
  }
  
  monthData <- numeric()
  
  yearOrder <- order(yearCounter)          ## Derive order
  ##print(yearCounter[yearOrder])
  
  longM <- c(1, 3, 5, 7, 8, 10, 12)
  shortM <- c(4, 6, 8, 10)
  shitM <- 2
  
  if (any(mon == longM) == TRUE) len <- c(31, 31)
  else if (any(mon == shortM) == TRUE) len <- c(30, 30)
  else if (mon == shitM) len <- c(28, 29)
  
  firstDayN <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  firstDayL <- c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336)
  
  createNSeq <- function (m) seq(from = firstDayN[m], length.out = len[1])
  createLSeq <- function (m) seq(from = firstDayL[m], length.out = len[2])
  
  for (i in yearOrder) {
    dataTable <- read.delim(dir_filelist[i], header = F, dec = ",", na.strings = "-999")   
    
    if (length(dataTable[, 1]) == 365) n <- createNSeq(mon)
    else n <- createLSeq(mon)

    dataMonth <- as.numeric(as.character(dataTable[n, 1]))
    if (length(dataTable[, 1]) == 365 && mon == 2) dataMonth[29] <- c("")
    print(dataMonth[29])
    
    monthData <- cbind(monthData, dataMonth)
  }
  
  monthData <- as.data.frame(monthData)
  colnames(monthData) <- yearCounter[yearOrder]
  return(monthData)
}
