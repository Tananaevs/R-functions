# This function grabs monthly data from a folder with Hydrograph model-ready files,
# and calculates average and extreme values for a given month
# USE: xxx.M <- extractMonth(M)
# ATTENTION : file set folder must be set as 'Working' in order to proceed

extractMonth <- function(mon) {                                 ##mon : integer, month number, 1...12
  dir_filelist <- list.files("./", ignore.case = TRUE)          ## Read the directory, extract filenames
  yearCounter <- numeric(length(dir_filelist))

  for (i in 1:length(dir_filelist)) {
    yearCounter[i] <- extractYear(i)       ## Year list
  }
  
  statsMonth <- data.frame()
  
  yearOrder <- order(yearCounter)          ## Derive order
  print(yearCounter[yearOrder])
  
  longM <- c(1, 3, 5, 7, 8, 10, 12)
  shortM <- c(4, 6, 8, 10)
  shitM <- 2
  
  if (any(mon == longM) == TRUE) len <- c(31, 31)
  else if (any(mon == shortM) == TRUE) len <- c(30, 30)
  else len <- c(28, 29)
  
  firstDayN <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  firstDayL <- c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336)
  
  createNSeq <- function (m) seq(from = firstDayN[m], length.out = len[1])
  createLSeq <- function (m) seq(from = firstDayL[m], length.out = len[2])
  
  for (i in yearOrder) {
    dataTable <- read.delim(dir_filelist[i], header = F, dec = ",")   ## Will it work as ordered?!
    
    if (length(dataTable) == 365) n <- createNSeq(mon)
    else n <- createLSeq(mon)
    
    dataMonth <- as.numeric(as.character(dataTable[n, 1]))
    ##print(c(mean(dataMonth), min(dataMonth), max(dataMonth)))
    
    statsMonth <- rbind(statsMonth, c(mean(dataMonth), min(dataMonth), max(dataMonth)))
    ##print(dataMonth)
  }
  
  statsMonth <- cbind(yearCounter[yearOrder], statsMonth)
  colnames(statsMonth) <- c("year", "avg", "minQ", "maxQ")
  return(statsMonth)
}
