# Thic code extracts hydrological summary from a set of binary files in a single directory
# containing Hydrograph model-ready data

extractSummary <- function (folder) {                               ##folder: Folder name, partial match allowed

  extractYear <- function (k) {                                     ##Function needed to access files with *.YY extension
    y <- as.integer(unlist(strsplit(dir_filelist[k], "[.]"))[2])    ##Extracting the extension & to integer##
    yc <- as.integer(substr(format(Sys.Date(), "%Y"), 3, 4))        ##Extracting system year & to integer##
    y <- ifelse(y <= yc, y+2000, y+1900)                            ##ATTENTION!! Will not work for years more than 100 years ago
    return(y)
  }
  extractGS <- function (list) return(unlist(strsplit(list[1], "[.]"))[1])  ##Calculate average, max and min for each year 
  avgQ <- function (x) ifelse(anyNA(x) == TRUE, NA, mean(x))
  maxQ <- function (x) max(x, na.rm = T)
  minQ <- function (x) min(x, na.rm = T)
  fapply1 <- function (x) c(AvgQ = avgQ(x), MaxQ = maxQ(x), MinQ = minQ(x))
  acquireData <- function (x) {
    table <- read.delim(x, header = F, dec = ",", na.strings = "-999")
    return(as.numeric(as.character(table[, 1])))
  }
  fastwrite <- function (a, b) write.table(a, b, quote = F, sep = "\t", dec = ",", row.names = F, col.names = T)
  fastread <- function (a) read.table(a, header = T, sep = "\t", dec = ".")
  
  oldwd <- getwd()
  dir_filelist <- list.files(grep(folder, list.dirs(), fixed = T, value = T), ignore.case = TRUE) ##Partial folder name match
  newwd <- as.character(grep(folder, list.dirs(), fixed = T, value = T))
  setwd(newwd)
  
  yearCounter <- numeric(length(dir_filelist))
  for (i in 1:length(yearCounter)) {
    yearCounter[i] <- extractYear(i)       ## Year list
  }
  yearOrder <- order(yearCounter)          ## Derive order
  
  summ_data <- vector(mode = "numeric", 0)
  
  for (i in yearOrder) {
    tmp <- acquireData(dir_filelist[i])
    summ_data <- rbind(summ_data, fapply1(tmp))
  }

  summ_data <- cbind(yearCounter[yearOrder], summ_data)
  summ_data <- as.data.frame(summ_data)
  colnames(summ_data) <- c("Year", "Avg", "Max.Q", "Min.Q")
  
  filename <- paste(oldwd, "/", extractGS(dir_filelist), ".summ", sep = "") ## "/summaries/", 
  fastwrite(summ_data, filename)
  
  setwd(oldwd)
  return(summ_data)
  invisible()
}
