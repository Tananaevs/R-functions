# This function acquires daily data from a fileset of Hydrograph model-ready files
# and creates a data.frame within a current project
#USE: xxx <- extract.daily(03042)

extract.daily <- function (folder) {                                ##folder: data folder, partial match allowed
  
  extractYear <- function (k) {
    y <- as.integer(unlist(strsplit(dir_filelist[k], "[.]"))[2])   ##Extracting the extension & to integer##
    yc <- as.integer(substr(format(Sys.Date(), "%Y"), 3, 4))       ##Extracting system year & to integer##
    y <- ifelse(y <= yc, y+2000, y+1900)
    return(y)
  }
  extractGS <- function (list) return(unlist(strsplit(list[1], "[.]"))[1])
  acquireData <- function (x) {
    table <- read.delim(x, header = F, dec = ",", na.strings = "-999")
    return(as.numeric(as.character(table[, 1])))
  }
  fastwrite <- function (a, b) write.table(a, b, quote = F, sep = "\t", dec = ",", row.names = F, col.names = T)

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

  for (i in yearOrder) {                                        ## For each file
    tmp <- acquireData(dir_filelist[i])
    if (length(tmp) == 365) tmp[366] <- c(" ")
    tmp <- as.numeric(as.character(tmp))
    summ_data <- cbind(summ_data, tmp)
    }
  
  summ_data <- as.data.frame(summ_data)
  colnames(summ_data) <- yearCounter[yearOrder]
  ##print(str(summ_data))
  
 ) filename <- paste(oldwd, "/", extractGS(dir_filelist), ".daily", sep = "") ## summaries/
  print(filename)
  fastwrite(summ_data, filename)
  setwd(oldwd)
  return(summ_data)
  invisible()
}
