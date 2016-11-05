# This function performs statistical tests and evaluations for hydrological time series
# Test list is openly modifiable; Hodges-Lehmann estimator is not fully implemented for a moment
# USE : xxx.s <- extractStat(tsobj)

extractStat <- function (tsobj) {            ##tsobj : time series object created with extractTS function
  require(stats)
  require(nortest)
  require(climtrends)
  require(boot)
  require(trend)
  require(Kendall)
  require(pspearman)
  require(pracma)
  require(zyp)
  require(DescTools)
  
  extractGS <- function (list) return(unlist(strsplit(list[1], "[.]"))[1])
  
  wd <- getwd()
  dataSet <- get(deparse(substitute(tsobj)))
  
  first <- function (x) start(x)[1]
  last <- function (x) end(x)[1]
  ##mean30 <- function (x) mean(tail(x, 30), na.rm = T)
  co.var <- function (x) sd(x)/mean(x)
  ##ad.s <- function (x) ad.test(x)$statistic
  ##ad.p <- function (x) ad.test(x)$p.value
  ##sf.s <- function (x) sf.test(x)$statistic
  ##sf.p <- function (x) sf.test(x)$p.value
  ##normality <- function (x) ifelse(ad.test(x)$p.value > 0.05 && sf.test(x)$p.value > 0.05, 1, 0)
  pett.K <- function (x) pettitt.test(x)$statistic
  pett.p <- function (x) pettitt.test(x)$p.value 
  pett.y <- function (x) seq(start(x)[1], end(x)[1], 1)[pettitt.test(x)$estimate]
  BRT.y <- function (x)  seq(start(x)[1], end(x)[1], 1)[which.min(BuishandRangeTest(x))]
  ##vnr.r <- function (x) VonNeumannRatioRank(x)
  ##outESD <- function (x) tmp1 <<- ifelse(FindOutliersESDtest(x)[1, 2] < FindOutliersESDtest(x)[1, 3], 0, 1)
  ##outTM <- function (x) {                           ##Tietjen-Moore statistics
    ##tmtest <- FindOutliersTietjenMooreTest(x, 1)
    ##tmp2 <<- ifelse(tmtest$T > tmtest$Talpha, 0, 1)
  ##}
  ##outliers <- function (x) tmp1 + tmp2
  ##outLB <- function (x) Box.test(x, lag = 10, type = "Ljung-Box")$p.value
  ##outLBd <- function (x) Box.test(detrend(x), lag = 10, type = "Ljung-Box")$p.value
  tsslope <- function (x) sens.slope(x)$b.sen
  mktau <- function (x) MannKendall(x)$tau
  mkp <- function (x) MannKendall(x)$sl
  sp.rho <- function (x) spearman.test(x, 1:length(x), approximation = 'AS89')$estimate
  sp.p <- function (x) spearman.test(x, 1:length(x), approximation = 'AS89')$p.value
  ar.1 <- function (x) acf(x, plot = F)$acf[2]
  ##hurst <- function (x) hurstexp(x, display = F)$Hs
  zyp.a1 <- function (x) zyp.yuepilon(x)[8]
  zyp.tau <- function (x) zyp.yuepilon(x)[5]
  zyp.p <- function (x) zyp.yuepilon(x)[6]
  zyp.lin <- function (x) zyp.yuepilon(x)[2]
  zyp.int <- function (x) zyp.yuepilon(x)[11]
  ##hl1 <- function (x) HodgesLehmann(x[1:pettitt.test(x)$estimate], conf.level = NA)
  ##hl2 <- function (x) HodgesLehmann(x[pettitt.test(x)$estimate:tail(x, 1)], conf.level = NA)
  ##hlrelmag <- function (x) (hl2(x) - hl1(x))/hl1(x) * 100
  ##hlabsmag <- function (x) (hl2(x) - hl1(x))/length(x) * 10
  
  fapply <- function(x) c(Start = first(x), End = last(x), Nobs = length(x), mean = mean(x), Cv = co.var(x), 
                          MKt = mktau(x), MKp = mkp(x), SRC = sp.rho(x), SRC.p = sp.p(x), TS.slope = tsslope(x), AR1 = ar.1(x), 
                          TFPW.tau <- zyp.tau(x), TFPW.p <- zyp.p(x), TFPW.sen = zyp.lin(x), TFPW.int = zyp.int (x),
                          TFPW.a1 = zyp.a1(x), Pett.K = pett.K(x), Pett.p = pett.p(x), Pett.y = pett.y(x),
                          BRT.year <- BRT.y(x))
  
  
  summary <- do.call(rbind, lapply(dataSet, fapply))
  summary <- as.data.frame(summary)
  
  filename <- paste(wd, "/", deparse(substitute(tsobj)), ".stat", sep = "") ## summaries/
  write.table(summary, file = filename, quote = F, sep = '\t', col.names = T, row.names = F, dec = ",")
  
  ##rm(tmp1, tmp2, envir = globalenv())
  return(summary)
}
