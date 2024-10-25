

tli_fx <- function(chl, TN, TP, secchi, timescale = 'annual'){
  if(timescale=='annual'){
    chl <- mean(chl, na.rm = TRUE)
    TN <- mean(TN, na.rm = TRUE)
    TP <- mean(TP, na.rm = TRUE)
    secchi <- mean(secchi, na.rm = TRUE)
  }
  
  tli_c <- 2.22+2.54*log10(chl) 
  tli_n <-  -3.61+3.01*log10(TN)
  tli_p <-  0.218+2.92*log10(TP) 
  tli_secchi <-  5.56+2.6*log10(1/secchi - 1/40) 
  
  tli_all <- (mean(tli_c, na.rm = TRUE) + mean(tli_n, na.rm = TRUE) + 
                mean(tli_p, na.rm = TRUE) + mean(tli_secchi, na.rm = TRUE))/4
  
  return(tli_all)
}


