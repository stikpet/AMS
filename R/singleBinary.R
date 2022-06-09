
#' Frequency Table
#' Generates a frequency table with the same info as SPSS would
#' 
#' @param data A dataframe
#' @param field The field within the dataframe to use as string
#' @return table A table with frequencies, percentage, valid percentage and cumulative percentage
#' 
#' @details 
#' For more info on this test with R: https://youtu.be/VjJCzeRa49w
#'    
#' @author 
#' P. Stikker
#' Please visit: https://PeterStatistics.com
#' YouTube channel: https://www.youtube.com/stikpet
#'  
#' @export
frequencyTable <- function(data, field){
  freq<-table(data[field])
  freq2<-table(data[field], exclude=NULL)
  prop<-prop.table(freq)
  prop2<-prop.table(freq2)
  cumf<-cumsum(freq)
  cumrf<-cumsum(prop)
  
  #Mixed table (mimic SPSS)
  f<-as.data.frame(freq2,stringsAsFactors=FALSE)
  rf<-as.data.frame(prop2,stringsAsFactors=FALSE)
  vp<-as.data.frame(prop,stringsAsFactors=FALSE)
  crf<-as.data.frame(cumrf,stringsAsFactors=FALSE)
  
  vp2<-rbind(vp,c(NA,NA))
  crf2<-rbind(crf,c(NA))
  
  myTable<-f
  myTable$Percent<-rf$Freq*100
  myTable$ValidPercent<-vp2$Freq*100
  myTable$CumulativePercent<-crf2$cumrf*100
  myTable<-rbind(myTable,c("TOTAL",sum(myTable$Freq),sum(myTable$Percent),sum(myTable$ValidPercent, na.rm = TRUE),NA))
  
  return(myTable)
}

#' Single Binary Test - One-sample binomial
#' 
#' This function will perform an exact one-sample binomial test, two-sided
#' It assumes the probability of success in the population to be 0.5
#' 
#' @param data A dataframe
#' @param field The field within the dataframe to use as string
#' @return pVal The two-sided significance (p-value)
#'
#' @details      
#' As number of success it picks the first category found.
#' For more info on this test with R: https://youtu.be/wUFNO7AjOGo
#'    
#' @author 
#' P. Stikker
#' Please visit: https://PeterStatistics.com
#' YouTube channel: https://www.youtube.com/stikpet
#'  
#' @export
bi_ts_binomial <- function(data, field){
  myVar<-na.omit(data[field])
  freq<-table(myVar)
  
  #check if binary
  if(length(freq)!=2){
    return('field is not binary')
  }
  
  #Determine number of successes
  k<-freq[1]
  
  #Determine total sample size
  n<-sum(freq)
  
  #Test if expected both groups to be equal  
  #Perform binomial test
  pVal <- binom.test(k,n)
  
  return(unname(pVal$p.value))
}


#' Single Binary Test - One-Sample Score test
#' 
#' This function performs a one-sample score test.
#' It assumes the probability of success in the population to be 0.5
#' 
#' @param data A dataframe
#' @param field The field within the dataframe to use as string
#' @param corr A boolean TRUE or FALSE to indicate the use of a Yates correction
#' @return list With the z-statistic and the two-sided significance (p-value)
#' 
#' @details 
#' The test uses:
#' \deqn{z = \frac{k - \mu}{\sigma}}
#' With \eqn{\mu = n\times p_0}, and \eqn{\sigma = \sqrt{\mu \times \left( 1 - p_0\right)}}
#' Where \eqn{k} is the number of successes, \eqn{n} is the total sample size and \eqn{p_0} is the proportion according to the null hypothesis (set to 0.5)
#' For the Yates continuity correction the formula is adjusted to:
#' \deqn{z_{Yates} = \frac{\left|x - \mu\right| - 0.5}{\sigma}}
#' 
#' For more info on this test with R: https://youtu.be/27n2ko-S5U0
#' 
#' @author 
#' P. Stikker
#' Please visit: https://PeterStatistics.com
#' YouTube channel: https://www.youtube.com/stikpet
#' @export
bi_ts_score <- function(data, field, corr=FALSE){
  myVar<-na.omit(data[field])
  freq<-table(myVar)
  
  #check if binary
  if(length(freq)!=2){
    return('field is not binary')
  }
  
  # We determine the number of successes (k):
  k <- freq[1]
  
  # And the number of trials, i.e. the sample size:
  n <- sum(freq)
  
  if(corr){
    testResults <- prop.test(k, n, correct = TRUE)
    pVal = unname(testResults$p.value)
  } else{
    testResults <- prop.test(k, n, correct = FALSE)
    pVal = unname(testResults$p.value)
  }
  zVal = unname(sqrt(testResults$statistic))
  
  results <- data.frame(zVal=zVal, pVal=pVal)
  return(results)
}

#' Single Binary Test - One-Sample Wald test
#' 
#' This function performs a one-sample Wald test.
#' It assumes the probability of success in the population to be 0.5
#' @param data A dataframe
#' @param field The field within the dataframe to use as string
#' @param corr A boolean TRUE or FALSE to indicate the use of a Yates correction
#' @return list With the z-statistic and the two-sided significance (p-value)
#' 
#' @details
#' The test uses:
#' \deqn{z = \frac{k - \mu}{s}}
#' With \eqn{\mu = n\times p_0}, and \eqn{s = \sqrt{k\times\left(1 - \frac{k}{n}\right)}}
#' Where \eqn{k} is the number of successes, \eqn{n} is the total sample size 
#' and \eqn{p_0} is the proportion according to the null hypothesis (set to 0.5)
#' For the Yates continuity correction the formula is adjusted to:
#' \deqn{z = \frac{\left|k - \mu\right| - 0.5}{s}}
#' 
#' For more info on this test with R: https://youtu.be/27n2ko-S5U0
#' 
#' @author
#' P. Stikker
#' Please visit: https://PeterStatistics.com
#' YouTube channel: https://www.youtube.com/stikpet
#' @export
bi_ts_wald <- function(data, field, corr=FALSE){
  myVar<-na.omit(data[field])
  freq<-table(myVar)
  
  #check if binary
  if(length(freq)!=2){
    return('field is not binary')
  }
  
  # We determine the number of successes (k):
  k <- freq[1]
  
  # And the number of trials, i.e. the sample size:
  n <- sum(freq)
  s <- sqrt(k*(1-k/n))
  mu = n*0.5
  
  if(corr){
    zVal <- unname((abs(k-mu)-0.5) / s)
  } else{
    zVal <- unname((k - mu) / s)
  }
  
  pVal = pnorm(-abs(zVal))*2
  
  return(c(zVal, pVal))
}


#' Single Binary Test - Pearson Chi-Square Goodness of Fit test
#' 
#' This function performs a Pearson Chi-Square Goodness of Fit test.
#' It assumes the probability of success in the population to be 0.5
#' 
#' @param data A dataframe
#' @param field The field within the dataframe to use as string
#' @param corr Correction to use, either 'none' (default), 'yates' for Yates, 'pearson' for Pearson or 'williams' for Williams
#' @return list With the chi-square value, degrees of freedom, sample size, and the two-sided significance (p-value)
#' 
#' @detials
#' The test uses:
#' \deqn{\chi^2 = \sum_{i=1}^k\frac{\left(F_i-E_i\right)^2}{E_i}}
#' Where \eqn{k} is the number of categories (i.e. 2), \eqn{F_i} is the observed frequency of category i, 
#' and \eqn{E_i} the expected frequency (i.e. n/2)
#' For the Yates continuity correction the formula is adjusted to:
#' \deqn{\chi_{\text{Yates}}^2 = \sum_{i=1}^k\frac{\left(\left|F_i-E_i\right|-0.5\right)^2}{E_i}}
#' For the E. Pearson correction:
#' \deqn{\chi_{\text{E.Pearson}}^2 = \chi^2\times\frac{n-1}{n}}
#' For the Williams correction:
#' \deqn{\chi_{\text{Williams}}^2 = \frac{\chi^2}{q}}
#' with:
#' \deqn{q = 1+\frac{k^2-1}{6\times n\times df}}
#' 
#' For more info on this test with R: https://youtu.be/27n2ko-S5U0
#' 
#' @author 
#' P. Stikker
#' Please visit: https://PeterStatistics.com
#' YouTube channel: https://www.youtube.com/stikpet
#' @export
bi_ts_chi2Gof_Pearson <- function(data, field, corr='none'){
  myVar<-na.omit(data[field])
  freq<-table(myVar)
  
  #check if binary
  if(length(freq)!=2){
    return('field is not binary')
  }
  
  # We determine the number of successes (k):
  k <- freq[1]
  
  # And the number of trials, i.e. the sample size:
  n <- sum(freq)
  
  if (corr=='yates'){
    chi2Gof <- prop.test(k, n, correct = TRUE)
    pVal <- chi2Gof$p.value
    chi2Val <- unname(chi2Gof$statistic)
  } else{
    chi2Gof <- prop.test(k, n, correct = FALSE)
    pVal <- chi2Gof$p.value
    chi2Val <- unname(chi2Gof$statistic)
    
    if(corr=='pearson'){
      chi2Val <- chi2Val * (n - 1)/n
      pVal <- pchisq(chi2Val, 1, lower.tail = FALSE)
    } else if(corr=='williams'){
      q = 1 + 1/(2 * n)
      chi2Val <- chi2Val / q
      pVal <- pchisq(chi2Val, 1, lower.tail = FALSE)
    }
    
  }  
  # And the degrees of freedom:
  df = unname(chi2Gof$parameter)
  
  return(c(chi2Val, df, n, pVal))
}



#' Single Binary Test - G Goodness of Fit test
#' 
#' This function performs a G Goodness of Fit test (a.k.a. likelihood ratio GoF).
#' It assumes the probability of success in the population to be 0.5
#' @param data A dataframe
#' @param field The field within the dataframe to use as string
#' @param corr Correction to use, either 'none' (default), 'yates' for Yates, 'pearson' for Pearson or 'williams' for Williams
#' @return list With the G value, degrees of freedom, sample size, and the two-sided significance (p-value)
#' 
#' @details
#' The test uses:
#' \deqn{G = 2\times\sum_{i=1}^{k}\left(O_i\times ln\left(\frac{F_i}{E_i}\right)\right)}
#' Where \eqn{k} is the number of categories (i.e. 2), \eqn{F_i} is the observed frequency of category i, 
#' and :math:`E_i` the expected frequency (i.e. n/2)
#' 
#' For the Yates continuity correction the formula is adjusted to:
#' \deqn{G_{Yates} = 2\times \sum_{i=1}^k\left(F_i^{'}\times ln\left(\frac{F_i^{'}}{E_i}\right)\right)}
#' with:
#' \deqn{F_i^{'}=\begin{cases} F_i-0.5 & \text{ if } F_i> E_i \\ F_i+0.5 & \text{ if } F_i<E_i \\ F_i & \text{ if } F_i=E_i\end{cases}}
#' 
#' For the E. Pearson correction:
#' \deqn{G_{\text{E.Pearson}} = G\times\frac{n-1}{n}}
#' 
#' For the Williams correction:
#' \deqn{G_{\text{Williams}} = \frac{G}{q}}
#' with:
#' \deqn{q = 1+\frac{k^2-1}{6\times n\times df}}
#' 
#' For more info on this test with R: https://youtu.be/27n2ko-S5U0
#' @author 
#' Author: P. Stikker
#' Please visit: https://PeterStatistics.com
#' YouTube channel: https://www.youtube.com/stikpet
#' @export
bi_ts_chi2Gof_G <- function(data, field, corr='none'){
  myVar<-na.omit(data[field])
  freq<-table(myVar)
  
  #check if binary
  if(length(freq)!=2){
    return('field is not binary')
  }
  
  # We determine the number of successes (k):
  k <- freq[1]
  
  # And the number of trials, i.e. the sample size:
  n <- sum(freq)
  
  expFreq <- sum(freq) / length(freq)
  
  if (corr=='yates'){
    G = 0
    for(i in 1:2){
      if(freq[i]>expFreq){
        obsAdj = freq[i] - 0.5
      } else if(freq[i]<expFreq){
        obsAdj = freq[i] + 0.5
      } else{
        obsAdj = freq[i]
      }
      G <- G + obsAdj * log(obsAdj/expFreq)
    }
    G <- 2*G
  } 
  else{
    # no corrections
    G = 2*sum(freq * log(freq/expFreq))
    
    if(corr=='pearson'){
      G <- G * (n - 1)/n
    } else if(corr=='williams'){
      q = 1 + 1/(2 * n)
      print(q)
      G <- G / q
    }
  }
  
  pVal = pchisq(G, 1, lower.tail = FALSE)
  
  return(c(unname(G), unname(pVal)))
}


#' Single Binary Effect Size - Cohen's H_2
#' 
#' This function performs calculates Cohen's H_2.
#' @param data A dataframe
#' @param field The field within the dataframe to use as string
#' @return list With a qualification and the effect size measure
#' 
#' @details 
#' Rosnow and Rosenthal (2003) mention as effect size for binary (they call it dichotomous) 
#' data Cohen's g and Cohen's h. Cohen's h is also the effect size used for two proportions in the NCSS software (n.d.).
#' 
#' Cohen's h from one sample can be calculated by (Cohen, 1988, p. 202):
#' \deqn{h_2 = \phi_1 - \phi_c}
#' with \eqn{\phi_i = 2\times arcsin\left(\sqrt{p_i}\right)}
#' 
#' The \eqn{p_i} is simply the proportion of category i and 
#' \eqn{p_c} the expected proportion in the population for that category. 
#' 
#' Cohen actually gives thresholds for 'small', 'medium', and 'large' effect sizes for h at .20, .50, and .80 resp. (Cohen, 1988, pp. 184-185). 
#' I would interpret this as:
#' | Cohen h | qualification |
#' |-------------|--------------|
#' | 0 < .20 | Negligible |
#' | 0.20 < 0.50 | Small |
#' | 0.50 < 0.80 | Medium |
#' | 0.80 or more | Large |
#' 
#' Note that this table is for Cohen's h for paired data. He gives a conversion that can be used from the one-sample:
#' \deqn{h = h_2 \times \sqrt{2}}
#' 
#' For more info on Cohen's h2 with R: https://youtu.be/e6JanIx0xEY
#' 
#' @references 
#' Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed). Hillsdale, N.J: L. Erlbaum Associates.
#' 
#' Rosnow, R. L., & Rosenthal, R. (2003). Effect sizes for experimenting psychologists. *Canadian Journal of Experimental Psychology/Revue Canadienne de Psychologie Expérimentale, 57*(3), 221–237. doi:10.1037/h0087427
#' 
#' NCSS. (n.d.). Tests for two proportions using effect size. In PASS Sample Size Software (pp. 199-1-199–10). Retrieved from https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/PASS/Tests_for_Two_Proportions_using_Effect_Size.pdf
#' 
#' @author 
#' P. Stikker
#' Please visit: https://PeterStatistics.com
#' YouTube channel: https://www.youtube.com/stikpet
#' @export
bi_es_cohenH2 <- function(data, field){
  myVar<-na.omit(data[field])
  freq<-table(myVar)
  
  #check if binary
  if(length(freq)!=2){
    return('field is not binary')
  }
  
  # We determine the number of successes (k):
  k <- freq[1]
  
  # And the number of trials, i.e. the sample size:
  n <- sum(freq)  
  
  prop <- freq / sum(freq)
  
  #Expected proportion
  expProp <- 0.5
  
  #Cohen's h_2
  phis <- 2 * asin(sqrt(prop))
  phiNull <- 2 * asin(sqrt(expProp))
  h2 <- unname(phis[2]-phiNull)
  
  h <- h2 * sqrt(2)
  #Using Cohen (1988, p. 198):
  habs <- abs(h)
  if (habs < 0.2){
    qual = "negligible"
  } else if (habs < 0.5){
    qual = "small"
  } else if (habs < 0.8){
    qual = "medium"
  } else {
    qual = "large" 
  }
  
  return(c(qual, h2))
  
}



#' Single Binary Effect Size - Cohen's g
#' 
#' This function performs calculates Cohen's g. 
#' 
#' @param data A dataframe
#' @param field The field within the dataframe to use as string
#' @return list With a qualification and the effect size measure
#' 
#' @details 
#' Rosnow and Rosenthal (2003) mention as effect size for binary (they call it dichotomous) 
#' data Cohen's g and Cohen's h. 
#' 
#' Cohen's g can be calculated by:
#' \deqn{g = p_i - 0.50}
#' The \eqn{p_i} is simply the proportion of category i. 
#' 
#' The maximum value for Cohen's g would be 0.5, which would occur 
#' if the sample proportion for one of the two categories is either 0 (0%) or 1 (100%).
#' 
#' Cohen (1988, p. 147) actually gives thresholds for 'small', 'medium', and 'large' effect sizes 
#' for this g at .05, .15, and .25 resp. (Cohen, 1988, pp. 147-149). I would interpret this as:
#' |Cohen g | qualification |
#' |-------------|------------|
#' | 0 < .05 | Negligible |
#' | 0.10 < 0.15 | Small |
#' | 0.15 < 0.25 | Medium |
#' | 0.25 or more | Large |
#' 
#' For more info on Cohen's g with R: https://youtu.be/25CQR5OQTK0
#' 
#' @references 
#' Cohen, J. (1988, p. 147). *Statistical power analysis for the behavioral sciences* (2nd ed). Hillsdale, N.J: L. Erlbaum Associates.
#' 
#' Rosnow, R. L., & Rosenthal, R. (2003). Effect sizes for experimenting psychologists. *Canadian Journal of Experimental Psychology/Revue Canadienne de Psychologie Expérimentale, 57*(3), 221–237. doi:10.1037/h0087427
#' 
#' @author
#' P. Stikker
#' Please visit: https://PeterStatistics.com
#' YouTube channel: https://www.youtube.com/stikpet
#' @export
bi_es_coheng <- function(data, field){
  myVar<-na.omit(data[field])
  freq<-table(myVar)
  
  #check if binary
  if(length(freq)!=2){
    return('field is not binary')
  }
  
  # We determine the number of successes (k):
  k <- freq[1]
  
  # And the number of trials, i.e. the sample size:
  n <- sum(freq)  
  
  prop <- freq / n
  
  CohenG <- unname(prop[1]-0.5)
  gAbs <- abs(CohenG)
  
  #Using Cohen (1988, p. 147-149):
  if (gAbs < 0.05){
    qual = "negligible"
  } else if (gAbs < 0.15){
    qual = "small"
  } else if (gAbs < 0.25){
    qual = "medium"
  } else {
    qual = "large" 
  }
  
  return(c(qual, CohenG))
  
}


#' Single Binary Effect Size - Alternative Ratio
#' 
#' This function performs calculates the Alternative Ratio (a.k.a. Relative Risk). 
#' @param data A dataframe
#' @param field The field within the dataframe to use as string
#' @return list with the two alternative ratios
#' 
#' @details 
#' JonB on CrossValidated suggests to use Relative Risks as effect size measure 
#' for a binomial test, which the NCSS calls Alternative Ratio (n.d.).
#' 
#' The Alternative Ratio can be calculated by:
#' \deqn{AR = \frac{p}{\pi}}
#' It is simply the actual proportion (:math:`p`) divided by the expected proportion (\eqn{\pi}). 
#' If the two are equal, the result will be 1. 
#' For the interpretation it is therefor easier to look at the difference with 1. 
#' It then informs you how much more or less likely the category is than was expected.
#' 
#' For more details on Alternative Ratio with R see: https://youtu.be/V5tug-2Zqjc
#' 
#' @references
#' JonB. (2015, October 14). Effect size of a binomial test and its relation to other measures of effect size. Retrieved from https://stats.stackexchange.com/q/176856
#' 
#' NCSS. (n.d.). Tests for two proportions using effect size. In PASS Sample Size Software (pp. 199-1-199–10). Retrieved from https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/PASS/Tests_for_Two_Proportions_using_Effect_Size.pdf
#' 
#' @author 
#' P. Stikker
#' Please visit: https://PeterStatistics.com
#' YouTube channel: https://www.youtube.com/stikpet
#' @export
bi_es_AR <- function(data, field){
  myVar<-na.omit(data[field])
  freq<-table(myVar)
  
  #check if binary
  if(length(freq)!=2){
    return('field is not binary')
  }
  
  
  # We determine the number of successes (k):
  k <- freq[1]
  
  # And the number of trials, i.e. the sample size:
  n <- sum(freq)  
  
  prop <- freq / n
  
  p0 <- 0.5
  
  #Calculate the Alternative Ratio
  AR1 <- prop[1]/p0
  
  #This indicates that the female proportion was about 48% (1 - 0.52) lower than expected
  
  AR2 <- prop[2]/p0
  
  return(c(AR1, AR2))
  
}
