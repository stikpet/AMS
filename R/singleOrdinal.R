

#' Single Ordinal - Visualisation - Single Stack Bar chart
#' 
#' This function will return a stacked bar-chart of one field. 
#' It can be defined as “a bar chart showing multiple bars stacked at each x-axis category, 
#' each representing a value of the stacking variable” (Upton & Cook, 2014, p. 88)
#' 
#' @param data A dataframe to be used
#' @param field Specific field in the dataframe as string
#' @param order A vector with the labels in order, or set to 'none'
#' @param height Either 'count' or 'percent'
#' @references 
#' Upton, G. J. G., & Cook, I. (2014). *Dictionary of statistics* (3rd ed.). Oxford University Press.
#' @export
ord_vi_barstack <- function(data, field, order='none', height='count'){
  myField <- data[[field]]
  myLikert<-na.omit(myField)
  
  if(is.numeric(myField)){
    tempData <- factor(myLikert, ordered = TRUE)
  } 
  else{
    tempData <- factor(myLikert, ordered = TRUE, levels=order)
  }
  
  k <- length(levels(tempData))
  
  chartData <- table(tempData)
  
  if(height=='percent'){
    chartData <- prop.table(chartData)*100
  } 
  
  barplot(as.matrix(chartData), 
                   beside = FALSE, 
                   legend.text=rownames(chartData), 
                   col=heat.colors(k),
                   xlab = height,
                   ylab = field,
                   horiz = TRUE)
  
}


#' Single Ordinal - Central Tendency - Median
#' 
#' This function will return the median, defined as “the middle value in a distribution, 
#' below and above which lie values with equal total frequencies or probabilities” (Porkess, 1991, p. 134). 
#' 
#' @param data A dataframe to be used
#' @param field Specific field in the dataframe as string
#' @param order A vector with the labels in order, or set to 'none'
#' @references 
#' Porkess, R. (1991). *The HarperCollins dictionary of statistics*. HarperPerennial.
#' @export
ord_ce_median <- function(data, field, order='none'){
  
  myField <- data[[field]]
  
  if(is.numeric(myField)){
    medNum <- median(myField, na.rm=TRUE)
    med <- medNum
    
  } else{
    tempData <- factor(myField, ordered = TRUE, levels=order)
    medNum <- median(as.numeric(tempData), na.rm=TRUE)
    
    # if data was not numeric
    # check if medNum is integer
    if(medNum%%1 == 0){
      med <- levels(tempData)[medNum]
      
    } else{
      med1 <- levels(tempData)[medNum-0.5]
      med2 <- levels(tempData)[medNum+0.5]
      med <- paste("between", med1, "and", med2, sep=" ")
    }
    
  }
  return(med)
}


#' Single Ordinal - Dispersion - Consensus
#' . 
#' Determines a measure of dispersion called consensus (Tastle & Wierman, 2007; Tastle, Wierman, & Rex Dumdum, 2005). 
#' This measure ranges from 0 to 1. A zero would indicate a complete lack of consensus, 
#' the number of people that tend towards one end of the ordinal variable (i.e. fully disagree) is then the same as 
#' the number of people who tend to the other end (i.e. fully agree), 
#' while consensus of one would indicate all respondents gave the same answer.
#' @param data A dataframe to be used
#' @param field Specific field in the dataframe as string
#' @param order A vector with the labels in order, or set to 'none'
#' @return Cns The consensus measure
#' @details 
#' The formula for the Consensus is:
#' \deqn{Cns = 1 + \sum_{i=1}^{k}p_i \log_2\left(1-\frac{\left|X_i-\mu_X\right|}{d_X}\left)}
#' With:
#' \deqn{\mu_X = \frac{\sum_{i=1}^k X_i\times F_i}{n}}
#' \deqn{d_X = \text{max}\left(X_i\right)-\text{min}\left(X_i\right)}
#' Where \eqn{X_i} is the rank of category i
#' @references 
#' Tastle, W. J., & Wierman, M. J. (2007). Consensus and dissention: A measure of ordinal dispersion. 
#' *International Journal of Approximate Reasoning, 45*(3), 531–545. doi:10.1016/j.ijar.2006.06.024
#' 
#' Tastle, W. J., Wierman, M. J., & Rex Dumdum, U. (2005). Ranking ordinal scales using the consensus measure. 
#' *Issues in Information Systems, 6*(2), 96–102.
#' @export
ord_di_consensus <- function(data, field, order='none'){
  myField <- data[[field]]
  if(is.numeric(myField)){
    tempData <- factor(myField, ordered = TRUE)
  } 
  else{
    tempData <- factor(myField, ordered = TRUE, levels=order)
  }
  
  X <- order(levels(tempData),decreasing=TRUE)
  muX<-mean(as.numeric(tempData), na.rm=TRUE)
  myMin<-min(as.numeric(tempData), na.rm=TRUE)
  myMax<-max(as.numeric(tempData), na.rm=TRUE)
  dx <- myMax - myMin
  freq<-table(tempData)
  prop<-prop.table(freq)
  Cns<-1+sum(prop*log(1 - abs(X-muX)/dx,2))
  
  return(Cns)
}


#' Single Ordinal - Test - Sign
#' . 
#' Performs a one-sample Sign test
#' 
#' @param data A dataframe to be used
#' @param field Specific field in the dataframe as string
#' @param order A vector with the labels in order, or set to 'none'
#' @return pVal The p-value of the test
#' @details 
#' This uses a binomial test which for large samples might take some time.
#' @export
ord_ts_sign <- function(data, field, order='none', hypMed='none'){
  myField <- data[[field]]
  myLikert<-na.omit(myField)
  
  if(is.numeric(myField)){
    tempData <- factor(myLikert, ordered = TRUE)
  } 
  else{
    tempData <- factor(myLikert, ordered = TRUE, levels=order)
  }
  
  k <- length(levels(tempData))

  #Convert to numbers
  tempData<-as.numeric(tempData)
  
  if(hypMed=='none'){
    hypMed <- (k + 1)/2
  }
  
  group1<-tempData[tempData<hypMed]
  group2<-tempData[tempData>hypMed]
  n1<-length(group1)
  n2<-length(group2)
  
  #Select the lowest of the two
  myMin<-min(n1,n2)
  
  #Determine total number of cases (unequal to hyp. median)
  n<-n1+n2
  
  #Determine the significance using binomial test
  pVal <- 2*pbinom(myMin, n,0.5)
  
  return(pVal)
  
}


#' Single Ordinal - Test - Wilcoxon
#' . 
#' Performs a one-sample Wilcoxon signed rank test
#' 
#' @param data A dataframe to be used
#' @param field Specific field in the dataframe as string
#' @param order A vector with the labels in order, or set to 'none'
#' @param hypMed The median according to the null hypotheses. If set to none the median of the categories will be used
#' @param exact Boolean to use the exact Wilcoxon distribution or not, only applicable if there are no ties
#' @param correct Boolean to indicate the use of a Yates continuity correction
#' @return results A dataframe with:
#' @return zVal The z-value statistic
#' @return pVal The two-sided p-value
#' @references 
#' Wilcoxon, F. (1945). Individual comparisons by ranking methods. *Biometrics Bulletin, 1*(6), 80. doi:10.2307/3001968
#' @export
ord_ts_wilcox <- function(data, field, order='none', hypMed='none', exact=NULL, correct=FALSE){
  myField <- data[[field]]
  myLikert<-na.omit(myField)
  
  if(is.numeric(myField)){
    tempData <- factor(myLikert, ordered = TRUE)
  } 
  else{
    tempData <- factor(myLikert, ordered = TRUE, levels=order)
  }
  
  k <- length(levels(tempData))
  
  #Convert to numbers
  tempData<-as.numeric(tempData)
  
  if(hypMed=='none'){
    hypMed <- (k + 1)/2
  }
  
  wtest <- wilcox.test(tempData, mu=hypMed, exact=exact, correct=correct)
  pVal <- wtest$p.value
  zVal <- qnorm(pVal/2)
  
  
  results <- data.frame(zVal=abs(zVal), pVal=unname(pVal))
  
  return(results)
}


#' Single Ordinal - Effect Size - Rosenthal Correlation
#' . 
#' Determines the Rosenthal Correlation
#' 
#' @param data A dataframe to be used
#' @param field Specific field in the dataframe as string
#' @param order A vector with the labels in order, or set to 'none'
#' @param hypMed The median according to the null hypotheses. If set to none the median of the categories will be used
#' @param correct Boolean to indicate the use of a Yates continuity correction
#' @return results A dataframe with:
#' @return es The correlation coefficient
#' @return qual A qualification of the effect size
#' @details 
#' This effect size is suggested by various authors (Fritz et al., 2012, p. 12; Mangiafico, 2016; Simone, 2017; Tomczak, M., & Tomczak, E., 2014, p. 23; ).
#' The formula can be found in Rosenthal book (1991, p. 19) , so I will refer to as the Rosenthal correlation coefficient 
#' (as to differentiate it with other correlation coefficients). Probably the original was Cohen (1988, p. 275) who calls it 'f', but all other authors label it 'r'
#' 
#' Since it requires a z-value the same settings as for ord_ts_wilcox function are needed.
#' 
#' The formula is fairly straightforward:
#' \deqn{r = \frac{z}{\sqrt{n}}}
#' 
#' The rule of thumb for the qualification is taken from Bartz (1999, p. 184):
#' | r           | qualification |
#' |-------------|--------------|
#' | 0 < .20     | very low |
#' | 0.20 < 0.40 | low |
#' | 0.40 < 0.60 | moderate |
#' | 0.60 < 0.80 | strong |
#' | 0.80 or more | very strong |
#' 
#' @references 
#' Bartz, A. E. (1999). *Basic statistical concepts* (4th ed). Merrill.
#' 
#' Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed). L. Erlbaum Associates.
#' 
#' Fritz, C. O., Morris, P. E., & Richler, J. J. (2012). Effect size estimates: Current use, calculations, and interpretation. 
#' *Journal of Experimental Psychology: General, 141*(1), 2–18. doi:10.1037/a0024338
#' 
#' Mangiafico, S. S. (2016). *Summary and analysis of extension program evaluation in R (1.15.0)*. Rutger Cooperative Extension.
#' 
#' Tomczak, M., & Tomczak, E. (2014). The need to report effect size estimates revisited. An overview of some recommended measures of effect size. 
#' *Trends in Sport Sciences, 1*(21), 19–25.
#' 
#' @export
ord_es_rosenthal <- function(data, field, order='none', hypMed='none', correct=FALSE){
  testRes <- ord_ts_wilcox(data, field, order=order, hypMed=hypMed, correct=correct)
  
  myField <- data[[field]]
  myLikert<-na.omit(myField)
  
  n<-length(myLikert)
  r<-testRes$zVal/sqrt(n)
  
  # qualification
  if(abs(r) < 0.2){
    qual <- 'very low'
  } else if(abs(r) < .4){
    qual <- 'low'
  } else if(abs(r) < .6){
    qual <- 'moderate'
  } else if(abs(r) < .8){
    qual <- 'strong'
  } else {
    qual <- 'very strong'
  }
  
  results <- data.frame(es = r, qual = qual)
  return(results)
}

