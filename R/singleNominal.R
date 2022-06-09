

#' Single Nominal - Visualisation - Bar Chart
#'
#' Simple function to create a bar-chart.
#'
#' @param data Dataframe to be used
#' @param field Specific field name (as string) to draw chart from
#' @param height Determines if the height of the bars are counts ('count', default) or percentages ('percent')
#' @param missing Boolean to indicate to include the missing values (default is FALSE)
#' @return bar chart
#' @details 
#' A bar-chart is defined as “a graph in which bars of varying height with spaces between them are used 
#' to display data for variables defined by qualities or categories” (Zedeck, 2014, p. 20)
#' @references 
#' Zedeck, S. (Ed.). (2014). *APA dictionary of statistics and research methods*. American Psychological Association.
#' @export
nom_vi_bar <- function(data, field, height="count", missing=FALSE){
  myVar <- data[field]

  if(missing){
    freq <- table(myVar, exclude=NULL)
  } else{
    freq <- table(myVar)
  }

  if(height=="percent"){
    props <- prop.table(freq)
    visual <- barplot(props*100, xlab=field, ylab=height)
  } else{
    visual <- barplot(freq, xlab=field, ylab=height)
  }

  return(visual)
}



#' Single Nominal - Visualisation - Dot plot
#'
#' Simple function to create a dot plot.
#'
#' @param data Dataframe to be used
#' @param field Specific field name (as string) to draw chart from
#' @param missing Boolean to indicate to include the missing values (default is FALSE)
#' @param dotScale One dot represents how many data points, default is 1
#' @param options A vector with settings for pch (dot style), col (color), cex (size) and offset (space between each dot)
#' @return dot plot
#' @details 
#' According to the Oxford Dictionary of Statistics a dot plot can be defined as 
#' "an alternative to a bar chart or line graph when there are very few data values. 
#' Each value is recorded as a dot, so that the frequencies for each value can easily be counted" (Upton & Cook, 2014, p. 129).
#' 
#' You can let one dot represent multiple datapoints by setting the dotScale parameter. Also the size, color and offset can be set.
#' @references 
#' Zedeck, S. (Ed.). (2014). *APA dictionary of statistics and research methods*. American Psychological Association.
#' @export
nom_vi_dot <- function(data, field, missing=FALSE, dotScale=1, options=c(19, 4, 2, 0.8)){
  # dotScale is used to set 1 dot to represent x data points (with normal rounding)
  # options a vector with the pch (dot style), col (color), cex (size) and offset (space between each dot)

  myVar <- data[field]
  if(missing){
    freq <- table(myVar, exclude=NULL)
    k <- length(names(freq))
    names(freq)[k] <- "missing"
  } else{
    freq <- table(myVar)
  }

  labels <- names(freq)
  k <- length(labels)
  numCat <- seq(1, k)
  dataNum <- rep(numCat, times=round(freq/dotScale, 0))
  stripchart(dataNum, method = "stack", at = 0, pch=options[1], col=options[2], cex=options[3], offset=options[4], axes=FALSE)
  axis(1, at=1:k, labels = labels)

}


#' Single Nominal - Visualisation - Cleveland Dot plot
#'
#' Simple function to create a Cleveland dot plot.
#'
#' @param data Dataframe to be used
#' @param field Specific field name (as string) to draw chart from
#' @param missing Boolean to indicate to include the missing values (default is FALSE)
#' @param dotScale One dot represents how many data points, default is 1
#' @param options A vector with settings for pch (dot style), col (color), and pt.cex (size)
#' @return a Cleveland dot plot
#' @details 
#' A Cleveland dot plot (Cleveland & McGill, 1987) is a bar chart where instead of bars a dot is placed at 
#' the center of the top of the bar (and then the bars removed). It is a dot plot only showing the top dot.
#' @references 
#' Cleveland, W. S., & McGill, R. (1984). Graphical perception: Theory, experimentation, and application to the development of graphical methods. 
#' *Journal of the American Statistical Association, 79*(387), 531–554. doi:10.2307/2288400 
#' @export
nom_vi_cleveland <- function(data, field, missing=FALSE, dotScale=1, options=c(19, 4, 2)){
  # dotScale is used to set 1 dot to represent x data points (with normal rounding)
  # options a vector with the pch (dot style), col (color), and pt.cex (size)

  myVar <- data[field]
  if(missing){
    freq <- table(myVar, exclude=NULL)
    k <- length(names(freq))
    names(freq)[k] <- "missing"
  } else{
    freq <- table(myVar)
  }

  k <- length(freq)
  dotchart(round(freq/dotScale,0), pch=options[1], color = options[2], pt.cex = options[3], xlab="count")
}


#' Single Nominal - Visualisation - Pie Chart
#'
#' Simple function to create a Pie Chart.
#'
#' @param data Dataframe to be used
#' @param field Specific field name (as string) to draw chart from
#' @param missing Boolean to indicate to include the missing values (default is FALSE)
#' @return a pie chart
#' @details 
#' Most definitions of a pie-chart describe the shape. For example one definition is given as 
#' “a graphic display in which a circle is cut into wedges with the area of each wedge being proportional to 
#' the percentage of cases in the category represented by that wedge” (Zedeck, 2014, p. 260). 
#' @references 
#' Zedeck, S. (Ed.). (2014). *APA dictionary of statistics and research methods*. American Psychological Association.
#' @export
nom_vi_pie <- function(data, field, missing=FALSE){
  myVar <- data[field]
  if(missing){
    freq <- table(myVar, exclude=NULL)
    k <- length(names(freq))
    names(freq)[k] <- "missing"
  } else{
    freq <- table(myVar)
  }

  myLabels <- names(freq)
  pct <- prop.table(freq)*100
  myLabels2 <- paste(round(pct), "%", sep = "")
  myLabels3 <- paste(myLabels, myLabels2)
  pie(freq, labels = myLabels3)

}

#' Single Nominal - Visualisation - Pareto chart
#' 
#' Simple function to draw a Pareto chart
#' 
#' @param data Dataframe to be used
#' @param field Specific field name (as string) to draw chart from
#' @return a Pareto chart
#' @details
#' Unfortunately, there is no general agreed upon definition of a Pareto diagram. The most general description I’ve found was 
#' by Kemp and Kemp (2004) who mention it is a name for a bar chart if the order of the bars have no meaning (i.e. for a nominal variable), 
#' and they only mention that often the bars are then placed in decreasing order. 
#' 
#' According to some authors a Pareto diagram is any diagram with the bars in order of size (Joiner, 1995; WhatIs.com, n.d.), 
#' while others suggest that a line representing the cumulative relative frequencies should also be included (Weisstein, 2002). 
#' 
#' Upton and Cook (2014) also add that the bars should not have any gaps, but many other authors ignore this.
#' 
#' I will use the following definition: a bar chart where the bars are placed in descending order of frequency. 
#' Usually an ogive is added in the chart as well.
#' 
#' An ogive (oh-jive) is: "the graphs of cumulative frequencies" (Kenney, 1939).
#' @references 
#' Joiner. (1995). *Pareto charts: Plain & simple*. Joiner Associates.
#' 
#' Kemp, S. M., & Kemp, S. (2004). *Business statistics demystified*. McGraw-Hill.
#' 
#' Kenney, J. F. (1939). *Mathematics of statistics; Part one*. Chapman & Hall.
#' 
#' Upton, G. J. G., & Cook, I. (2014). *Dictionary of statistics* (3rd ed.). Oxford University Press.
#' 
#' Weisstein, E. W. (n.d.). Pareto Plot. Retrieved May 25, 2014, from http://mathworld.wolfram.com/ParetoPlot.html
#' 
#' WhatIs.com. (n.d.). What is Pareto chart (Pareto distribution diagram)? - Definition from WhatIs.com. 
#' Retrieved April 20, 2014, from http://whatis.techtarget.com/definition/Pareto-chart-Pareto-distribution-diagram
#' @export
nom_vi_pareto <- function(data, field){
  myVar <- data[field]
  freq <- table(myVar)
  k <- length(names(freq))

  freqSort <- sort(freq, decreasing = TRUE)
  freqSort

  cumFr <- cumsum(freqSort)
  cumPerc <- cumFr /sum(freq)*100

  op <- par(mar= c(5.1,4.1,4.1,4.1))
  barplot(freqSort)
  par(new=TRUE)
  plot(cumPerc, type = 'b', xlim=c(0.5,5.5), ylim=c(0,100), col = "red", axes = FALSE, xlab = field, ylab = "count")
  mtext("cumulative percent", side = 4, line = 3)
  axis(side = 4)
  par(op)
}


#' Single Nominal - Central Tendancy - Mode
#' 
#' Function to determine the mode for a nominal variable
#' 
#' @param data Dataframe to be used
#' @param field Specific field name (as string) from data frame
#' @return values The category or categories that are the mode
#' @details 
#' This function will return all modes if the data is multimodal, or no mode if all categories have equal count
#' @export
nom_ce_mode <- function(data, field){
  myVar <- data[field]
  freq <- table(myVar)

  modes <- names(freq)[freq == max(freq)]
  #so add an if in case the number of modes is equal to the number of labels
  if (length(names(freq))==length(modes)){
    modes <- "no mode"
  }

  return(modes)
}




#' Single Nominal - Dispersion - Variation Ratio
#' 
#' Simple function to determine the variation ratio of a nominal variable
#' 
#' @param data Dataframe to be used
#' @param field Specific field name (as string) from data frame
#' @return values The Variation Ratio
#' @details 
#' The Variation Ratio (VR) (Freeman, 1965) is simply the proportion that does not belong to the modal category (Zedeck, 2014, p.406). 
#' @references 
#' Freeman, L. C. (1965). *Elementary applied statistics: for students in behavioral science*. Wiley.
#' 
#' Zedeck, S. (Ed.). (2014). *APA dictionary of statistics and research methods*. American Psychological Association.
#' @export
nom_di_vr <- function(data, field){
  myVar <- data[field]
  freq <- table(myVar)

  maxFreq <- max(freq)
  maxCount <- sum(freq==maxFreq)

  #Determine the Variation Ratio
  VR<-1-maxCount*maxFreq/sum(freq)

  return(VR)

}




#' Single Nominal - Test - Pearson chi-square goodness-of-fit
#' 
#' This function will perform a Pearson chi-square goodness-of-fit (GoF) test on a nominal variable.
#' 
#' @param data Dataframe to be used
#' @param field Specific field name (as string) from data frame
#' @param corr Specific correction to be used, 'none' (default), 'yates', 'pearson', or 'williams'
#' @return results A dataframe with the following values
#' @return chi2Val The chi-square test statistic
#' @return pVal The p-value of the test (significance)
#' @return df The degrees of freedom
#' @return n The sample size
#' @return nExpBelow5 The number of cells with an expected count less than 5
#' @return pExpBelow5 The proportion of cells with an expected count less than 5
#' @return minExp The minimum expected count
#' @details 
#' This function will assume that the expected counts are simply the sample size 
#' divided by the total number of categories (i.e. uniform distributed)
#' 
#' The Pearson chi-square test formula is:
#' \deqn{ \chi^{2}=\sum_{i=1}^{k}\frac{\left(F_{i}-E_{i}\right)^{2}}{E_{i}}}
#' #' With E. Pearson correction:
#' \deqn{\chi_{E Pearson}^2 = \frac{n - 1}{n}\times \chi^2}
#' With Williams correction:
#' \deqn{\chi_{Williams}^2 = \frac{\chi^2}{q}}
#' Where:
#' \deqn{q = 1 + \frac{k+1}{6\times n}}
#' In the formulas \eqn{F_i} are the observed frequencies, \eqn{E_i} the expected frequencies,
#' \eqn{k} the number of categories, and \eqn{n} the total sample size.
#' 
#' Note that a Yates correction is only applicable with df = 1, i.e. a binary variable. So it
#' is not incorporated here.
#' @export
nom_ts_pearson <- function(data, field, corr='none'){
  myVar <- data[field]
  freq <- table(myVar)

  k <- length(freq)
  n <- sum(freq)

  prop<-rep(1/k,k)

  test <- chisq.test(freq, p=prop,correct=FALSE)

  chiVal <- unname(test$statistic)
  pVal <- unname(test$p.value)
  df <- unname(test$parameter)

  if(corr=='williams'){
    q = 1 + (k+1)/(6*n)
    chiVal <- chiVal/q
    pVal <- pchisq(chiVal, df, lower.tail = FALSE)
  } else if(corr=='pearson'){
    chiVal <- (n-1)/n * chiVal
    pVal <- pchisq(chiVal, df, lower.tail = FALSE)
  }

  expected <-unname(test$expect)
  nbelow<-length(which(expected<5))
  nbelowProp <- nbelow/k
  expMin <- min(expected)
  
  results <- data.frame(chi2Val = chiVal, pVal = pVal, df = df, n = n, nExpBelow5 = nbelow, pExpBelow5 = nbelowProp, minExp = expMin)

  return(results)
}





#' Single Nominal - Test - G goodness-of-fit
#' 
#' This function will perform a G goodness-of-fit (GoF) test on a nominal variable. Also known as a likelihood ratio test
#' 
#' @param data Dataframe to be used
#' @param field Specific field name (as string) from data frame
#' @param corr Specific correction to be used, 'none' (default), 'yates', 'pearson', or 'williams'
#' @return results A dataframe with the following values
#' @return chi2Val The chi-square test statistic
#' @return pVal The p-value of the test (significance)
#' @return df The degrees of freedom
#' @return n The sample size
#' @return nExpBelow5 The number of cells with an expected count less than 5
#' @return pExpBelow5 The proportion of cells with an expected count less than 5
#' @return minExp The minimum expected count
#' @details 
#' This function will assume that the expected counts are simply the sample size 
#' divided by the total number of categories (i.e. uniform distributed)
#' 
#' The G test formula is:
#' \deqn{G=2\times\sum_{i=1}^{k}\left(F_{i}\times ln\left(\frac{O_{i}}{E_{i}}\right)\right)}
#' #' With E. Pearson correction:
#' \deqn{\chi_{E Pearson}^2 = \frac{n - 1}{n}\times \chi^2}
#' With Williams correction:
#' \deqn{\chi_{Williams}^2 = \frac{\chi^2}{q}}
#' Where:
#' \deqn{q = 1 + \frac{k+1}{6\times n}}
#' In the formulas \eqn{O_i} are the observed frequencies, \eqn{E_i} the expected frequencies,
#' \eqn{k} the number of categories, and \eqn{n} the total sample size.
#' 
#' Note that a Yates correction is only applicable with df = 1, i.e. a binary variable. So it
#' is not incorporated here.
#' @export
nom_ts_g <- function(data, field, corr='none'){
  myVar<-na.omit(data[field])
  freq<-table(myVar)

  # We determine the number of categories (k):
  k <- length(freq)

  # And the number of trials, i.e. the sample size:
  n <- sum(freq)

  expFreq <- sum(freq) / length(freq)

  if (corr=='yates'){
    G = 0
    for(i in 1:k){
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
      G <- G / q
    }
  }

  pVal = pchisq(G, 1, lower.tail = FALSE)

  nbelow<-length(which(expFreq<5))
  nbelowProp <- unname(nbelow/k)
  expMin <- min(expFreq)

  df = k - 1
  
  results <- data.frame(chi2Val = G, pVal = pVal, df = df, n = n, nExpBelow5 = nbelow, pExpBelow5 = nbelowProp, minExp = expMin)

  return(results)
}



#' Single Nominal - Test - Exact Multinomial
#' 
#' This function will perform an exact multinomial test
#' 
#' @param data Dataframe to be used
#' @param field Specific field name (as string) from data frame
#' @return pVal The p-value of the test
#' @return chi2Val The chi-square test statistic
#' @return pVal The p-value of the test (significance)
#'  
#' @details 
#' To calculate the exact multinomial test the following steps can be used.
#' 
#' Step 1: Determine the probability of the observed counts using the probability mass function of the multinomial distribution.
#' 
#' The formula for this is given by:
#' \deqn{\frac{n!}{\prod_{i=1}^{k}F_i}\times\prod_{i=1}^{k}\pi_i^{F_i}}
#' Where \eqn{n} is the total sample size, \eqn{k} the number of categories, \eqn{F_i} the frequency of the i-th category, 
#' and \eqn{\pi_i} the expected proportion of the i-th category.
#' 
#' Step 2: Determine all possible permutations with repetition that create a sum equal to the sample size over the k-categories.
#' 
#' Step 3: Determine the probability of each of these permutations using the probability mass function of the multinomial distribution.
#' 
#' Step 4: Sum all probabilities found in step 3 that are equal or less than the one found in step 1.
#' 
#' 
#' Step 2 is quite tricky. We could create all possible permutations with replacement. 
#' If our sample size is n and the number of categories is k, this gives \eqn{(n+1)^k} permutations. 
#' The ‘+ 1’ comes from the option of 0 to be included. Most of these permutations will not sum to the sample size, so they can be removed.
#' 
#' If the expected probability for each category is the same, we could use another approach. 
#' We could then create all possible combinations with replacement. This would give fewer results:
#' \deqn{\binom{n+k}{k}=\frac{(n+k)!}{n!k!}}
#' Again we can then remove the ones that don’t sum to the sample size. Then perform step 3, 
#' but now multiply each by how many variations this can be arranged in. If for example we have 5 categories, 
#' and a total sample size of 20, one possible combination is [2, 2, 3, 3, 10]. 
#' This would be the same as  [2, 3, 3, 2, 10], [2, 3, 10, 2, 3], etc. 
#' We could determine the count (frequency) of each unique score, so in the example 2 has a frequency of 2, 3 also and 10 only one. 
#' Now the first 2 we can arrange in:
#' \deqn{\binom{5}{2}=\frac{5!}{(5-2)!2!}=10}
#' The 5 is our number of categories, the 2 the frequency. For the two 3’s we now have 5 – 2 = 3 spots left, so those can only be arranged in:
#' \deqn{\binom{3}{2}=\frac{3!}{(3-2)!2!}=3}
#' Combining these 3 with the 10 we had earlier gives 3×10=30 possibilities. The single 10 only can now go to one spot so that’s it.
#' 
#' In general, if we have k categories, m different values and F_i is the i-th frequency of those values, sorted from high to low, we get:
#' \deqn{\binom{k}{F_1}\prod_{i=2}^m\binom{k-\sum_{j=1}^{m-i+1}F_j}{F_j}=\binom{k}{F_1}\binom{k-F_1}{F_2}\binom{k-\sum{j=1}^{2}F_j}{F_3}…\binom{k-\sum_{j=1}^{m-1}F_j}{F_k}}
#' Where:
#' \deqn{\binom{a}{b}=\frac{a!}{(a-b)!b!}}
#' @export
nom_ts_multi <- function(data, field){
  myVar<-na.omit(data[field])
  freq<-table(myVar)

  n <- sum(freq)
  k <- length(freq)
  expProb =rep(1/k, k)
  pObs = dmultinom(sort(freq, decreasing=TRUE), size=n, expProb)
  counts <- seq(0, n, by = 1)

  kCounts <- matrix(nrow=n+1, ncol=k)
  for (i in 1:k){
    kCounts[,i] <- counts
  }

  all_perm <- merge(kCounts[,1], as.data.frame(kCounts[,2]),all=TRUE)
  all_perm <- all_perm[rowSums(all_perm) <= n,]
  for (i in 3:k){
    all_perm <- merge(all_perm, as.data.frame(kCounts[,i]),all=TRUE)
    all_perm <- all_perm[rowSums(all_perm) <= n,]
  }

  all_perm <- all_perm[rowSums(all_perm) == n,]
  pObsAll <- apply(all_perm, 1, function(x) dmultinom(sort(x, decreasing=TRUE), size=n, expProb))
  dfPs <- data.frame(pObsAll)
  pval <- sum(dfPs[which(round(dfPs$pObsAll, digits=8) <= round(pObs, 8)),1])
  return(pval)
}



#' Single Nominal - Post-Hoc - Binary Settings
#' 
#' This is a helper function to decide on which post-hoc test to use.
#' 
#' @param n Sample size used in test
#' @param nThres An integer specifying a threshold up to which to use the exact binomial test
#' @param prefTest Indicator for the preferred approximation, either 'g' (default) or 'pearson'
#' @param corr Indicator for which correction to apply either 'yates' (default), or 'pearson' for Pearson or 'williams' for Williams
#' @param es Indicator for effect size measure to use in post-hoc test, either 'cohonH2' (default), 'coheng', or 'ar'
#' @return results a data frame with:
#' @return test The name of the test to use
#' @return corr The correction to use in the test
#' @return es The effect size measure to use
#' @export
bin_decider <-function(n, nThres=1000, prefTest='g', corr='yates', es='cohenH2'){
  if(n < nThres){
    test = 'binomial'
    correction = 'none'
  } else{
    test = prefTest
    correction = corr
  }
  
  results <- data.frame(test=test, corr=correction, es=es)

  return(results)
}



#' Single Nominal - Post-Hoc
#' 
#' This function will perform a post-hoc analysis on a nominal variable
#' 
#' @param data Dataframe to be used
#' @param field Specific field name (as string) from data frame
#' @param binDecider A vector with the settings for the binDecider, default is c(1000, 'g', 'yates', 'cohenH2')
#' @return results A dataframe with all the information
#' @export
nom_ph_test <- function(data, field, binDecider = c(1000, 'g', 'yates', 'cohenH2')){
  myField = field
  myVar<-na.omit(data[myField])
  freq<-table(myVar)
  myFreq <- as.data.frame(freq)
  comb <-combn(t(myFreq[myField]), 2)
  comb <- t(comb)
  comb2 <- data.frame(t(combn(myFreq$Freq,2)))
  comb2$PairN <- comb2$X1+comb2$X2
  comb2$test <- NA
  comb2$correction <- NA
  comb2$sig <- NA
  comb2$adjSig <- NA
  comb2$esUsed <- NA
  comb2$es <- NA
  comb2$esQual <- NA
  npairs <- nrow(comb)
  npairs

  for (i in 1:npairs){
    # create the dataframe with only the two selected categories in pair
    ff <- data[data[myField] == toString(comb[i,1]) | data[myField] == toString(comb[i,2]) , ]

    tstUse <- bin_decider(comb2$PairN[i], nThres=binDecider[1], prefTest=binDecider[2], corr=binDecider[3], es=binDecider[4])

    if(tstUse$test == 'binomial'){
      pVal <- bi_ts_binomial(ff, myField)
      comb2$test[i] <- 'binomial'
      comb2$correction[i] <- 'none'
    }
    else if(tstUse$test =='score'){
      comb2$test[i] <- 'score'
      if (tstUse$corr == 'yates'){
        comb2$correction[i] <- 'yates'

        tst <- bi_ts_score(ff, myField, corr=TRUE)
        pVal <- tst[2]
      }else{
        comb2$correction[i] <- 'none'
        tst <- bi_ts_score(ff, myField, corr=FALSE)
        pVal <- tst[2]
      }
    }
    else if(tstUse$test =='wald'){
      comb2$test[i] <- 'wald'
      if (tstUse$corr == 'yates'){
        comb2$correction[i] <- 'yates'
        tst <- bi_ts_wald(ff, myField, corr=TRUE)
        pVal <- tst[2]
      }else{
        comb2$correction[i] <- 'none'
        tst <- bi_ts_wald(ff, myField, corr=FALSE)
        pVal <- tst[2]
      }
    }
    else if(tstUse$test =='pearson'){
      comb2$test[i] <- 'pearson gof'
      comb2$correction[i] <- tstUse$corr
      tst <- bi_ts_chi2Gof_Pearson(ff, myField, corr=tstUse$corr)
      pVal <- tst[4]
    }
    else if(tstUse$test =='g'){
      comb2$test[i] <- 'g gof'
      comb2$correction[i] <- tstUse$corr
      tst <- bi_ts_chi2Gof_G(ff, myField, corr=tstUse$corr)
      pVal <- tst[2]
    }



    comb2$sig[i] <- pVal

    if (pVal*npairs>1){
      comb2$adjSig[i] <- 1
    } else{
      comb2$adjSig[i] <- pVal*npairs
    }

    # Effect sizes
    if(tstUse$es =='cohenH2'){
      esRes <- bi_es_cohenH2(ff, myField)
      comb2$esUsed[i] <- 'Cohen H2'
      comb2$es[i] <- esRes[2]
      comb2$esQual[i] <- esRes[1]
    }
    else if(tstUse$es =='coheng'){
      esRes <- bi_es_coheng(ff, myField)
      comb2$esUsed[i] <- 'Cohen g'
      comb2$es[i] <- esRes[2]
      comb2$esQual[i] <- esRes[1]
    }
    else if(tstUse$es =='ar'){
      esRes <- bi_es_ar(ff, myField)
      comb2$esUsed[i] <- 'Alternative Ratio'
      comb2$es[i] <- esRes[1]
      comb2$esQual[i] <- 'na'
    }

  }

  results <- cbind(comb, comb2)
  last<-2*npairs
  results<-results[-c(npairs+1:last), ]
  return(results)

}




#' Single Nominal - Effect Size - Cramer's V
#' 
#' This function will determine Cramer's V for a Goodness-of-Fit test
#' 
#' @param data Dataframe to be used
#' @param field Specific field name (as string) from data frame
#' @param test Which test to use, either 'pearson' (default) or 'g'
#' @param corr Correction to use in test, either 'none' (default), 'yates', 'pearson', or 'williams'
#' @param bergsma Boolean to indicate to use the Bergsma correction on Cramer's V, FALSE (default) or TRUE
#' @return results A dataframe with:
#' @return qual A qualification of the effect size
#' @return es The effect size
#' @export
nom_es_cramerv <- function(data, field, test='pearson', corr='none', bergsma=FALSE){
  
  freq<-table(data[field])
  
  #the number of rows
  k<-nrow(freq)
  
  #the total sample size
  n<-sum(freq)

  if(test=='pearson'){
    testRes <- nom_ts_pearson(data, field, corr=corr)
  }
  else if(test=='g'){
    testRes <- nom_ts_g(data, field, corr=corr)
  }

  chiVal <- unname(testRes$chi2Val)
  
  #Cramer's v
  if(bergsma){
    phi2 <- chiVal/n
    phi2m <- max(0, phi2)
    km <- k - (k-1)^2 / (n-1)
    v <- sqrt(phi2m / (km - 1))
  } 
  else {
    v<-sqrt(chiVal/(n*(k-1)))
  }
  
  
  # qualification
  w <- sqrt(k - 1) * v
  
  if(w < 0.1){
    qual <- 'negligible'
  } else if(w < .3){
    qual <- 'small'
  } else if(w < .5){
    qual <- 'medium'
  } else {
    qual <- 'large'
  }
  
  results <- data.frame(es = v, qual = qual)

  
  return(results)

}




#' Single Nominal - Effect Size - Cohen's w
#' 
#' This function will determine Cohen's w for a Goodness-of-Fit test
#' 
#' @param data Dataframe to be used
#' @param field Specific field name (as string) from data frame
#' @param test Which test to use, either 'pearson' (default) or 'g'
#' @param corr Correction to use in test, either 'none' (default), 'yates', 'pearson', or 'williams'
#' @return results A dataframe with:
#' @return qual A qualification of the effect size
#' @return es The effect size
#' @export
nom_es_cohenw <- function(data, field, test='pearson', corr='none'){

  freq<-table(data[field])

  #the total sample size
  n<-sum(freq)

  if(test=='pearson'){
    testRes <- nom_ts_pearson(data, field, corr=corr)
  }
  else if(test=='g'){
    testRes <- nom_ts_g(data, field, corr=corr)
  }

  chiVal <- unname(testRes$chi2Val)

  #Cohen w
  w <- sqrt(chiVal/n)
  
  # qualification
  if(w < 0.1){
    qual <- 'negligible'
  } else if(w < .3){
    qual <- 'small'
  } else if(w < .5){
    qual <- 'medium'
  } else {
    qual <- 'large'
  }
  
  results <- data.frame(es = w, qual = qual)
  return(results)

}




#' Single Nominal - Effect Size - JBM E
#' 
#' This function will determine JBM E
#' 
#' @param data Dataframe to be used
#' @param field Specific field name (as string) from data frame
#' @param test Which test to use, either 'pearson' (default) or 'g'
#' @param corr Correction to use in test, either 'none' (default), 'yates', 'pearson', or 'williams'
#' @return E The effect size
#' @export
nom_es_jbme <- function(data, field, test='pearson', corr='none'){
  freq<-table(data[field])

  #the number of rows
  k<-nrow(freq)

  #the total sample size
  n<-sum(freq)

  if(test=='pearson'){
    testRes <- nom_ts_pearson(data, field, corr=corr)
    minE <- unname(testRes$minExp)
    chiVal <- unname(testRes$chi2Val)
    E <- chiVal * minE / (n * (n - minE))
  }
  else if(test=='g'){
    testRes <- nom_ts_g(data, field, corr=corr)
    expProp <- rep(1/k, k)
    props <- freq / n
    E <- -1 / log(min(expProp)) * sum(props * log(props / expProp))
  }

  return(E)

}

