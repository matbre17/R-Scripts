require(ggplot2)

#Mandels Test
############################################################################################
MandelLoF <- function(x, y, weights = NULL, m1.deg = 1, m2.deg = 2, data){ 
  x <- data[[x]] #Redefine x to denote the x values of the data frame, df
  y <- data[[y]] #same for y...
  
  n <- length(x) #Find sample size (which equals the length of any arbitrary column of df)
  
  if (is.null(weights) == TRUE){w <- rep(c(1), n)}#Provides equal weight to all points, i.e. normal OLS, if no weights are provided
  else {w <- df[[weights]]} #Assigns weights to w, if weights are given
  
  #Defines regression models
  lm.m1 <- summary(lm(y ~ poly(x, degree = m1.deg, raw = TRUE), weights = w))
  lm.m2 <- summary(lm(y ~ poly(x, degree = m2.deg, raw = TRUE), weights = w))
  
  m1.df <- n - (m1.deg + 1) #degree of polynomial plus 1a denotes number of parameters
  m2.df <- n - (m2.deg + 1)
  
  m1.yx.v <- (n-1)*var(lm.m1$residuals)/m1.df #Extracts residual variance
  m2.yx.v <- (n-1)*var(lm.m2$residuals)/m2.df
  
  F.statistic <- ((m1.df*m1.yx.v)-(m2.df*m2.yx.v))/m2.yx.v
  
  p.val <- pf(F.statistic, df1 = m1.df, df2 = m2.df, lower.tail = FALSE) #Converts F-quantile into a p-value
  
  dat <- NULL #Assign data frame to return models
  dat <- data.frame(rbind(dat, c(lm.m1$coefficients[,1],rep(0, m2.deg-m1.deg)),c(lm.m2$coefficients[,1])))
  colnames(dat) <- paste('b', seq(0, m2.deg), sep = '')
  rownames(dat) <- c('m1','m2')
  models <- data.frame(c(lm.m1$coefficients[,1]))
  
  #Lines below prints out relevant information
  
  cat('\nF =', F.statistic, 'with num df =', m1.df, 'and denom df =', m2.df, 'degrees of freedom\n')
  cat('p-value =', p.val,'\n\n')
  
  invisible(list(F.stat = F.statistic, p.value = p.val, models = dat))
}
################################################################################

#Defines weights which sum to 1
###############################################################################
VarWeight <- function(y, level, data){ #Returns weights of compounds, sorted in accordance with the levels
  y <- data[[y]] #Redefine y to denote the x values of the data frame, df
  lvl <- data[[level]] #.'..and levels
  
  n <- length(y) #Find sample size (which equals the length of any arbitrary column of df)
  
  weights <- c()
  
  for (l in unique(lvl)){
    replicates <- sum(lvl == l) #Number of replicates at the particular level
    
    sr <- 1/var(y[lvl == l]) #Extract data individually for each level
    
    weights <- c(weights, rep(sr, replicates))
  }
  
  return(weights/(sum(weights)/n))
}
################################################################################
 
#Youden Plot
################################################################################
Youden <- function(estimates, replicates, day, TrueValue = NULL, data){
  est <- data[[estimates]]
  
  x.rep <- unique(data[[replicates]])[1]
  y.rep <- unique(data[[replicates]])[2]
  
  day <- as.factor(data[[day]][data[[replicates]] == x.rep])
  
  
  
  
  x <- est[data[[replicates]] == x.rep]
  y <- est[data[[replicates]] == y.rep]
  
  x.max <- max(x)
  y.max <- max(y)
  
  df <- data.frame(x,y,day, TrueValue)
  
  return(ggplot(data = df, aes(x = x, y = y))+geom_point(aes(color = day))+
    geom_vline(xintercept = TrueValue)+geom_hline(yintercept = TrueValue)+
    geom_abline(slope = 1, intercept = 0)+xlab('Parallel 1')+ylab('Parallel 2')+
      xlim(c(-x.max,x.max))+ylim(c(-y.max,y.max)))
  
}
################################################################################

#ANOVA-based Precision estimates
###############################################################################
PrecisionANOVA <- function(Days, Response, I, J, data){ #Defining a function which enables the calculation of accuracy components
  
  x.dat <- data[[Days]] #Define the days
  y.dat <- data[[Response]] #Define the response
  
  #The repeatability variance is the average within-day variance among the I days
  
  r.ev <- sum(aggregate(x = y.dat, by = list(x.dat), var)$x)/I
  
  #x.ev is the standard deviation of the grand mean
  
  x.ev <- var(aggregate(x = y.dat, by = list(x.dat), mean)$x)
  
  #It can be shown that the between-day variance is the difference between
  #the standard deviation of the means and the standard variance of the repeatability
  #see the thesis
  
  b.ev <- x.ev - r.ev/J 

  r.es <- sqrt(r.ev) #Repeatability estimate
  
  if (b.ev < 0){ #If between-day variance is less than zero, is assumed to be absent
    b.ev <- 0
    b.es <- 0
    
    R.ev <- r.ev
    R.es <- sqrt(R.ev)
    

    nu.es <- I-1 #Due to the means of estimation, the degrees of freedom becomes I-1, rather than IJ-1, see GUM
    
  }
  
  
  else{#If between-day variance is positive
    b.es <- sqrt(b.ev)
    R.ev <- r.ev+b.ev
    R.es <- sqrt(R.ev) #Intermediate precision estimate
    
    
    #Intermediate variance estimate
    nu.es <- unlist(WelchSatter(ui=c(sqrt(r.ev), sqrt(b.ev)), df=c(I*(J-1), I-1))[1])

  }

  
  
  results <- list(rep.ev=c(r.ev), rep.es=c(r.es), btw.ev=c(b.ev),
                        btw.es=c(b.es), IP.ev=c(R.ev), IP.es=c(R.es), 
                        Nu.eff=c(nu.es)) #Provide all results
  
  return(results)
  
}
################################################################################

AccuracyProfile <- function(Days, Response, I, J, TrueMean,gamma, data, customcoverage = FALSE){
  
  aov.dat <- PrecisionANOVA(Days = Days, Response = Response, I = I, J = J, data = data)
  
  mean.response <- mean(data[[Response]])
  
  sr <- aov.dat[2] #Extracts repeatability
  sb <- aov.dat[4] #Extracts between-day variance
  nu <- aov.dat[7] #Effective degrees of freedom
  
  v.ip <- (sr^2)+(sb^2)
  s.ip <- sqrt(v.ip)
  v.xbar <- (sr*sr/(I*J))+(sb*sb/I)
  
  bias <- mean(Response) - unique(data[[TrueMean]])
  
  neff <- v.ip/v.xbar
  
  if (customcoverage == FALSE){Q <- qt(p = (1+gamma)/2, df = nu)}
  if (customcoverage != FALSE){Q <- customcoverage}
  
  uct <- s.ip*(1+(1/neff))^0.5
  
  # print(c(Q, nu))
  
  return(Q*c(-uct,uct)+bias)
  
} 
################################################################################


