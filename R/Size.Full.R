#' Sample size calculator for full factorial design
#'
#' Sample size in full factorial design is computed in order to detect a certain standardized effect size "delta" with power "1-beta" at the significance level "alpha".
#'
#' @usage Size.Full(factor,factor.lev,order,delta_type,delta,alpha,beta)
#' @param factor the number of factor
#' @param factor.lev factor levels
#' @param order building the model with main or including the interaction effects ;
#'              1 : only main effects(default) , 2 : both main and two-way interaction effects
#' @param delta_type type of standardized effect size ; 1 : standard deviation type(default), 2 : range of effect type
#' @param delta lists of effects size; The first and the second column is effect size of main and two-way interaction effects, respectively. The third column is standard devitaion of noise.
#' @param alpha Type I error ; 0.05 (default)
#' @param beta  Type II error ; 0.20 (default)
#' @return model, optimal sample size and detectable standardized effect sizes
#' @references
#'
#' Lenth,R.V., 2006-9. Java Applets for Power and Sample Size[Computer software]. Retrieved March 27, 2018 from http://www.stat.uiowa.edu/~rlenth/Power
#'
#' Lim, Yong Bin, 1998. Study on the Size of Minimal Standardized Detectable Difference in Balanced Design of Experiments, \emph{Journal of the Korean society for Quality Management}, 26(4),239-249.
#'
#' Marvin, A., Kastenbaum, A. and Hoel, D.G., 1970. Sample size requirements : one-way analysis of variance, \emph{Biometrika} 57(2),421-430.
#' @examples
#' #only main effects
#' A<-Size.Full(factor=2, factor.lev=c(2,2),order=1,
#' delta_type=1, delta=c(1,0,1), alpha=0.05, beta=0.2)
#' A$model
#' A$n
#' A$Delta
#'
#' #including two-way interaction effects
#' B<-Size.Full(factor=2, factor.lev=c(2,2),order=2,
#' delta_type=1, delta=c(1,1,1), alpha=0.05, beta=0.2)
#'
#' @export

Size.Full <- function(factor, factor.lev,order=1, delta_type=1, delta, alpha=0.05, beta=0.2)
{
  main_n<-0
  two_n<-0
  nn<-0
  Delta <- NULL
  list_tmp<-sizelist(factor,order)
  full_list<-list_tmp$full_list
  list1<-list_tmp$list1

  if( order==2)
  {
    v_flag<-c(rep(0,factor), rep(1,factor*(factor-1)/2))


  }
  else if (order==1){
    v_flag<-rep(0,factor+factor*(factor-1)/2)
  }

  for (n in 2:100){
    v1=n-1
    if (order==1){
      v <- factor.lev-1
      c <- prod(factor.lev)*n/factor.lev
      v.denom <-  prod(factor.lev)*n-1-sum(v)

      for (i in 1: length(v)){
        Delta[i] <- fsize(alpha, beta, v[i], v.denom, c[i],delta_type,0)
      }

      if (max(Delta)<=delta[1]/delta[3] ) (nn<-n)
    }

    else if (order==2) {
      v <- (factor.lev-1)%*%t(factor.lev-1)
      v <- c(factor.lev-1,v[upper.tri(v, diag=FALSE)])
      c <- prod(factor.lev)*n/c(factor.lev, (factor.lev%*%t(factor.lev))[upper.tri((factor.lev)%*%t(factor.lev), diag=FALSE)])
      v.denom <- prod(factor.lev)*n-1-sum(v)

      if(main_n==0)
      {
        for (i in 1: factor){
          Delta[i] <- fsize(alpha, beta, v[i], v.denom, c[i],delta_type,0)
        }
        if (max(Delta[1:factor])<=delta[1]/delta[3] ) (main_n<-n)
      }

      if(two_n==0)
      {
        for (i in (factor+1): length(v)){
          Delta[i] <- fsize(alpha, beta, v[i], v.denom, c[i],delta_type,1)
        }
        if (max(Delta[(factor+1):length(v)])<=delta[2]/delta[3] ) (two_n<-n)
      }
      if(main_n >0 & two_n>0) (nn<-max(main_n,two_n))
    }

    if(nn>0)
    {
      for(i in 1:length(v)){
        Delta[i]<-fsize(alpha, beta, v[i], v.denom, c[i],delta_type,v_flag[i])
      }
      break
    }
  }
  return(list(model=list1,n=nn, Delta=Delta))
}
