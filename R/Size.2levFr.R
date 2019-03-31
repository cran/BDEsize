#' Sample size calculator for 2 level fractional factorial design
#'
#' Sample size in 2 level fractional factorial design is computed in order to detect a certain standardized effect size "delta" with power "1-beta" at the significance level "alpha".
#' The model for fractional factorial design contains only main effects in resolution III and IV.
#'
#' @usage Size.2levFr(factor,fraction,order,delta_type,delta,alpha,beta)
#' @param factor the number of factor
#' @param fraction the number of generators p ex) \eqn{2^(k-p)}
#' @param order building the model with main or including the interaction effects ;
#'              1 : only main effects(default) , 2 : both main and two-way interaction effects
#' @param delta_type type of standardized effect size ; 1 : standard deviation type(default), 2 : range of effect type
#' @param delta lists of effects size; The first and the second column is effect size of main and two-way interaction effects, respectively. The third column is standard devitaion of noise.
#' @param alpha Type I error ; 0.05 (default)
#' @param beta  Type II error ; 0.20 (default)
#' @return model, optimal sample size and detectable standardized effect sizes
#'
#'         Detectable standardized effect sizes return only one or two values for main and two-way interaction effects.
#' @references
#'
#' Lenth,R.V., 2006-9. Java Applets for Power and Sample Size[Computer software]. Retrieved March 27, 2018 from http://www.stat.uiowa.edu/~rlenth/Power
#'
#' Lim, Yong Bin, 1998. Study on the Size of Minimal Standardized Detectable Difference in Balanced Design of Experiments, \emph{Journal of the Korean society for Quality Management}, 26(4),239-249.
#'
#' Marvin, A., Kastenbaum, A. and Hoel, D.G., 1970. Sample size requirements : one-way analysis of variance, \emph{Biometrika} 57(2),421-430.
#' @examples
#' #only main effects
#' A<-Size.2levFr(factor=3, fraction=1,order=1,
#' delta_type=1, delta=c(1,0,1), alpha=0.05, beta=0.2)
#' A$model
#' A$n
#' A$Delta
#'
#' #including two-way interaction effects
#' B<-Size.2levFr(factor=5, fraction=1,order=2,
#' delta_type=1, delta=c(1,1,1), alpha=0.05, beta=0.2)
#'
#' @export

Size.2levFr<- function(factor, fraction, order=1, delta_type=1,  delta , alpha=0.05, beta=0.2)
{
  main_n<-0
  two_n<-0
  nn<-0
  Delta <- NULL
  list_tmp<-sizelist(factor,order)
  full_list<-list_tmp$full_list
  list1<-list_tmp$list1

  if (2^(factor-fraction)-1 -(factor+factor*(factor-1)/2)<0 & order==2)
  {
    stop( "Two-way interactions can't be estimated.")
  }

  for (n in 2:100){
    v1=n-1
    if (order==1){
      v <- (rep(2,factor)-1)
      c <- (prod(rep(2,factor))/rep(2,fraction) )*n/rep(2,factor)
      v.denom<- 2^(factor-fraction)*n-1-factor
      Delta[1] <- fsize(alpha, beta, v[1], v.denom, c[1],delta_type,0)
      if (Delta[1]<=delta[1]/delta[3] ) (nn<-n)
    }
    else if (order==2) {
      v <- (rep(2,factor)-1)%*%t(rep(2,factor)-1)
      v <- c(rep(2,factor)-1,v[upper.tri(v, diag=FALSE)])
      c <- (prod(rep(2,factor))/rep(2,fraction) )*n/c(rep(2,factor), (rep(2,factor)%*%t(rep(2,factor)))[upper.tri((rep(2,factor))%*%t(rep(2,factor)), diag=FALSE)])
      v.denom<- 2^(factor-fraction)*n-1-factor-factor*(factor-1)/2
      if(main_n==0)
      {
        Delta[1] <- fsize(alpha, beta, v[1], v.denom, c[1],delta_type,0)
        if (Delta[1]<=delta[1]/delta[3] ) (main_n<-n)
      }
      if(two_n==0)
      {
        Delta[2] <- fsize(alpha, beta, v[factor+1], v.denom, c[factor+1],delta_type,1)
        if ( Delta[2]<=delta[2]/delta[3] ) (two_n<-n)
      }
      if(main_n >0 & two_n>0) (nn<-max(main_n,two_n))
    }

    if(nn>0)
    {
      if(order==1)
      {Delta[1]<-fsize(alpha, beta, v[1], v.denom, c[1],delta_type,0)}
      else if(order==2)
      {
        Delta[1]<-fsize(alpha, beta, v[1], v.denom, c[1],delta_type,0)
        Delta[2]<-fsize(alpha, beta, v[factor+1], v.denom, c[factor+1],delta_type,1)
      }
      break
    }
  }

  return(list(model=list1,n=nn, Delta=Delta))
}
