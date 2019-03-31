#' Sample size calculator for split-plot design
#'
#' Sample size in split-plot design is computed in order to detect a certain standardized effect size "delta" with power "1-beta" at the significance level "alpha".
#'
#' @usage Size.Split(whole.factor, whole.factor.lev,split.factor,
#' split.factor.lev, order, delta_type, delta, alpha, beta)
#' @param whole.factor the number of whole factor
#' @param whole.factor.lev whole factor levels
#' @param split.factor the number of split factor
#' @param split.factor.lev split factor levels
#' @param order building the model with main or including the interaction effects ;
#'              1 : only main effects(default) , 2 : both main and two-way interaction effects
#' @param delta_type type of standardized effect size ; 1 : standard deviation type(default), 2 : range of effect type
#' @param delta lists of effects size; The first and the second column is effect size of main and two-way interaction effects, respectively. The third and the forth column is standard devitaion of whole noise and noise, respectively.
#' @param alpha Type I error ; 0.05 (default)
#' @param beta  Type II error ; 0.20 (default)
#' @details The linear model for the split-plot design is
#' \deqn{y_{ijklm} = \mu + \tau_i +\beta_j +\gamma_k + (\beta\tau)_{ik} +\theta_{ijk} + \delta_l +\lambda_m +(\delta\lambda)_{im} +(\beta\delta)_{jl}+(\beta\lambda)_{jm}+(\gamma\delta)_{kl}+(\delta\lambda)_{lm}+\epsilon_{ijklm}}
#' where \eqn{\tau_i} represents the replicate effect, \eqn{\beta_j ,\gamma_k} represents the whole plot main effects, \eqn{\theta_{ijk}} is the whole plot error, \eqn{\delta_l ,\lambda_m} represent the subplot main effects, and \eqn{\epsilon_{ijklm}} is the subplot error.
#' @return model, optimal sample size and detectable standardized effect sizes
#' @references
#'
#' Lenth,R.V., 2006-9. Java Applets for Power and Sample Size[Computer software]. Retrieved March 27, 2018 from http://www.stat.uiowa.edu/~rlenth/Power
#'
#' Lim, Yong Bin, 1998. Study on the Size of Minimal Standardized Detectable Difference in Balanced Design of Experiments, \emph{Journal of the Korean society for Quality Management}, 26(4),239-249.
#'
#' Marvin, A., Kastenbaum, A. and Hoel, D.G., 1970. Sample size requirements : one-way analysis of variance, \emph{Biometrika} 57(2),421-430.
#'
#' Montgomery, Douglas C., 2013. Design and analysis of experiments. John wiley & sons. ISBN: 978-1-118-14692-7
#'
#' @examples
#' #only main effects
#' A<-Size.Split(whole.factor=2, whole.factor.lev=c(2,2),
#' split.factor=2, split.factor.lev=c(2,2), order=1,
#' delta_type=1, delta=c(1,0,1,1), alpha=0.05, beta=0.2)
#' A$model
#' A$n
#' A$Delta
#'
#' #including two-way interaction effects
#' B<-Size.Split(whole.factor=2, whole.factor.lev=c(2,2),
#' split.factor=2, split.factor.lev=c(2,2), order=2,
#' delta_type=1, delta=c(1,1,1,1), alpha=0.05, beta=0.2)
#'
#' @export
#'

Size.Split<- function(whole.factor, whole.factor.lev, split.factor, split.factor.lev,order=1,  delta_type=1, delta, alpha=0.05, beta=0.2){
  main_n<-0
  two_n<-0
  nn<-0
  whole.Delta <- NULL
  splot.Delta<-NULL
  list_tmp<-sizelist.split(whole.factor, split.factor,order)
  list1<-list_tmp$list1
  if (whole.factor>2 || split.factor>2)
  {
    stop("The number of whole plot factors and split plot factors are up to 2.")
  }
  if( order==2)
  {
    wv_flag<-c(rep(0,whole.factor), rep(1,whole.factor*(whole.factor-1)/2))
    sv_flag<-c(rep(0,split.factor), rep(1,split.factor*(split.factor-1)/2), rep(1,whole.factor*split.factor))

  }
  else if (order==1){
    wv_flag<-rep(0,whole.factor )
    sv_flag<-rep(0,split.factor)
  }


  for (n in 2:100){
    if (order==1){
      v.whole <- NULL ; v.split <- NULL ; v.split.temp <- NULL
      c.whole <- NULL ; c.split <- NULL
      whole.Delta <- NULL; split.Delta <- NULL
      v.rep <- n-1
      #whole
      v.whole <- whole.factor.lev-1
      v.whole.denom <- prod(whole.factor.lev)*n-1-sum(whole.factor.lev-1)-v.rep
      c.whole <- prod(whole.factor.lev)*prod(split.factor.lev)*n/whole.factor.lev

      for (i in 1: length(v.whole)){
        whole.Delta[i] <- fsize(alpha, beta, v.whole[i], v.whole.denom, c.whole[i],delta_type,0)*sqrt(prod(split.factor.lev)+1)
      }
      #split
      v.split <- split.factor.lev-1
      v.split.denom <- prod(whole.factor.lev)*prod(split.factor.lev)*n-1-(prod(whole.factor.lev)*n-1)-sum(split.factor.lev-1)
      c.split <- prod(whole.factor.lev)*prod(split.factor.lev)*n/split.factor.lev

      for (i in 1: length(v.split)){
        split.Delta[i] <- fsize(alpha, beta, v.split[i], v.split.denom, c.split[i],delta_type,0)
      }
      if ( max(whole.Delta)<=delta[1]/delta[3] & max(split.Delta)<=delta[1]/delta[4]  )
        (nn<-n)
    }

    else if (order==2) {
      v.whole <- NULL ; v.split <- NULL ; v.split.temp <- NULL
      c.whole <- NULL ; c.split <- NULL
      whole.Delta <- NULL; split.Delta <- NULL
      v.rep <- n-1

      #whole
      v.whole <- (whole.factor.lev-1)%*%t(whole.factor.lev-1)
      v.whole <- c(whole.factor.lev-1,v.whole[upper.tri(v.whole, diag=FALSE)])
      v.whole.denom <- prod(whole.factor.lev)*n-1-sum(v.whole)-v.rep
      #v.whole.denom<-v.rep
      c.whole <- prod(whole.factor.lev)*prod(split.factor.lev)*n/c(whole.factor.lev, (whole.factor.lev%*%t(whole.factor.lev))[upper.tri((whole.factor.lev)%*%t(whole.factor.lev), diag=FALSE)])
      #split
      v.split <- (split.factor.lev-1)%*%t(split.factor.lev-1)
      v.split.temp <- (whole.factor.lev-1)%*%t(split.factor.lev-1)
      v.split <- c(split.factor.lev-1, v.split[upper.tri(v.split, diag=FALSE)], as.vector(t(v.split.temp)))
      v.split.denom <- prod(whole.factor.lev)*prod(split.factor.lev)*n-prod(whole.factor.lev)*n-sum(v.split)
      c.split <- prod(whole.factor.lev)*prod(split.factor.lev)*n/c(split.factor.lev, (split.factor.lev%*%t(split.factor.lev))[upper.tri((split.factor.lev)%*%t(split.factor.lev), diag=FALSE)], as.vector(t(whole.factor.lev%*%t(split.factor.lev))))

      if(main_n==0)
      {
        for (i in 1:whole.factor){
          whole.Delta[i] <-  fsize(alpha, beta, v.whole[i], v.whole.denom, c.whole[i],delta_type,0)*sqrt(prod(split.factor.lev)+1)
        }
        for (i in 1:split.factor){
          split.Delta[i] <-fsize(alpha, beta, v.split[i], v.split.denom, c.split[i],delta_type,0)
        }
        if (max(whole.Delta[1:whole.factor])<=delta[1]/delta[3] & max(split.Delta[1:split.factor])<=delta[1]/delta[4] ) (main_n<-n)
      }

      if(two_n==0)
      {
        for(i in (split.factor+1) : length(v.split)){
          split.Delta[i] <- fsize(alpha, beta, v.split[i], v.split.denom, c.split[i],delta_type,1)
        }
        if(whole.factor>1){
          for (i in (whole.factor+1): length(v.whole)){
            whole.Delta[i] <- fsize(alpha, beta, v.whole[i], v.whole.denom, c.whole[i],delta_type,1)*sqrt(prod(split.factor.lev)+1)
          }
          if (max(whole.Delta[(whole.factor+1): length(v.whole)])<=delta[2]/delta[3] & max(split.Delta[(split.factor+1) : length(v.split)])<=delta[2]/delta[4] ) (two_n<-n)
        }
        else
          ( if (  max(split.Delta[(split.factor+1) : length(v.split)])<=delta[2]/delta[4] ) (two_n<-n))
      }
      if(main_n >0 & two_n>0) (nn<-max(main_n,two_n))
    }

    if(nn>0)
    {
      for(i in 1:length(v.whole)){

        whole.Delta[i]<- fsize(alpha, beta, v.whole[i], v.whole.denom, c.whole[i],delta_type,wv_flag[i])*sqrt(prod(split.factor.lev)+1)
      }

      for(i in 1:length(v.split))
      {
        split.Delta[i] <-  fsize(alpha, beta, v.split[i], v.split.denom, c.split[i],delta_type,sv_flag[i])
      }
      Delta <- c(whole.Delta, split.Delta)
      break
    }
  }
  return(list(model=list1,n=nn, Delta=Delta))
}
