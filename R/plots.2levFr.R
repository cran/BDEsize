#' Graphs for investigating sample size in 2 level fractional factorial design
#'
#' Three graphs in 2 level fractional factorial design are for investigating the mutual relationship between the sample size, power "1-beta" and the detectable standardized effect size "delta"
#'
#' @usage plots.2levFr(factor,fraction,order,delta_type,delta,deltao,alpha,beta,type)
#' @param factor the number of factor
#' @param fraction the number of generators p ex) \eqn{2^(k-p)}
#' @param order building the model with main or including the interaction effects ;
#'              1 : only main effects(default) , 2 : both main and two-way interaction effects
#' @param delta_type type of standardized effect size ; 1 : standard deviation type(default), 2 : range of effect type
#' @param delta lists of effects size; The first and the second column is effect size of main and two-way interaction effects, respectively. The third column is standard devitaion of noise.
#' @param deltao deltao is the detectable standardized effect size for the sample size vs power plot (in case of type=3) ; 1 (default)
#' @param alpha Type I error ; 0.05 (default)
#' @param beta  Type II error ; 0.20 (default)
#' @param type three graphs ; If type=1, Delta vs Power plot. If type=2, Sample size vs Delta plot. If type=3, Sample ize vs Power plot
#' @return one of three graphs ;  Delta vs Power plot , Sample size vs Delta plot, and Sample ize vs Power plot
#' @examples
#' #Delta vs Power plot
#' plots.2levFr(factor=3, fraction=1,order=1,
#' delta_type=1, delta=c(1,0,1), alpha=0.05, beta=0.2, type=1)
#' #Sample ize vs Power plot including two-way interaction effects
#' plots.2levFr(factor=5, fraction=1,order=2,
#' delta_type=1, delta=c(1,1,1), deltao=1, alpha=0.05, beta=0.2, type=3)
#'
#' @export


plots.2levFr<-
  function(factor, fraction, order=1, delta_type, delta ,deltao=1 , alpha=0.05, beta=0.2,type=1)
  {

    FF<-Size.2levFr(factor,fraction, order,delta_type,delta , alpha,beta)
    (n.choose <- FF$n);
    (Delta.choose <- data.frame(t(FF$Delta)))
    nn<- ncol(Delta.choose)
    power <- round(seq(0,1,length.out=1001),3)

    delta.pwr <-matrix(0,1000,nn)
    Delta <-matrix(0,1000,nn)
    pwr <-matrix(0,1000,nn)



  for (n in 2:1001) {

    v1=n-1
    if (order==1){
      v <-  (rep(2,factor)-1)
      c <- (prod(rep(2,factor))/rep(2,fraction) )*n/rep(2,factor)
      v.denom<- 2^(factor-fraction)*n-1-factor
      }

    else if (order==2) {
      v <-(rep(2,factor)-1)%*%t(rep(2,factor)-1)
      v <- c(rep(2,factor)-1,v[upper.tri(v, diag=FALSE)])
      c <- (prod(rep(2,factor))/rep(2,fraction) )*n/c(rep(2,factor), (rep(2,factor)%*%t(rep(2,factor)))[upper.tri((rep(2,factor))%*%t(rep(2,factor)), diag=FALSE)])
      v.denom<- 2^(factor-fraction)*n-1-factor-factor*(factor-1)/2
    }

    if (type==1 & n==n.choose) {
      for(i in 1: order)
      {

        for (ind in 1: 1000){
          if (alpha + 1 - power[ind] < 0.9999 & 1 - power[ind] > 0.0001 & 1 - power[ind] <0.9999 )
            (delta.pwr[ind, i] <- fsize(alpha, 1-power[ind], v[ ifelse(i==1,1,1+factor)], v.denom, c[ifelse(i==1,1,1+factor)], delta_type,ifelse(i==1,0,1)))

          else   (delta.pwr[ind,i]<-NA)
          }

        }
    }

    else if (type ==2) {
      for (i in 1: order){
        Delta[n-1,i] <- fsize(alpha, beta,v[ ifelse(i==1,1,1+factor)], v.denom, c[ifelse(i==1,1,1+factor)],delta_type,ifelse(i==1,0,1))
      }
    }

    else if (type ==3){
      for (i in 1: order){
        pwr[n-1,i]<-round((1-stats::pf(stats::qf((1-alpha),v[ ifelse(i==1,1,1+factor)],v.denom),v[ ifelse(i==1,1,1+factor)],v.denom,ncp=ifelse( delta_type==1,(deltao^2*(c[ ifelse(i==1,1,1+factor)]*v[ ifelse(i==1,1,1+factor)])),
                                                                                   ifelse(i==1,
                                                                                           (deltao^2*c[1]/2 ),(deltao^2*c[  1+factor ]) )
        ))),3)

    }
    }
  }

      factor_type<-rep("Main",1000)
    if(order==2)
      factor_type<-t(cbind(t(factor_type),t(rep("Two-way Interactions",1000))))



  if (type==1)
  {
    for( i in 1: order)
    {
      if(i==1)
      {
      Delta1<-delta.pwr[,i]
      power1 <-power[1:1000]
      }

      else
      {
        Delta1<-t(cbind(t(Delta1),t(delta.pwr[,i])))
        power1 <-t(cbind(t(power1),t(power[1:1000])))
      }
    }

    data<-data.frame(factor_type, Delta1, power1)
    l<-c(unique(factor_type))
    data$factor_type<- factor(data$factor_type, levels = l)

    gr<-ggplot2::ggplot(data ,ggplot2::aes(x=Delta1, y=power1, group=factor_type , shape=factor_type,color=factor_type)) +
      ggplot2::geom_line(size=1.5)   +
      ggplot2::labs(title = "Delta vs Power") +
      ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90,   size=12))+
      ggplot2::xlab("Delta") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
      ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
      ggplot2::geom_vline(xintercept = c(1.0,1.5),linetype = "dashed")

  }
  else if(type==2)
  {
    n<-c(2:1001)
    for( i in 1: order)
    {
      if(i==1)
      {
        Delta1<-Delta[,i]
      }

      else
      {
        Delta1<-t(cbind(t(Delta1),t(Delta[,i])))
      }
    }

    data<-data.frame(n, factor_type, Delta1 )
    l<-c(unique(factor_type))
    data$factor_type<- factor(data$factor_type, levels = l)

    gr<- ggplot2::ggplot(data[n<=2*n.choose,],ggplot2::aes(x=n, y=Delta1, group=factor_type , shape=factor_type,color=factor_type)) +
      ggplot2::geom_line(size=1.5)   +
      ggplot2::labs(title = "Sample size vs Delta") +
      ggplot2::ylab("Delta") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90,  size=12))+
      ggplot2::xlab("Sample size") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12)) +
      ggplot2::geom_hline(yintercept = c(1.0,1.5),linetype = "dashed") +
      ggplot2::geom_vline(xintercept =n.choose,linetype = "dashed")

  }
  else if(type==3)
  {
    n<-c(2:1001)
    for( i in 1: order)
    {
      if(i==1)
      {
        power1 <-pwr[,i]
      }

      else
      {
        power1 <-t(cbind(t(power1),t(pwr[,i])))
      }
    }
    data<-data.frame(n, factor_type,  power1)
    l<-c(unique(factor_type))
    data$factor_type<- factor(data$factor_type, levels = l)

    gr <- ggplot2::ggplot(data[n<=2*n.choose,] ,ggplot2::aes(x=n, y=power1, group=factor_type , shape=factor_type,color=factor_type)) +
      ggplot2::geom_line(size=1.5)   +
      ggplot2::labs(title = "Sample size vs Power") +
      ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90,  size=12))+
      ggplot2::xlab("Sample size") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
      ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
      ggplot2::geom_vline(xintercept =n.choose,linetype = "dashed")
  }

gr


  }
