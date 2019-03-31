#' Graphs for investigating sample size in full factorial design
#'
#' Three graphs in full factorial design are for investigating the mutual relationship between the sample size, power "1-beta" and the detectable standardized effect size "delta"
#'
#' @usage plots.Full(factor,factor.lev,order,delta_type,delta,deltao,alpha,beta,type)
#' @param factor the number of factor
#' @param factor.lev factor levels
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
#' #Delta vs Power plot in case of two-level full factorial design with 2 factors
#' plots.Full(factor=2, factor.lev=c(2,2),order=1,
#' delta_type=1, delta=c(1,0,1), alpha=0.05, beta=0.2, type=1)
#' #Sample ize vs Power plot in case of two-level full factorial design with 2 factors
#' plots.Full(factor=2, factor.lev=c(2,2),order=2,
#' delta_type=1, delta=c(1,1,1), deltao=1.5, alpha=0.05, beta=0.2, type=3)
#'
#' @export

plots.Full<-
  function(factor, factor.lev,order=1, delta_type=1, delta , deltao=1 , alpha=0.05, beta=0.2, type)
  {

    FF<-Size.Full(factor, factor.lev,order, delta_type, delta, alpha, beta)
    (n.choose <- FF$n);
    (Delta.choose <- data.frame(t(FF$Delta)))
    nn<- ncol(Delta.choose)
    power <- round(seq(0,1,length.out=101),3)

    delta.pwr <-matrix(0,100,nn)
    Delta <-matrix(0,100,nn)
    pwr <-matrix(0,100,nn)

  if(order==2)
  {
    v_flag<-c(rep(0,factor), rep(1,factor*(factor-1)/2))

  }
  else if (order==1){
    v_flag<-rep(0,factor+factor*(factor-1)/2)
  }


  for (n in 2:101) {

    v1=n-1
    if (order==1){
      v <- factor.lev-1
      c <- prod(factor.lev)*n/factor.lev
    } else if (order==2) {

      v <- (factor.lev-1)%*%t(factor.lev-1)
      v <- c(factor.lev-1,v[upper.tri(v, diag=FALSE)])
      c <- prod(factor.lev)*n/c(factor.lev, (factor.lev%*%t(factor.lev))[upper.tri((factor.lev)%*%t(factor.lev), diag=FALSE)])
    }
    v.denom <- prod(factor.lev)*n-1-sum(v)

    if (type==1 & n==n.choose) {
      for (j in 1: length(v) ){
        for (ind in 1: 100){
          if(alpha+1-power[ind]<0.9999)
            (delta.pwr[ind, j] <- fsize(alpha, 1-power[ind], v[j], v.denom, c[j], delta_type,v_flag[j]))
          else (delta.pwr[ind,j]<-NA)
        }
      }
    }
    else if (type ==2) {
      for (j in 1: length(v) ){
        Delta[n-1,j] <- fsize(alpha, beta, v[j], v.denom, c[j],delta_type,v_flag[j])
      }
    }
    else if (type ==3){
      for (j in 1: length(v) ){
        pwr[n-1,j ]<-round((1-stats::pf(stats::qf((1-alpha),v[j],v.denom),v[j],v.denom,ncp=ifelse( delta_type==1,(deltao^2*(c[j]*v[j])),
                                                                                   ifelse( v_flag[j]==1,
                                                                                           (deltao^2*c[j] ),(deltao^2*c[j]/2) )
        ))),3)
    }
    }
  }

  list_tmp<-sizelist(factor,order)
  full_list<-list_tmp$full_list

  for(i in 1: length(v))
  {
    tmp1<-rep(full_list[[i]],100)
    if ( i==1)
      factor_type<-tmp1
    else if (i>1)
      factor_type<-t(cbind(t(factor_type),t(tmp1)))
  }

  if (type==1)
  {
    for( i in 1: length(v))
    {
      if(i==1)
      {
      Delta1<-delta.pwr[,i]
      power1 <-power[1:100]
      }

      else
      {
        Delta1<-t(cbind(t(Delta1),t(delta.pwr[,i])))
        power1 <-t(cbind(t(power1),t(power[1:100])))
      }
    }

    data<-data.frame(factor_type, Delta1, power1)
    l<-c(unique(factor_type))
    data$factor_type<- factor(data$factor_type, levels = l)

    gr<-ggplot2::ggplot(data ,ggplot2::aes(x=Delta1, y=power1, group=factor_type , shape=factor_type,color=factor_type)) +
      ggplot2::geom_line(size=1.5)   +
      ggplot2::labs(title = "Delta vs Power") +
      ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90, size=12))+
      ggplot2::xlab("Delta") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
      ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
      ggplot2::geom_vline(xintercept = c(1.0,1.5),linetype = "dashed")

  }
  else if(type==2)
  {
    n<-c(2:101)
    for( i in 1: length(v))
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
    n<-c(2:101)
    for( i in 1: length(v))
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
      ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90, size=12))+
      ggplot2::xlab("Sample size") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
      ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
      ggplot2::geom_vline(xintercept =n.choose,linetype = "dashed")
  }

gr


  }
