#' Graphs for investigating sample size in split-plot design
#'
#' Three graphs in split-plot design are for investigating the mutual relationship between the sample size, power "1-beta" and the detectable standardized effect size "delta"
#'
#' @usage plots.Split(whole.factor,whole.factor.lev,
#' split.factor,split.factor.lev,order,delta_type,
#' delta,deltao,alpha,beta,type)
#' @param whole.factor the number of whole factor
#' @param whole.factor.lev whole factor levels
#' @param split.factor the number of split factor
#' @param split.factor.lev split factor levels
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
#' plots.Split(whole.factor=2, whole.factor.lev=c(2,2),
#' split.factor=2, split.factor.lev=c(2,2), order=1,
#' delta_type=1, delta=c(1,0,1,1), alpha=0.05, beta=0.2, type=1)
#' #Sample ize vs Power plot including two-way interaction effects
#' plots.Split(whole.factor=2, whole.factor.lev=c(2,2),
#' split.factor=2, split.factor.lev=c(2,2), order=2,
#' delta_type=1, delta=c(1,1,1,1),deltao=1, alpha=0.05, beta=0.2, type=3)
#'
#' @export
plots.Split<-
  function(whole.factor, whole.factor.lev, split.factor, split.factor.lev,order=1,  delta_type=1, delta ,deltao=1, alpha , beta, type )
  {

    FF<-Size.Split(whole.factor, whole.factor.lev, split.factor, split.factor.lev, order,delta_type,  delta , alpha , beta )
    (n.choose <- FF$n);
    (Delta.choose <- data.frame(t(FF$Delta)))
    nn<- ncol(Delta.choose)
    power <- round(seq(0,1,length.out=101),3)

    delta.pwr <-matrix(0,100,nn)
    Delta <-matrix(0,100,nn)
    pwr <-matrix(0,100,nn)
    wfl<-whole.factor.lev
    sfl<-split.factor.lev

    if( order==2)
    {
      wv_flag<-c(rep(0,whole.factor), rep(1,whole.factor*(whole.factor-1)/2))
      sv_flag<-c(rep(0,split.factor), rep(1,split.factor*(split.factor-1)/2), rep(1,whole.factor*split.factor))
    }
    else if (order==1){
      wv_flag<-rep(0,whole.factor )
      sv_flag<-rep(0,split.factor)
    }
    vv_flag<-c(wv_flag,sv_flag)

    v.whole <- NULL ; v.split <- NULL ; v.split.temp <- NULL
    c.whole <- NULL ; c.split <- NULL
    for (n in 2:101) {
      v1=n-1
      v.rep <- n-1
      if (order==1){
        #whole
        v.whole <-  wfl-1
        v.whole.denom <- prod( wfl)*n-1-sum( wfl-1)-v.rep
        c.whole <- prod( wfl)*prod( sfl)*n/ wfl
        #split
        v.split <-  sfl-1
        v.split.denom <- prod( wfl)*prod( sfl)*n-1-(prod( wfl)*n-1)-sum( sfl-1)
        c.split <- prod( wfl)*prod( sfl)*n/ sfl

      } else if (order==2) {
        #whole
        v.whole <- ( wfl-1)%*%t( wfl-1)
        v.whole <- c( wfl-1,v.whole[upper.tri(v.whole, diag=FALSE)])
        v.whole.denom <- prod( wfl)*n-1-sum(v.whole)-v.rep
        c.whole <- prod( wfl)*prod( sfl)*n/c( wfl, ( wfl%*%t( wfl))[upper.tri(( wfl)%*%t( wfl), diag=FALSE)])

        #split
        v.split <- ( sfl-1)%*%t( sfl-1)
        v.split.temp <- (wfl-1)%*%t( sfl-1)
        v.split <- c( sfl-1, v.split[upper.tri(v.split, diag=FALSE)], as.vector(t(v.split.temp)))
        v.split.denom <- prod( wfl)*prod(sfl)*n-prod( wfl)*n-sum(v.split)
        c.split <- prod( wfl)*prod( sfl)*n/c( sfl, ( sfl%*%t( sfl))[upper.tri(( sfl)%*%t(sfl), diag=FALSE)], as.vector(t( wfl%*%t( sfl))))
      }

      if (type==1 & n==n.choose) {
        for (i in 1: length(v.whole)){
          for (ind in 1: 100){
            if(alpha+1-power[ind]<0.9999)
              (delta.pwr[ind,  i] <- fsize(alpha, 1-power[ind],v.whole[i], v.whole.denom, c.whole[i], delta_type ,wv_flag[i])*sqrt(prod( sfl)+1))
            else ( delta.pwr[ind, i]<-NA)
          }
        }
        for (i in 1: length(v.split)){
          for (ind in 1: 100){
            if(alpha+1-power[ind]<0.9999)
              (delta.pwr[ind, (length(v.whole)+i)] <- fsize(alpha, 1-power[ind], v.split[i], v.split.denom, c.split[i], delta_type ,sv_flag[i]))
            else ( delta.pwr[ind, (length(v.whole)+i)] <-NA)
          }
        }

      }
      else if (type ==2) {
        for (i in 1: length(v.whole)){
          Delta[n-1,i] <- fsize(alpha,beta, v.whole[i], v.whole.denom, c.whole[i], delta_type ,wv_flag[i] )*sqrt(prod( sfl)+1)
        }

        for (i in 1: length(v.split)){
          Delta[n-1,(length(v.whole)+i)] <- fsize(alpha, beta, v.split[i], v.split.denom, c.split[i], delta_type ,sv_flag[i])
        }

      }
      else if (type ==3){
        for (i in 1: length(v.whole)){
          pwr[n-1,i ]<- (1-stats::pf(stats::qf((1-alpha),v.whole[i], v.whole.denom),v.whole[i],v.whole.denom,ncp=ifelse( delta_type ==1,( deltao^2*(c.whole[i]*v.whole[i]))/(prod(sfl)+1),
                                                                                                                 ifelse(wv_flag[i] ==1, ( deltao^2*c.whole[i]),( deltao^2*c.whole[i]/2))/(prod(sfl)+1))))#(Delta^2)*(c*nu1)
        }

        for (i in 1: length(v.split)){
          pwr[n-1,(i+length(v.whole)) ]<- (1-stats::pf(stats::qf((1-alpha),v.split[i], v.split.denom),v.split[i],v.split.denom,ncp=ifelse( delta_type ==1,( deltao^2*(c.split[i]*v.split[i])) ,
                                                                                                                                   ifelse(sv_flag[i] ==1,( deltao^2*c.split[i]) ,  ( deltao^2*c.split[i]/2) ))))#(Delta^2)*(c*nu1)
        }
      }
    }

    list_tmp<-sizelist.split(whole.factor,split.factor,order)
    full_list<-list_tmp$full_list

    for(i in 1: (length(v.whole)+length(v.split)))
    {
      tmp1<-rep(full_list[[i]],100)
      if ( i==1)
        factor_type<-tmp1
      else if (i>1)
        factor_type<-t(cbind(t(factor_type),t(tmp1)))
    }

    if (type==1)
    {
      for( i in 1: (length(v.whole)+length(v.split)))
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
        ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90,  size=12))+
        ggplot2::xlab("Delta") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
        ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
        ggplot2::geom_vline(xintercept = c(1.0,1.5),linetype = "dashed")

    }
    else if(type==2)
    {
      n<-c(2:101)
      for( i in 1: (length(v.whole)+length(v.split)))
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
      for( i in 1: (length(v.whole)+length(v.split)))
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
