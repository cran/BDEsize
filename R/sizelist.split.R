#' Buliding the model for split-plot design
#'
#' Model is built on the number of factor and order.
#'
#' @usage sizelist.split(whole.factor,split.factor,order)
#' @param whole.factor the number of whole factor
#' @param split.factor the number of split factor
#' @param order building the model with main or including the interaction effects ;
#'              1 : only main effects(default) , 2 : both main and two-way interaction effects
#' @return terms and expansion of model
#' @examples
#' #one whole.factor and one split.factor ; both main and two-way interaction effects
#' A<-sizelist.split(1,1,2)
#' A$full_list
#' A$list1
#'
#' @export

sizelist.split<-
  function(whole.factor, split.factor,order)
{

    w_main_list<-list()

    for(i in 1:whole.factor)
      {
      w_main_list[[i]]<-tolower(letters)[i]
      }

    s_main_list<-list()

    for(i in 1:split.factor)
      {
      s_main_list[[i]]<-toupper(letters)[length(w_main_list)+i]
    }


    if(order==1)
      {
       full_list<-c(w_main_list,s_main_list)
    }
    else if(order==2)
      {
      if(split.factor>1)
        {
        k<-1
        s_two_list<-list()
        for(i in 1:(length(s_main_list)-1))
          {
          for(j in 1:(length(s_main_list)-i)){
            s_two_list[[k]]<-gsub(" ","",paste(s_main_list[[i]],"*",s_main_list[[i+j]]))
            k<-k+1
          }
        }
        }
      if(whole.factor>1)
        {
        k<-1
        w_two_list<-list()

    for(i in 1:(length(w_main_list)-1)) {
      for(j in 1:(length(w_main_list)-i)){
        w_two_list[[k]]<-gsub(" ","",paste(w_main_list[[i]],"*",w_main_list[[i+j]]))
        k<-k+1
      }
    }
  }
  k<-1
  ws_two_list<-list()

  for(i in 1:length(w_main_list)){
    for (j in 1:length(s_main_list)){
      ws_two_list[[k]]<-gsub(" ","",paste(w_main_list[[i]],"*",s_main_list[[j]]))
      k<-k+1
    }
  }
  if(split.factor>1 & whole.factor>1)
  {
    full_list<-c(w_main_list,w_two_list,s_main_list,s_two_list,ws_two_list)
  }
  else if(split.factor>1) {
    full_list<-c(w_main_list,s_main_list,s_two_list,ws_two_list)
  }
  else if(whole.factor>1) {
    full_list<-c(w_main_list,w_two_list,s_main_list, ws_two_list)
  }
  else {
    full_list<-c(w_main_list, s_main_list, ws_two_list)
  }
    }
  x<- length(full_list)
  list1<-full_list[[1]]
  for( i in 1:(x-1))
    list1<-paste(list1,full_list[[i+1]],sep="+")
  return(list(full_list=full_list,list1=list1) )
}
