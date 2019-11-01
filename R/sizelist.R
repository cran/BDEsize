#' Buliding the model
#'
#' Model is built on the number of factor and order.
#'
#' @usage sizelist(factor,order)
#' @param factor the number of factor
#' @param order building the model with main or including the interaction effects ;
#'              1 : only main effects(default) , 2 : both main and two-way interaction effects
#' @return terms and expansion of model
#' @examples
#' #2 factors; both main and two-way interaction effects
#' A<-sizelist(2,2)
#' A$full_list
#' A$list1
#'
#' @export


sizelist<-function(factor,order)
{
main_list<-list()
for(i in 1:factor)
{
  main_list[[i]]<-toupper(letters)[i]
}

if(order==1)
{
  full_list<-main_list
}
else if(order==2)
{
  k<-1
  two_list<-list()
  for(i in 1:(length(main_list)-1)) {
    for(j in 1:(length(main_list)-i)){
      two_list[[k]]<-two_list[[k]]<-gsub(" ","",paste(main_list[[i]],"*",main_list[[i+j]]))
      k<-k+1
    }
  }
  full_list<-c(main_list,two_list)
}


x<- length(full_list)
list1<-full_list[[1]]
if (x>1)
{
  for (i in 1:(x - 1))
    list1 <- paste(list1, full_list[[i +1]], sep = "+")
}

return(list(full_list=full_list,list1=list1) )
}
