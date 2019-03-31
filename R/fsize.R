#' Detectable minimum effect size
#'
#' Detectable minimum effect size is calculated using the distribution function of noncentral F-distribution with noncentrality parameter.
#'
#' @usage fsize(alpha,beta,nu1,nu2,c,delta_type,flag)
#' @param alpha Type I error
#' @param beta  Type II error
#' @param nu1 numerator degree of freedom for the f-test
#' @param nu2 denominator degree of freedom for the f-test
#' @param c the coefficient of sum of squares
#' @param delta_type type of standardized effect size ; 1 : standard deviation type, 2 : range of effect type
#' @param flag  In case of delta_type=2 ; If flag=1 , two-way interaction effect for range of effect type. If flag=0(default), main effect for range of effect type.
#' @return detectable minimal effect sizes
#' @examples
#' #two-level full factorial design with 2 factors
#' #5 replications, main effect for standardized type
#' fsize(alpha=0.05, beta=0.2, nu1=1, nu2=17,
#' c=10,delta_type=1 )
#'
#' @export
fsize <-  function(alpha, beta, nu1, nu2, c,delta_type,flag=0)
{
  fl <- fpow::ncparamF(alpha, beta, nu1, nu2)/2
  if (delta_type==1) (Delta<-sqrt(2*fl/(c*nu1))) #ncp<-Delta^2*c*nu1
  else if (delta_type==2 & flag==0) (Delta<-sqrt(4*fl/c)) #ncp<-Delta^2*c/2
  else if (delta_type==2 & flag==1) (Delta<-sqrt(2*fl/c)) #ncp<-Delta^2*c/2
  return(Delta)
}
