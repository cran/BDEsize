% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.2levFr.R
\name{plots.2levFr}
\alias{plots.2levFr}
\title{Graphs for investigating sample size in 2 level fractional factorial design}
\usage{
plots.2levFr(factor,fraction,order,delta_type,delta,deltao,alpha,beta,type)
}
\arguments{
\item{factor}{the number of factor}

\item{fraction}{the number of generators p ex) \eqn{2^(k-p)}}

\item{order}{building the model with main or including the interaction effects ;
1 : only main effects(default) , 2 : both main and two-way interaction effects}

\item{delta_type}{type of standardized effect size ; 1 : standard deviation type(default), 2 : range of effect type}

\item{delta}{lists of effects size; The first and the second column is effect size of main and two-way interaction effects, respectively. The third column is standard devitaion of noise.}

\item{deltao}{deltao is the detectable standardized effect size for the sample size vs power plot (in case of type=3) ; 1 (default)}

\item{alpha}{Type I error ; 0.05 (default)}

\item{beta}{Type II error ; 0.20 (default)}

\item{type}{three graphs ; If type=1, Delta vs Power plot. If type=2, Sample size vs Delta plot. If type=3, Sample ize vs Power plot}
}
\value{
one of three graphs ;  Delta vs Power plot , Sample size vs Delta plot, and Sample ize vs Power plot
}
\description{
Three graphs in 2 level fractional factorial design are for investigating the mutual relationship between the sample size, power "1-beta" and the detectable standardized effect size "delta"
}
\examples{
#Delta vs Power plot
plots.2levFr(factor=3, fraction=1,order=1,
delta_type=1, delta=c(1,0,1), alpha=0.05, beta=0.2, type=1)
#Sample ize vs Power plot including two-way interaction effects
plots.2levFr(factor=5, fraction=1,order=2,
delta_type=1, delta=c(1,1,1), deltao=1, alpha=0.05, beta=0.2, type=3)

}