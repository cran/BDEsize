% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.Split.R
\name{plots.Split}
\alias{plots.Split}
\title{Graphs for investigating sample size in split-plot design}
\usage{
plots.Split(whole.factor,whole.factor.lev,
split.factor,split.factor.lev,order,delta_type,
delta,deltao,alpha,beta,type)
}
\arguments{
\item{whole.factor}{the number of whole factor}

\item{whole.factor.lev}{whole factor levels}

\item{split.factor}{the number of split factor}

\item{split.factor.lev}{split factor levels}

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
Three graphs in split-plot design are for investigating the mutual relationship between the sample size, power "1-beta" and the detectable standardized effect size "delta"
}
\examples{
#Delta vs Power plot
plots.Split(whole.factor=2, whole.factor.lev=c(2,2),
split.factor=2, split.factor.lev=c(2,2), order=1,
delta_type=1, delta=c(1,0,1,1), alpha=0.05, beta=0.2, type=1)
#Sample ize vs Power plot including two-way interaction effects
plots.Split(whole.factor=2, whole.factor.lev=c(2,2),
split.factor=2, split.factor.lev=c(2,2), order=2,
delta_type=1, delta=c(1,1,1,1),deltao=1, alpha=0.05, beta=0.2, type=3)

}