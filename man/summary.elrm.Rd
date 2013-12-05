\name{summary.elrm}
\alias{summary.elrm}
\title{Summarize an elrm Object}
\description{Summary method for class \code{elrm} that formats and prints out the results of an \code{elrm} object.}
\usage{\method{summary}{elrm}(object, ...)}
\arguments{
  \item{object}{an object of class \code{elrm}, resulting from a call to \command{elrm()} or a previous call to \command{update()}.}
  \item{\dots}{additional arguments to the summary function (currently unused).}
}
\details{The following results are formatted and printed to the screen: the matched call, coefficient estimates and confidence intervals for each model term of interest, estimated p-value for jointly testing that the parameters of interest are simultaneously equal to zero, full conditional p-values from separately testing each parameter equal to zero, length of the Markov chain that inference was based on, and the Monte Carlo standard error of each reported p-value.}
\references{
Zamar David. Monte Carlo Markov Chain Exact Inference for Binomial
Regression Models. Master's thesis, Statistics and Actuarial
Sciences, Simon Fraser University, 2006.

Zamar D, McNeney B and Graham J. elrm: Software Implementing Exact-like Inference for Logistic Regression Models. Journal of Statistical Software 2007, 21(3).
}
\author{David Zamar, Jinko Graham, Brad McNeney}
\seealso{\code{\link{update.elrm}}, \code{\link{plot.elrm}}, \code{\link{elrm}}.}
\examples{
# Drug dataset example with both sex and treatment as the variables of interest
data(drugDat);
drug.elrm = elrm(formula=recovered/n~sex+treatment, interest=~sex+treatment, r=4, 
	iter=50000, burnIn=1000, dataset=drugDat); 

# Summarize the results:
summary(drug.elrm);

# Call:
# [[1]]
# elrm(formula = recovered/n ~ sex + treatment, interest = ~sex + 
#     treatment, r = 4, iter = 1e+05, dataset = drugDat, burnIn = 1000)


# Results:
#           estimate p-value p-value_se mc_size
# joint           NA 0.12951    0.00216   99000
# sex        0.29479 0.54092    0.00880    2749
# treatment  0.82389 0.06892    0.00347   13131

# 95\% Confidence Intervals for Parameters

#                lower    upper
# sex       -0.6109481 1.209525
# treatment -0.1042183 2.028083

\dontrun{
# Urinary tract dataset example with dia as the variable of interst
data(utiDat);
uti.elrm = elrm(uti/n~age+current+dia+oc+pastyr+vi+vic+vicl+vis, interest=~dia, r=4, 
	iter=30000, burnIn=100, dataset=utiDat);

# Summarize the results:
summary(uti.elrm);

# Call:
# [[1]]
# elrm(formula = uti/n ~ age + current + dia + oc + pastyr + vi + 
#     vic + vicl + vis, interest = ~dia, r = 4, iter = 30000, dataset = uti, 
#     burnIn = 1000)


# Results:
#     estimate p-value p-value_se mc_size
# dia  2.07146 0.03286    0.00802   29000

# 95\% Confidence Intervals for Parameters

#           lower upper
# dia -0.06231932   Inf
}
}
\keyword{methods}


