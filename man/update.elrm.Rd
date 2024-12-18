\name{update.elrm}
\alias{update.elrm}
\title{Update Method for Objects of Class elrm.}
\description{An update method for objects created by \command{elrm()}. Extends the Markov chain of an \code{elrm} object by a specified number of iterations.}
\usage{\method{update}{elrm}(object, iter, burnIn = 0, alpha = 0.05, ...)}
\arguments{
  \item{object}{an object of class \code{elrm}, resulting from a call to \command{elrm()} or a previous call to \command{update()}.}
  \item{iter}{an integer representing the number of Markov chain iterations to make.}
  \item{burnIn}{the burn-in period to use when conducting inference. Values of the Markov chain in the burn-in period are discarded; default=0.}
  \item{alpha}{determines the level used for confidence intervals; default=0.05.}
  \item{\dots}{additional arguments to the update function (currently unused).}
}
\details{Extends the Markov chain of an \code{elrm} object by creating a new Markov chain of the specified length using the last sampled value as the starting point. The newly created chain is then appended to the original. Subsequent inference is based on the extended Markov chain.}
\value{An object of class \code{elrm}.}
\references{
Zamar David. Monte Carlo Markov Chain Exact Inference for Binomial
Regression Models. Master's thesis, Statistics and Actuarial
Sciences, Simon Fraser University, 2006.

Zamar D, McNeney B and Graham J. elrm: Software Implementing Exact-like Inference for Logistic Regression Models. Journal of Statistical Software 2007, 21(3).
}
\author{David Zamar, Jinko Graham, Brad McNeney}
\seealso{\code{\link{summary.elrm}}, \code{\link{plot.elrm}}, \code{\link{elrm}}.}
\examples{
# Drug dataset example with sex as the variables of interest
data(drugDat);
drug.elrm = elrm(formula=recovered/n~sex, interest=~sex, r=2, 
	iter=1000, burnIn=0, dataset=drugDat); 

# Summarize the results
summary(drug.elrm);

# Call update and extend the chain by 500 iterations and set the burn-in 
# period to 100 iterations
drug.elrm = update(drug.elrm, iter=500, burnIn=100);

# Summarize the results
summary(drug.elrm);

# Now change the burn-in to 200
drug.elrm = update(drug.elrm, iter=0, burnIn=200);

# Summarize the results
summary(drug.elrm);
}
\keyword{methods}
