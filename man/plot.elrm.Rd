\name{plot.elrm}
\alias{plot.elrm}
\title{Plot Diagnostics for an elrm Object}
\description{Produces both a trace plot and histogram of the sampled values of each sufficient statistic of interest. Sampled values within the burn-in period are also plotted.}
\usage{\method{plot}{elrm}(x, p = 1, breaks = "Sturges", ask=FALSE, ...)}
\arguments{
\item{x}{an object of class \code{elrm}, resulting from a call to \command{elrm()} or a previous call to \command{update()}.}
\item{p}{the sampling fraction of points to be plotted. A random sample consisting of \var{p}*100\% of all the observations in the Markov chain is plotted; default=1.}
\item{breaks}{a vector giving the number of cells to use for the histogram of each sufficient statistic of interest or a single number giving the number of cells for each histogram or the character string naming an algorithm to compute the number of cells.}
\item{ask}{the graphics parameter \var{ask}: see \code{\link{par}} for details. If set to TRUE will ask between plots corresponding to each sufficient statistic; default=FALSE.}
\item{\dots}{additional arguments to the plot function (currently unused).}
}
\details{The default for \var{breaks} is "Sturges": see \code{\link[grDevices]{nclass.Sturges}}. Other names for which algorithms are supplied are "Scott" and "FD".}
\references{
Zamar David. Monte Carlo Markov Chain Exact Inference for Binomial
Regression Models. Master's thesis, Statistics and Actuarial
Sciences, Simon Fraser University, 2006.

Zamar D, McNeney B and Graham J. elrm: Software Implementing Exact-like Inference for Logistic Regression Models. Journal of Statistical Software 2007, 21(3).
}
\author{David Zamar, Jinko Graham, Brad McNeney}
\seealso{\code{\link{update.elrm}}, \code{\link{summary.elrm}}, \code{\link{elrm}}.}
\examples{
# Drug dataset example with sex and treatment as the variables of interest
data(drugDat);
drug.elrm = elrm(formula=recovered/n~sex+treatment, interest=~sex+treatment, r=4, 
	iter=50000, burnIn=1000, dataset=drugDat);

# Plot the sampled values of the sufficient statistic for each parameter of 
# interest (sex and treatment)
plot(drug.elrm,p=0.5,ask=TRUE);
}
\keyword{methods}

