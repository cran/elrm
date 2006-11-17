\name{update.elrm}
\alias{update.elrm}
\alias{update}
\title{Update Method for Objects of Class elrm.}
\description{An update method for objects created by \code{elrm()}. Extends the Markov chain of an \code{elrm} object by a specified number of iterations.}
\usage{\method{update}{elrm}(object, iter, burnIn = 0, alpha = 0.05, ...)}
\arguments{
  \item{object}{an object of class \code{elrm}, resulting from a call to \code{elrm()} or a previous call to \code{update()}.}
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
}
\author{David Zamar, Jinko Graham, Brad McNeney}
\seealso{\code{\link{summary.elrm}}, \code{\link{plot.elrm}}, \code{\link{elrm}}.}
\examples{
# Drug dataset example with sex and treatment as the variables of interest
data(drugDat);
drug.elrm=elrm(formula=recovered/n~sex+treatment,interest=~sex+treatment,r=4,iter=2000,burnIn=0,dataset=drugDat); 

# Summarize the results
summary(drug.elrm);

# Call:
# [[1]]
# elrm(formula = recovered/n ~ sex + treatment, interest = ~sex + 
#     treatment, r = 4, iter = 2000, dataset = drugDat, burnIn = 0)


# Results:
#           estimate p-value p-value_se mc_size
# joint           NA   0.517    0.01755    2000
# sex             NA      NA         NA      90
# treatment       NA      NA         NA     275

# 95\% Confidence Intervals for Parameters

#           lower upper
# sex          NA    NA
# treatment    NA    NA

# Call update and extend the chain by 50000 iterations and set the burn-in period to 100 iterations
drug.elrm = update(drug.elrm, iter=50000, burnIn=100);

# Summarize the results
summary(drug.elrm);

# Call:
# [[1]]
# elrm(formula = recovered/n ~ sex + treatment, interest = ~sex + 
#     treatment, r = 4, iter = 2000, dataset = drugDat, burnIn = 0)

# [[2]]
# update.elrm(object = drug.elrm, iter = 50000, burnIn = 100)


# Results:
#           estimate p-value p-value_se mc_size
# joint           NA 0.14669    0.00314   51900
# sex        0.29431 0.52625    0.01180    1543
# treatment  0.75707 0.07805    0.00512    6842

# 95\% Confidence Intervals for Parameters

#                lower    upper
# sex       -0.6109599 1.230676
# treatment -0.1366174 1.845202

# Now change the burn-in to 5000
drug.elrm = update(drug.elrm, iter=0, burnIn=5000);

# Summarize the results
summary(drug.elrm);

# Call:
# [[1]]
# elrm(formula = recovered/n ~ sex + treatment, interest = ~sex + 
#     treatment, r = 4, iter = 2000, dataset = drugDat, burnIn = 0)

# [[2]]
# update.elrm(object = drug.elrm, iter = 50000, burnIn = 100)

# [[3]]
# update.elrm(object = drug.elrm, iter = 0, burnIn = 5000)


# Results:
#           estimate p-value p-value_se mc_size
# joint           NA 0.13419    0.00341   47000
# sex        0.28423 0.52774    0.01890    1370
# treatment  0.79565 0.07500    0.00377    6227

# 95\% Confidence Intervals for Parameters

#                lower    upper
# sex       -0.6053313 1.199807
# treatment -0.1240906 1.926238
}
\keyword{methods}
