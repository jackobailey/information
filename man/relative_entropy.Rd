% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/information_theory.R
\name{relative_entropy}
\alias{relative_entropy}
\title{Compute Relative Entropy}
\usage{
relative_entropy(p = NULL, q = NULL, base = 2, log0 = FALSE)
}
\arguments{
\item{p}{A numeric vector of probabilities.}

\item{q}{A numeric vector of probabilities.}

\item{base}{Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).}

\item{log0}{If TRUE, allow log(0) = -Inf. If FALSE, use additive smoothing to avoid log(0) = -Inf. Defaults to FALSE.}
}
\value{
\code{relative_entropy()} takes two numeric vectors as its input. It then returns the vectors' relative entropy as its output.
}
\description{
Relative entropy, also known as Kullback-Leibler divergence, quantifies the difference between two probability distributions. It measures how much one distribution differs from another. A relative entropy value of zero indicates that the distributions are identical, while a higher value suggests greater dissimilarity.

\code{relative_entropy()} provides a simple way to compute relative entropy in \code{R}.
}
\references{
Kullback, S, and Leibler, R.A. (1951). On information and sufficiency, \emph{Annals of Mathematical Statistics}. 22(1), pp. 79-86. DOI: 10.1214/aoms/1177729694
}
