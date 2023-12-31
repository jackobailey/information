% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/information_theory.R
\name{cross_entropy}
\alias{cross_entropy}
\title{Compute Cross Entropy}
\usage{
cross_entropy(p = NULL, q = NULL, base = 2, log0 = FALSE)
}
\arguments{
\item{p}{A numeric vector of probabilities.}

\item{q}{A numeric vector of probabilities.}

\item{base}{Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).}

\item{log0}{If TRUE, allow log(0) = -Inf. If FALSE, use additive smoothing to avoid log(0) = -Inf. Defaults to FALSE.}
}
\value{
\code{cross_entropy()} takes two numeric vectors as its input. It then returns the vectors' cross entropy as its output.
}
\description{
Cross entropy measures the average number of bits required to encode data from one probability distribution using another probability distribution. It is often used to compare two probability distributions and is commonly used in machine learning, particularly in the context of training classifiers.

\code{cross_entropy()} provides a simple way to compute cross entropy in \code{R}.
}
\references{
Shannon, C. E. (1948). A mathematical theory of communication, \emph{The Bell System Technical Journal}. 27(3), pp. 379–423.
}
