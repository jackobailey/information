% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/information_theory.R
\name{joint_entropy}
\alias{joint_entropy}
\title{Compute Joint Entropy}
\usage{
joint_entropy(p = NULL, base = 2)
}
\arguments{
\item{p}{A matrix of probabilities.}

\item{base}{Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).}
}
\value{
\code{joint_entropy()} takes a matrix of probabilities as its input. It then returns the matrix's joint entropy as its output.
}
\description{
Joint entropy quantifies the uncertainty in two or more variables considered together. It measures the average amount of information needed to describe the combined outcomes of these variables. When the variables are independent, the joint entropy is the sum of their individual entropies. However, when they are dependent, the joint entropy is lower than the sum of individual entropies since knowledge of one variable can provide information about the other.

\code{joint_entropy()} provides a simple way to compute joint entropy in \code{R}.
}
\references{
Shannon, C. E. (1948). A mathematical theory of communication, \emph{The Bell System Technical Journal}. 27(3), pp. 379–423.
}
