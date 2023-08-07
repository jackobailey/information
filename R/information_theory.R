#' Compute Information Content
#'
#' @description
#'
#' Information content measures our surprise in some outcome. Uncommon outcomes have a higher information content. Common outcomes, instead, have a lower information content. This is because uncommon outcomes do more to reduce our uncertainty than do common ones.
#'
#' \code{info()} provides a simple way to compute information content in \code{R}.
#'
#' @return \code{info()} takes a vector or matrix of probabilities as its input. It then returns the information content of each element as its output.
#'
#' @param p A numeric vector or matrix of probabilities.mc
#' @param base Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).
#'
#' @references
#'
#' Shannon, C. E. (1948). A mathematical theory of communication, \emph{The Bell System Technical Journal}. 27(3), pp. 379–423.
#'
#' @export

info <- function(p = NULL, base = 2){

  # Check that all values are numeric

  check_double(p)


  # Check that all values are between 0 and 1

  check_probability(p)


  # Compute information content

  info <- -log(p, base = base)


  # Return to the user

  return(info)

}



#' Compute Entropy
#'
#' @description
#'
#' Information entropy quantifies the uncertainty in a given variable. It measures the average amount of information across each of the variable's possible outcomes. For example, a fair coin toss would have an entropy of \eqn{0.5 \times I(0.5) + 0.5 \times I(0.5) = 1}. Systems with higher entropy are more unpredictable. Systems with lower entropy, instead, are less unpredictable. Importantly, a system's entropy also equals the fewest bits needed to compress it without losing information.
#'
#' \code{entropy()} provides a simple way to compute information entropy in \code{R}.
#'
#' @return \code{entropy()} takes a vector of probabilities as its input. It then returns the vector's entropy as its output.
#'
#' @param p A numeric vector of probabilities.
#' @param base Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).
#'
#' @references
#'
#' Shannon, C. E. (1948). A mathematical theory of communication, \emph{The Bell System Technical Journal}. 27(3), pp. 379–423.
#'
#' @export

entropy <- function(p = NULL, base = 2){

  # Check input probability sums to 1

  check_sums_one(p)


  # Compute information content

  entropy <- sum(p * info(p, base = base), na.rm = T)


  # Return to the user

  return(entropy)

}



#' Compute Joint Entropy
#'
#' @description
#'
#' Joint entropy quantifies the uncertainty in two or more variables considered together. It measures the average amount of information needed to describe the combined outcomes of these variables. When the variables are independent, the joint entropy is the sum of their individual entropies. However, when they are dependent, the joint entropy is lower than the sum of individual entropies since knowledge of one variable can provide information about the other.
#'
#' \code{joint_entropy()} provides a simple way to compute joint entropy in \code{R}.
#'
#' @return \code{joint_entropy()} takes a matrix of probabilities as its input. It then returns the matrix's joint entropy as its output.
#'
#' @param p A matrix of probabilities.
#' @param base Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).
#'
#' @references
#'
#' Shannon, C. E. (1948). A mathematical theory of communication, \emph{The Bell System Technical Journal}. 27(3), pp. 379–423.
#'
#' @export

joint_entropy <- function(p = NULL, base = 2){

  # Check input probability sums to 1

  check_sums_one(p)


  # Check that the input is a matrix

  check_is_matrix(p)


  # Compute information content

  entropy <- sum(p * info(p, base = base), na.rm = T)


  # Return to the user

  return(entropy)

}



#' Compute Mutual Information
#'
#' @description
#'
#' Mutual information measures the amount of information shared between two variables. It quantifies how much knowing the value of one variable reduces the uncertainty about the other. When the variables are independent, the mutual information is zero, indicating no shared information. Conversely, a higher mutual information value indicates a stronger dependency between the variables.
#'
#' \code{mutual_info()} provides a simple way to compute mutual information in \code{R}.
#'
#' @return \code{mutual_info()} takes a matrix of probabilities as its input. It then returns the matrix's mutual information as its output.
#'
#' @param p A matrix of probabilities.
#' @param base Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).
#'
#' @references
#'
#' Shannon, C. E. (1948). A mathematical theory of communication, \emph{The Bell System Technical Journal}. 27(3), pp. 379–423.
#'
#' @export

mutual_info <- function(p = NULL, base = 2){

  # Check input probability sums to 1

  check_sums_one(p)


  # Check that the input is a matrix

  check_is_matrix(p)


  # Compute mutual information

  mutual_info <-
    entropy(colSums(p), base = base) +
    entropy(rowSums(p), base = base) -
    joint_entropy(p, base = base)


  # Return to the user

  return(mutual_info)

}



#' Compute Conditional Entropy
#'
#' @description
#'
#' Conditional entropy quantifies the uncertainty remaining in one variable when another variable is known. It measures the average amount of information required to describe the first variable given the value of the second. Lower conditional entropy suggests a more predictable relationship between the variables, whereas higher conditional entropy indicates greater uncertainty.
#'
#' \code{conditional_entropy()} provides a simple way to compute conditional entropy in \code{R}.
#'
#' @return \code{conditional_entropy()} takes a matrix of probabilities as its input. It then returns the matrix's entropy as its output.
#'
#' @param p A matrix of probabilities.
#' @param margin Which variable to condition on? Either 1 (rows) or 2 (columns). Defaults to 1 (rows)
#' @param base Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).
#'
#' @references
#'
#' Shannon, C. E. (1948). A mathematical theory of communication, \emph{The Bell System Technical Journal}. 27(3), pp. 379–423.
#'
#' @export

conditional_entropy <- function(p = NULL, margin = 1, base = 2){

  # Check input probability sums to 1

  check_sums_one(p)


  # Compute conditional entropy

  if(margin == 1){

    conditional_entropy <-
      entropy(rowSums(p), base = base) -
      mutual_info(p, base = base)

  } else if(margin == 2){

    conditional_entropy <-
      entropy(colSums(p), base = base) -
      mutual_info(p, base = base)

  }


  # Return to the user

  return(conditional_entropy)

}



#' Compute Cross Entropy
#'
#' @description
#'
#' Cross entropy measures the average number of bits required to encode data from one probability distribution using another probability distribution. It is often used to compare two probability distributions and is commonly used in machine learning, particularly in the context of training classifiers.
#'
#' \code{cross_entropy()} provides a simple way to compute cross entropy in \code{R}.
#'
#' @return \code{cross_entropy()} takes two numeric vectors as its input. It then returns the vectors' cross entropy as its output.
#'
#' @param p A numeric vector of probabilities.
#' @param q A numeric vector of probabilities.
#' @param base Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).
#' @param log0 If TRUE, allow log(0) = -Inf. If FALSE, use additive smoothing to avoid log(0) = -Inf. Defaults to FALSE.
#'
#' @references
#'
#' Shannon, C. E. (1948). A mathematical theory of communication, \emph{The Bell System Technical Journal}. 27(3), pp. 379–423.
#'
#' @export

cross_entropy <- function(p = NULL, q = NULL, base = 2, log0 = FALSE){

  # Check p is a probability and sums to 1

  check_sums_one(p)


  # Check q is a probability and sums to 1

  check_sums_one(q)


  # Avoid log(0) using additive smoothing and normalisation

  if(log0 == FALSE){
    if(any(p == 0) == T){
      p <- add_smooth_normalise(p)
    } else if(any(q == 0) == T){
      q <- add_smooth_normalise(q)
    }
  }


  # Compute cross entropy

  cross_entropy <- sum(p * info(q, base = base), na.rm = T)


  # Return to the user

  return(cross_entropy)

}



#' Compute Relative Entropy
#'
#' @description
#'
#' Relative entropy, also known as Kullback-Leibler divergence, quantifies the difference between two probability distributions. It measures how much one distribution differs from another. A relative entropy value of zero indicates that the distributions are identical, while a higher value suggests greater dissimilarity.
#'
#' \code{relative_entropy()} provides a simple way to compute relative entropy in \code{R}.
#'
#' @return \code{relative_entropy()} takes two numeric vectors as its input. It then returns the vectors' relative entropy as its output.
#'
#' @param p A numeric vector of probabilities.
#' @param q A numeric vector of probabilities.
#' @param base Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).
#' @param log0 If TRUE, allow log(0) = -Inf. If FALSE, use additive smoothing to avoid log(0) = -Inf. Defaults to FALSE.
#'
#' @references
#'
#' Kullback, S, and Leibler, R.A. (1951). On information and sufficiency, \emph{Annals of Mathematical Statistics}. 22(1), pp. 79-86. DOI: 10.1214/aoms/1177729694
#'
#' @export

relative_entropy <- function(p = NULL, q = NULL, base = 2, log0 = FALSE){

  # Avoid log(0) using additive smoothing and normalisation

  if(log0 == FALSE){
    if(any(p == 0) == T){
      p <- add_smooth_normalise(p)
    } else if(any(q == 0) == T){
      q <- add_smooth_normalise(q)
    }
  }


  # Compute relative entropy

  relative_entropy <- cross_entropy(p, q, base = base, log0 = log0) - entropy(p, base)


  # Return to the user

  return(relative_entropy)

}


#' Compute Information Radius
#'
#' @description
#'
#' Information radius, also known as Jensen-Shannon divergence, is a symmetrised version of the Kullback-Leibler divergence. It quantifies the similarity between two probability distributions and represents the average "distance" between them. The information radius ranges from 0 to 1, where 0 indicates identical distributions, and 1 suggests completely different distributions.
#'
#' \code{info_radius()} provides a simple way to compute the Jensen-Shannon divergence in \code{R}.
#'
#' @return \code{info_radius()} takes two numeric vectors as its input. It then returns the vectors' relative entropy as its output.
#'
#' @param p A numeric vector of probabilities.
#' @param q A numeric vector of probabilities.
#' @param base Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).
#'
#' @references
#'
#' Shannon, C. E. (1948). A mathematical theory of communication, \emph{The Bell System Technical Journal}. 27(3), pp. 379–423.
#'
#' @export

info_radius <- function(p = NULL, q = NULL, base = 2){

  # Compute average of two distributions

  m <- (p + q)/2


  # Compute Jensen-Shannon Divergence

  info_radius <- (0.5 * relative_entropy(p, m, base, log0 = TRUE)) + (0.5 * relative_entropy(q, m, base, log0 = TRUE))


  # Return to the user

  return(info_radius)

}



#' Compute Pointwise Mutual Information
#'
#' @description
#'
#' Pointwise mutual information measures how much information the outcomes of a set of variables' share. Unlike many other information theoretic quantities, it can be either positive or negative. A positive value indicates events that tend to occur together. A negative value suggests events that are likely independent. The expected value of the pointwise mutual information of a set of variables equals its mutual information.
#'
#' \code{pointwise_mutual_info} provides a simple way to compute pointwise mutual information in \code{R}.
#'
#' @return \code{pointwise_mutual_info()} takes a matrix of probabilities as its input. It then returns each element's pointwise mutual information as its output.
#'
#' @param p A matrix of probabilities.
#' @param base Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).
#'
#' @references
#'
#' Shannon, C. E. (1948). A mathematical theory of communication, \emph{The Bell System Technical Journal}. 27(3), pp. 379–423.
#'
#' @export

pointwise_mutual_info <- function(p = NULL, base = 2){

  # Check p is a probability and sums to 1

  check_sums_one(p)


  # Check that the input is a matrix

  check_is_matrix(p)


  # Compute pointwise mutual information

  pointwise_mutual_info <- log(p/(rowSums(p) %*% t(colSums(p))), base = base)


  # Return to the user

  return(pointwise_mutual_info)

}


#' Compute the Variation of Information
#'
#' @description
#'
#' Variation of Information (VOI) is a measure of dissimilarity between two variables. It quantifies how much information is lost or gained when transforming one variable into another. Lower values suggest greater similarity between the variables. Higher values, instead, suggest greater dissimilarity between them.
#'
#' \code{variation_of_info()} provides a simple way to compute the variation of information in \code{R}.
#'
#' @return \code{variation_of_info()} takes a matrix of probabilities as its input. It then returns the matrix's variation of information as its output.
#'
#' @param p A matrix of probabilities.
#' @param base Which log base to use? Three are most typical. Base 2 for "bits", base \emph{e} for "nats", and base 10 for "hartleys". Defaults to base 2 (bits).
#'
#' @references
#'
#' Shannon, C. E. (1948). A mathematical theory of communication, \emph{The Bell System Technical Journal}. 27(3), pp. 379–423.
#'
#' @export

variation_of_info <- function(p = NULL, base = 2){

  # Check p is a probability and sums to 1

  check_sums_one(p)


  # Check that the input is a matrix

  check_is_matrix(p)


  # Compute variation of information

  variation_of_info <- joint_entropy(p) - mutual_info(p)


  # Return to the user

  return(variation_of_info)

}
