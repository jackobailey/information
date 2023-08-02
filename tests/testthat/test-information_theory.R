
# Housekeeping ------------------------------------------------------------

# Define vector of probabilities to test with

p <- c(0.5, 0.3, 0.1, 0.1)
q <- c(0.5, 0.2, 0.2, 0.1)


# Define matrix of probabilities to test with

p_mat <- matrix(data = p, ncol = 2)


# Set up information content/relative entropy test

value_to_discover <- 4
possible_values <- 1:5
prob_values <- c(0.35, 0.3, 0.2, 0.1, 0.05)



# info --------------------------------------------------------------------

# Shows expected relationships with other quantities ----------------------

test_that(
  "info() shows expected relationships with other quantities", {

    # I(x) = -\text{log}_{2}P

    expect_equal(
      info(0.5),
      -log(0.5, 2)
    )


    # I(x) = \text{log}_{2}\frac{1}{p_{X}(x)}

    expect_equal(
      info(0.5),
      log(1/0.5, 2)
    )


    # \text{log-odds}(x) = I(\lnot x) - I(x)

    expect_equal(
      log(0.75/0.25, 2),
      info(0.25) - info(0.75)
    )


    # I(m) = D_{KL}(\delta_{im}\parallel {p_{i}})

    expect_equal(
      info(prob_values[value_to_discover]),
      relative_entropy(possible_values == value_to_discover, prob_values)
    )

  }
)



# entropy -----------------------------------------------------------------

# Shows expected relationships with other quantities ----------------------

test_that(
  "entropy() shows expected relationships with other quantities", {

    # H(X,Y) = -\sum_{i=1}^{m}\sum_{j=1}^{m} p(x_{i},y_{j})\text{log}_{2}p(x_{i},y_{j})

    expect_equal(
      entropy(p),
      sum(p * info(p))
    )


    # H(X) = log_{2}(N) - D_{KL}(p_X(x)||P_U(X))

    expect_equal(
      entropy(p),
      log(4, 2) - relative_entropy(p, rep(1/4, 4))
    )

  }
)



# joint_entropy -----------------------------------------------------------

# Shows expected relationships with other quantities ----------------------

test_that(
  "joint_entropy() shows expected relationships with other quantities", {

    # H(X,Y) = I(X,Y) + H(X|Y) + H(Y|X)

    expect_equal(
      joint_entropy(p_mat),
      sum(p_mat * info(p_mat))
    )


    # H(X,Y) = I(X,Y) + H(X|Y) + H(Y|X)

    expect_equal(
      joint_entropy(p_mat),
      mutual_info(p_mat) + conditional_entropy(p_mat, 1) + conditional_entropy(p_mat, 2)
    )


    # H(X,Y) = H(X) + H(Y|X)

    expect_equal(
      joint_entropy(p_mat),
      entropy(rowSums(p_mat)) + conditional_entropy(p_mat, 2)
    )


    # H(X,Y) = H(Y) + H(X|Y)

    expect_equal(
      joint_entropy(p_mat),
      entropy(colSums(p_mat)) + conditional_entropy(p_mat, 1)
    )


    # Joint entropy of two independent variables is the sum of their entropies

    expect_equal(
      joint_entropy(matrix(data = rep(1/4, 4), ncol = 2)),
      entropy(c(0.5, 0.5)) + entropy(c(0.5, 0.5))
    )

  }
)



# mutual_info -------------------------------------------------------------

# Shows expected relationships with other quantities ----------------------

test_that(
  "mutual_info() shows expected relationships with other quantities", {

    # I(X,Y) = H(X) + H(Y) - H(X,Y)

    expect_equal(
      mutual_info(p_mat),
      entropy(rowSums(p_mat)) + entropy(colSums(p_mat)) - joint_entropy(p_mat)
    )


    # I(X,Y) = H(X) - H(X|Y)

    expect_equal(
      mutual_info(p_mat),
      entropy(rowSums(p_mat)) - conditional_entropy(p_mat, 1)
    )


    # I(X,Y) = H(Y) - H(Y|X)

    expect_equal(
      mutual_info(p_mat),
      entropy(colSums(p_mat)) - conditional_entropy(p_mat, 2)
    )


    # I(X,Y) = H(X,Y) - [H(X|Y) + H(Y|X)]

    expect_equal(
      mutual_info(p_mat),
      joint_entropy(p_mat) - (conditional_entropy(p_mat, 1) + conditional_entropy(p_mat, 2))
    )


    # I(X,Y) = H(X,Y) - VI(X;Y)

    expect_equal(
      mutual_info(p_mat),
      joint_entropy(p_mat) - variation_of_info(p_mat)
    )


    # Mutual information of two independent distributions is 0

    expect_equal(
      mutual_info(p = matrix(data = rep(1/4, 4), ncol = 2)),
      0
    )

  }
)



# conditional_entropy -----------------------------------------------------

# Shows expected relationships with other quantities ----------------------

test_that(
  "conditional_entropy() shows expected relationships with other quantities", {

    # H(Y|X) = H(X,Y) - H(Y)

    expect_equal(
      conditional_entropy(p_mat, 1),
      joint_entropy(p_mat) - entropy(colSums(p_mat))
    )


    # H(X|Y) = H(X,Y) - H(X)

    expect_equal(
      conditional_entropy(p_mat, 2),
      joint_entropy(p_mat) - entropy(rowSums(p_mat))
    )


    # H(Y|X) = \text{log}_{2}(N) - \text{E}_{Y}\left[ D_{KL}(P(Y|X)\parallel P_{U}(Y)) \right]

    expect_equal(
      conditional_entropy(p_mat, 2),
      log(2, 2) - relative_entropy(p_mat, t(c(0.5, 0.5) %*% t(rowSums(p_mat))))
    )


    # H(X|Y) = \text{log}_{2}(N) - \text{E}_{Y}\left[ D_{KL}(P(X|Y)\parallel P_{U}(X)) \right]

    expect_equal(
      conditional_entropy(p_mat, 1),
      log(2, 2) - relative_entropy(p_mat, c(0.5, 0.5) %*% t(colSums(p_mat)))
    )

  }
)


# cross_entropy -----------------------------------------------------------

# Shows expected relationships with other quantities ----------------------

test_that(
  "cross_entropy() shows expected relationships with other quantities", {

    # H(p,q) = H(p) + D_{KL}(p\parallel q)

    expect_equal(
      cross_entropy(p, q),
      entropy(p) + relative_entropy(p, q)
    )

  }
)


# Uses log(0) when asked to -----------------------------------------------

test_that(
  "cross_entropy() uses log(0) when asked to", {

    # Use log(0) [Answer == Inf]

    expect_equal(
      cross_entropy(c(0.5, 0.5), c(1, 0), log0 = T),
      Inf
    )


    # Does not use log(0)

    expect_true(
      cross_entropy(c(0.5, 0.5), c(1, 0), log0 = F) != Inf
    )

  }
)



# relative_entropy --------------------------------------------------------

# Shows expected relationships with other quantities ----------------------

test_that(
  "relative_entropy() shows expected relationships with other quantities", {

    # D_{KL}(P(X,Y)\parallel P(X)P(Y)) = I(X;Y)

    expect_equal(
      relative_entropy(p_mat, rowSums(p_mat) %*% t(colSums(p_mat))),
      mutual_info(p_mat)
    )

  }
)


# Uses log(0) when asked to -----------------------------------------------

test_that(
  "relative_entropy() uses log(0) when asked to", {

    # Use log(0) [Answer == Inf]

    expect_equal(
      relative_entropy(c(0.5, 0.5), c(1, 0), log0 = T),
      Inf
    )


    # Does not use log(0)

    expect_true(
      relative_entropy(c(0.5, 0.5), c(1, 0), log0 = F) != Inf
    )

  }
)



# info_radius -------------------------------------------------------------

# Shows expected relationships with other quantities ----------------------

test_that(
  "info_radius() shows expected relationships with other quantities", {

    # IR(P,Q) = H(M) - \frac{1}{2}\bigl( H(P) + H(Q) \bigr)

    expect_equal(
      info_radius(p, q),
      entropy((p + q)/2) - 0.5*(entropy(p) + entropy(q))
    )

  }
)



# pointwise_mutual_info ---------------------------------------------------

# Shows expected relationships with other quantities ----------------------

test_that(
  "pointwise_mutual_info() shows expected relationships with other quantities", {

    # Weighted average of PMI equals the mutual information

    expect_equal(
      sum(p_mat * pointwise_mutual_info(p_mat)),
      mutual_info(p_mat)
    )

  }
)



# variation_of_info -------------------------------------------------------

# Shows expected relationships with other quantities ----------------------

test_that(
  "variation_of_info() shows expected relationships with other quantities", {

    # VI(X;Y) = H(X) + H(Y) - 2I(X;Y)

    expect_equal(
      variation_of_info(p_mat),
      entropy(rowSums(p_mat)) + entropy(colSums(p_mat)) - 2*mutual_info(p_mat)
    )

  }
)
