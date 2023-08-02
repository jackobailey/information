
# check_double ------------------------------------------------------------

# Returns error where input is non-numeric --------------------------------

test_that(
  "check_double returns error where input is not a double and not otherwise", {

    # Integer vector (ERROR)
    expect_error(
      check_double(as.integer(0:1))
    )


    # Character vector (ERROR)
    expect_error(
      check_double(letters)
    )

    # Logical vector (ERROR)
    expect_error(
      check_double(c(TRUE, FALSE))
    )

    # List (ERROR)
    expect_error(
      check_double(list(as.integer(0:1), letters, c(TRUE, FALSE)))
    )

    # List with double component (ERROR)
    expect_error(
      check_double(list(c(0.5, 0.4, 0.1), as.integer(0:1), letters, c(TRUE, FALSE)))
    )

    # Double (NO ERROR)
    expect_no_error(
      check_double(c(0.5, 0.4, 0.1))
    )
  }
)



# check_probability -------------------------------------------------------

# Returns error where input is not a valid probability --------------------

test_that(
  "check_probability returns error where input is not a valid probability and not otherwise", {

    # Integer < 0 (ERROR)
    expect_error(
      check_probability(-1)
    )

    # Real number < 0 (ERROR)
    expect_error(
      check_probability(-0.5)
    )

    # Integer > 1 (ERROR)
    expect_error(
      check_probability(2)
    )

    # Real number > 1 (NO ERROR)
    expect_error(
      check_probability(2.5)
    )

    # Valid probability including (NO ERROR)
    expect_no_error(
      check_probability(seq(0, 1, length.out = 15))
    )
  }

)



# check_is_matrix ---------------------------------------------------------

# Returns error where input is not a matrix -------------------------------

test_that(
  "check_is_matrix returns error where input is not a valid matrix and not otherwise", {

    # Integer vector (ERROR)
    expect_error(
      check_is_matrix(as.integer(0:1))
    )


    # Character vector (ERROR)
    expect_error(
      check_is_matrix(letters)
    )

    # Logical vector (ERROR)
    expect_error(
      check_is_matrix(c(TRUE, FALSE))
    )

    # List (ERROR)
    expect_error(
      check_is_matrix(list(as.integer(0:1), letters, c(TRUE, FALSE)))
    )

    # List with double component (ERROR)
    expect_error(
      check_is_matrix(list(c(0.5, 0.4, 0.1), as.integer(0:1), letters, c(TRUE, FALSE)))
    )

    # Double vector (ERROR)
    expect_error(
      check_is_matrix(c(0.5, 0.4, 0.1))
    )


    # Valid matrix (NO ERROR)

    expect_no_error(
      check_is_matrix(
        matrix(
          data = c(0.5, 0.3, 0.2, 0.1),
          ncol = 2
        )
      )
    )

  }

)
