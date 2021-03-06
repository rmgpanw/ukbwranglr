
# EXPORTED FUNCTIONS ------------------------------------------------------


# PRIVATE FUNCTIONS -------------------------------------------------------

make_dummy_gp_clinical_df_single_eid <- function(eid = 1,
                                                 n_rows = 5,
                                                 coding = "read_2") {
  # validate args
  assert_integer_ge_n(eid, arg_name = "eid", n = 1)
  assert_integer_ge_n(n_rows, arg_name = "n_rows", n = 1)
  match.arg(coding, choices = c("read_2", "read_3"))

  # make dummy df
  set.seed(eid)

  eid <- rep(eid, n_rows)
  data_provider <- sample(x = c("1", "2", "3", "4"),
                          size = n_rows,
                          replace = TRUE)
  event_dt <- sample(
    x = c(
      "01/02/1999", # normal date
      "01/01/1901",
      "02/02/1902",
      "03/03/1903",
      "07/07/2037"
    ),
    size = n_rows,
    replace = TRUE,
    prob = c(0.5, 0.1, 0.1, 0.1, 0.2)
  )

  if (coding == "read_2") {
    read_2 <- sample(LETTERS[1:10],
                     size = n_rows,
                     replace = TRUE)
    read_3 <- NA
  } else if (coding == "read_3") {
    read_2 <- NA
    read_3 <- sample(LETTERS[1:10],
                     size = n_rows,
                     replace = TRUE)
  }
  value_1 <- NA
  value_2 <- NA
  value_3 <- NA

  tibble::tibble(eid = eid,
                 data_provider = data_provider,
                 event_dt = event_dt,
                 read_2 = read_2,
                 read_3 = read_3,
                 value_1 = value_1,
                 value_2 = value_2,
                 value_3 = value_3)
}

make_dummy_clinical_events_df_single_eid <- function(eid = 1,
                                                     n_rows = 100) {

  # validate args
  assert_integer_ge_n(eid, arg_name = "eid", n = 1)
  assert_integer_ge_n(n_rows, arg_name = "n_rows", n = 1)

  # make dummy df
  set.seed(eid)

  eid <- rep(eid, n_rows)
  source <- sample(ukbwranglr:::clinical_events_sources$source,
                   size = n_rows,
                   replace = TRUE)
  code <- sample(LETTERS[1:10],
                 size = n_rows,
                 replace = TRUE)
  date <- sample(seq(as.Date("2000/01/01"),
                     by = "day",
                     length.out = 200),
                 size = n_rows,
                 replace = TRUE)

  tibble::tibble(eid = eid,
                 source = source,
                 code = code,
                 date = date)
}

make_dummy_clinical_events_df <- function(eids = c(1, 2, 3),
                                          n_rows = c(200, 200, 200)) {

  # validate args
  assertthat::assert_that(length(eids) == length(n_rows),
                          msg = "`eids` and `n_rows` must be the same length")

  # combine args for `pmap()`
  args <- tibble::tibble(eid = eids, n_rows = n_rows)

  args %>%
    purrr::pmap(make_dummy_clinical_events_df_single_eid) %>%
    dplyr::bind_rows()
}

#' Make dummy data frame
#'
#' Returns a dataframe with 6 rows and columns of types character, integer and factor
#'
#' @return df
#' @noRd
make_dummy_df <- function() {
  data.frame(chr = c(letters[1:5], NA),
             int = c(1:5, NA),
             fac = as.factor(c(letters[1:5], NA)),
             log = c(rep(TRUE, 5), FALSE),
             chr_rpt = c(rep("a", 3), rep("c", 3)))
}
