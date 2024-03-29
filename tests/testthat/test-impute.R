source("generate.data.R")

test_that("PKNCA_impute_method_start_conc0", {
  # Time 0 is replaced
  expect_equal(
    PKNCA_impute_method_start_conc0(conc = 1:3, time = 0:2),
    data.frame(conc = c(0, 2:3), time = 0:2)
  )
  # Time 0 is added
  expect_equal(
    PKNCA_impute_method_start_conc0(conc = 2:3, time = 1:2),
    data.frame(conc = c(0, 2:3), time = 0:2),
    ignore_attr = TRUE
  )
  # Time 0 is inserted
  expect_equal(
    PKNCA_impute_method_start_conc0(conc = 1:3, time = c(-1, 1:2)),
    data.frame(conc = c(1, 0, 2:3), time = -1:2),
    ignore_attr = TRUE
  )
})

test_that("PKNCA_impute_method_start_predose", {
  # No modification if no predose samples
  expect_equal(
    PKNCA_impute_method_start_predose(conc = 1:3, time = 1:3, start = 0, end = 24),
    data.frame(conc = 1:3, time = 1:3)
  )
  # No modification if time 0 is already present
  expect_equal(
    PKNCA_impute_method_start_predose(conc = 1:3, time = 0:2, start = 0, end = 24),
    data.frame(conc = 1:3, time = 0:2)
  )
  # Shift happens when time is within max_shift
  expect_equal(
    PKNCA_impute_method_start_predose(conc = 1:3, time = c(-1, 1:2), start = 0, end = 24),
    data.frame(conc = 1:3, time = 0:2)
  )
  # Shift happens when time is equal to max_shift
  expect_equal(
    PKNCA_impute_method_start_predose(conc = 1:3, time = c(-1.2, 1:2), start = 0, end = 24),
    data.frame(conc = 1:3, time = 0:2)
  )
  # Shift occurs to a new start
  expect_equal(
    PKNCA_impute_method_start_predose(conc = 1:3, time = c(-0.3, 1:2), start = 0.5, end = 24),
    data.frame(conc = 1:3, time = c(0.5, 1:2))
  )
  # Shift does not when time is more than max_shift
  expect_equal(
    PKNCA_impute_method_start_predose(conc = 1:3, time = c(-3, 1:2), start = 0, end = 24),
    data.frame(conc = 1:3, time = c(-3, 1:2))
  )
  # max_shift overrides the start/end automation
  expect_equal(
    PKNCA_impute_method_start_predose(conc = 1:3, time = c(-3, 1:2), max_shift = 3, start = 0, end = 24),
    data.frame(conc = 1:3, time = 0:2)
  )
})

test_that("PKNCA_impute_method_start_cmin", {
  # No imputation when start is in the data
  expect_equal(
    PKNCA_impute_method_start_cmin(conc = 1:3, time = 0:2, start = 0, end = 24),
    data.frame(conc = 1:3, time = 0:2)
  )
  # impute when start is not in the data
  expect_equal(
    PKNCA_impute_method_start_cmin(conc = 1:3, time = 1:3, start = 0, end = 24),
    data.frame(conc = c(1, 1:3), time = 0:3),
    ignore_attr = TRUE
  )
  # data outside the interval are ignored (before interval)
  expect_equal(
    PKNCA_impute_method_start_cmin(conc = 1:3, time = 1:3, start = 1.5, end = 24),
    data.frame(conc = c(1, 2, 2:3), time = c(1, 1.5, 2:3)),
    ignore_attr = TRUE
  )
  # data outside the interval are ignored (after interval)
  expect_equal(
    PKNCA_impute_method_start_cmin(conc = c(1:3, 0.5), time = c(1:3, 25), start = 1.5, end = 24),
    data.frame(conc = c(1, 2, 2:3, 0.5), time = c(1, 1.5, 2:3, 25)),
    ignore_attr = TRUE
  )

})

test_that("PKNCA_impute_fun_list", {
  expect_equal(
    PKNCA_impute_fun_list(NA_character_),
    list(NA_character_)
  )
  # an empty string is the same as NA
  expect_equal(
    PKNCA_impute_fun_list(NA_character_),
    PKNCA_impute_fun_list("")
  )
  # logical NA works the same as character NA
  expect_equal(
    PKNCA_impute_fun_list(NA_character_),
    PKNCA_impute_fun_list(NA)
  )
  # Non-character input (other than all NA) is an error
  expect_error(
    PKNCA_impute_fun_list(1),
    regexp = "non-character argument"
  )
  # One imputation method works
  expect_equal(
    PKNCA_impute_fun_list("start_conc0"),
    list("PKNCA_impute_method_start_conc0")
  )
  # Two imputation methods detect errors correctly
  expect_error(
    PKNCA_impute_fun_list("start_conc0,foo"),
    regexp = "The following imputation functions were not found: PKNCA_impute_method_foo"
  )
  # Two imputation methods work
  expect_equal(
    PKNCA_impute_fun_list("start_conc0,start_predose"),
    list(c("PKNCA_impute_method_start_conc0", "PKNCA_impute_method_start_predose"))
  )
  # A vector of different imputation methods works
  expect_equal(
    PKNCA_impute_fun_list(c(NA, NA_character_, "", "start_conc0,start_predose", "start_conc0")),
    list(
      NA_character_,
      NA_character_,
      NA_character_,
      c("PKNCA_impute_method_start_conc0", "PKNCA_impute_method_start_predose"),
      "PKNCA_impute_method_start_conc0"
    )
  )
})

test_that("PKNCA_impute_fun_list_paste", {
  expect_equal(
    PKNCA_impute_fun_list_paste("A"),
    "PKNCA_impute_method_A"
  )
  expect_equal(
    PKNCA_impute_fun_list_paste("PKNCA_impute_method_A"),
    "PKNCA_impute_method_A"
  )
  expect_equal(
    PKNCA_impute_fun_list_paste(NA_character_),
    NA_character_
  )
  expect_equal(
    PKNCA_impute_fun_list_paste(c("PKNCA_impute_method_A", "A", NA)),
    c("PKNCA_impute_method_A", "PKNCA_impute_method_A", NA_character_)
  )
})

test_that("add_impute_to_intervals", {
  d_conc <- generate.conc(nsub=5, ntreat=2, time.points=0:24)
  d_dose <- generate.dose(d_conc)
  o_conc <- PKNCAconc(d_conc, formula=conc~time|treatment+ID)
  o_dose <- PKNCAdose(d_dose, formula=dose~time|treatment+ID)
  o_data <- PKNCAdata(o_conc, o_dose)
  expect_error(
    add_impute_to_intervals(o_data),
    class = "pknca_add_impute_to_intervals_NA"
  )
  # Nothing if impute is in the data
  o_data_start <- o_data
  o_data_start$impute <- "start"
  expect_equal(
    add_impute_to_intervals(PKNCAdata(o_conc, o_dose, impute = "start")),
    o_data_start
  )
  # Impute added to the intervals and the column is modified, if it is not in
  # the data
  o_data <- PKNCAdata(o_conc, o_dose, impute = "start_conc0")
  expect_equal(add_impute_to_intervals(o_data)$impute, "impute")
  expect_true(all(
    add_impute_to_intervals(o_data)$intervals$impute %in% "start_conc0"
  ))
  # If impute is already in the names of the intervals, add something else as
  # the impute column name
  o_data <- PKNCAdata(o_conc, o_dose, impute = "start_conc0")
  o_data$intervals$impute <- "foo"
  expect_equal(add_impute_to_intervals(o_data)$impute, "imputeX")
  expect_true(all(
    add_impute_to_intervals(o_data)$intervals$imputeX %in% "start_conc0"
  ))
})

# Putting it all together
test_that("pk.nca with imputation", {
  d_conc <- as.data.frame(datasets::Theoph)[!datasets::Theoph$Time == 0, ]
  conc_obj <- PKNCAconc(d_conc, conc~Time|Subject)
  d_dose <- unique(datasets::Theoph[datasets::Theoph$Time == 0,
                                    c("Dose", "Time", "Subject")])
  dose_obj <- PKNCAdose(d_dose, Dose~Time|Subject)
  data_obj_noimpute <- PKNCAdata(conc_obj, dose_obj)
  data_obj_impute <- PKNCAdata(conc_obj, dose_obj, impute = "start_predose,start_conc0")
  suppressWarnings(nca_obj_noimpute <- pk.nca(data_obj_noimpute))
  nca_obj_impute <- pk.nca(data_obj_impute)

  # By interval imputation works
  d_intervals <-
    data.frame(
      start=0, end=c(24, 24.1),
      auclast=TRUE,
      impute=c(NA, "start_conc0")
    )

  data_obj_manualimpute <- PKNCAdata(conc_obj, dose_obj, intervals = d_intervals, impute = "impute")
  suppressWarnings(nca_obj_manualimpute <- pk.nca(data_obj_manualimpute))

  auclast_noimpute <- as.data.frame(nca_obj_noimpute)
  auclast_noimpute <- auclast_noimpute$PPORRES[auclast_noimpute$PPTESTCD %in% "auclast"]
  expect_true(all(is.na(auclast_noimpute)))
  auclast_impute <- as.data.frame(nca_obj_impute)
  auclast_impute <- auclast_impute$PPORRES[auclast_impute$PPTESTCD %in% "auclast"]
  expect_true(!any(is.na(auclast_impute)))

  auclast_manualimpute <- as.data.frame(nca_obj_manualimpute)
  auclast_manualimpute_24 <- auclast_manualimpute$PPORRES[auclast_manualimpute$PPTESTCD %in% "auclast" & auclast_manualimpute$end %in% 24]
  auclast_manualimpute_24.1 <- auclast_manualimpute$PPORRES[auclast_manualimpute$PPTESTCD %in% "auclast" & auclast_manualimpute$end %in% 24.1]
  expect_true(all(is.na(auclast_manualimpute_24)))
  expect_true(!any(is.na(auclast_manualimpute_24.1)))
})
