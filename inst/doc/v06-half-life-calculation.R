## ----setup, echo=FALSE, include=FALSE-----------------------------------------
library(PKNCA)
library(dplyr)

## ----check-ggplot, include=!requireNamespace("dplyr"), results="asis"---------
cat("dplyr is required for this vignette to work correctly.  Please install the dplyr library and retry building the vignette.")

## ----normal-example-----------------------------------------------------------
# Perform calculations for subject 1, only
data_conc <- as.data.frame(datasets::Theoph)[datasets::Theoph$Subject == 1, ]

# Keep all points
conc_obj <-
  PKNCAconc(
    data_conc,
    conc~Time|Subject
  )

# Only calculate half-life and parameters required for half-life
current_intervals <- data.frame(start=0, end=Inf, half.life=TRUE)
data_obj <- PKNCAdata(conc_obj, intervals=current_intervals)
result_obj <- pk.nca(data_obj)

# Extract the results for subject 1 
as.data.frame(result_obj)

## ----exclude-points-----------------------------------------------------------
data_conc$exclude_hl <- data_conc$Time == 12.12
# Confirm that we will be excluding exactly one point
stopifnot(sum(data_conc$exclude_hl) == 1)

# Drop one point
conc_obj_exclude1 <-
  PKNCAconc(
    data_conc,
    conc~Time|Subject,
    exclude_half.life="exclude_hl"
  )

data_obj_exclude1 <- PKNCAdata(conc_obj_exclude1, intervals=current_intervals)

# Perform the calculations
result_obj_exclude1 <- pk.nca(data_obj_exclude1)

# Results differ when excluding the 12-hour point for subject 1 (compare to
# example in the previous section)
as.data.frame(result_obj_exclude1)

## ----include-points-----------------------------------------------------------
data_conc$include_hl <- data_conc$Time > 3
# Confirm that we will be excluding exactly one point
stopifnot(sum(data_conc$include_hl) == 6)

# Drop one point
conc_obj_include6 <-
  PKNCAconc(
    data_conc,
    conc~Time|Subject,
    include_half.life="include_hl"
  )

data_obj_include6 <- PKNCAdata(conc_obj_include6, intervals=current_intervals)

# Perform the calculations
result_obj_include6 <- pk.nca(data_obj_include6)

# Results differ when including 6 points (compare to example in the previous
# section)
as.data.frame(result_obj_include6)

