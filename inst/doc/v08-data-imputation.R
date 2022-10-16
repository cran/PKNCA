## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----results='markup'---------------------------------------------------------
library(PKNCA)
cat(paste(
  "*", ls("package:PKNCA", pattern = "^PKNCA_impute_method")
), sep = "\n")

## ----impute-full-data---------------------------------------------------------
library(PKNCA)
# Remove time 0 to illustrate that imputation works
d_conc <- as.data.frame(datasets::Theoph)[!datasets::Theoph$Time == 0, ]
conc_obj <- PKNCAconc(d_conc, conc~Time|Subject)
d_dose <- unique(datasets::Theoph[datasets::Theoph$Time == 0,
                                  c("Dose", "Time", "Subject")])
dose_obj <- PKNCAdose(d_dose, Dose~Time|Subject)
data_obj <- PKNCAdata(conc_obj, dose_obj, impute = "start_predose,start_conc0")
nca_obj <- pk.nca(data_obj)
summary(nca_obj)

## ----impute-by-interval-------------------------------------------------------
library(PKNCA)
# Remove time 0 to illustrate that imputation works
d_conc <- as.data.frame(datasets::Theoph)[!datasets::Theoph$Time == 0, ]
conc_obj <- PKNCAconc(d_conc, conc~Time|Subject)
d_dose <- unique(datasets::Theoph[datasets::Theoph$Time == 0,
                                  c("Dose", "Time", "Subject")])
dose_obj <- PKNCAdose(d_dose, Dose~Time|Subject)

d_intervals <-
  data.frame(
    start=0, end=c(24, 24.1),
    auclast=TRUE,
    impute=c(NA, "start_conc0")
  )

data_obj <- PKNCAdata(conc_obj, dose_obj, intervals = d_intervals, impute = "impute")
nca_obj <- pk.nca(data_obj)
# PKNCA does not impute time 0 by default, so AUClast in the 0-24 interval is
# not calculated
summary(nca_obj)

