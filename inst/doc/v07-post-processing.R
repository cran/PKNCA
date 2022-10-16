## ----setup, echo=FALSE, include=FALSE-----------------------------------------
library(PKNCA)
library(dplyr)

## ----setup-example------------------------------------------------------------
library(PKNCA)
suppressPackageStartupMessages(library(dplyr))

d_conc <-
  as.data.frame(datasets::Theoph) %>%
  mutate(Subject=as.numeric(as.character(Subject)))
## Generate the dosing data
d_dose <- d_conc[d_conc$Time == 0,]
d_dose$Time <- 0

conc_obj <-
  PKNCAconc(
    d_conc,
    conc~Time|Subject
  )
dose_obj <-
  PKNCAdose(
    d_dose,
    Dose~Time|Subject
  )
data_obj <- PKNCAdata(conc_obj, dose_obj)
results_obj <- pk.nca(data_obj)

## ----exclude-function---------------------------------------------------------
results_excl_span <- exclude(results_obj, FUN=exclude_nca_span.ratio())

# Without any exclusions applied, the 'exclude' column is all NA.
as.data.frame(results_obj) %>%
  filter(Subject == 1)
# With exclusions applied, the 'exclude' column has the reason for exclusion.
as.data.frame(results_excl_span) %>%
  filter(Subject == 1)

## ----exclude-specific---------------------------------------------------------
mask_exclude_cmax <-
  results_obj %>%
  as.data.frame() %>%
  dplyr::mutate(
    mask_exclude=Subject == 1 & PPTESTCD == "cmax"
  ) %>%
  "[["("mask_exclude")
results_excl_specific <-
  exclude(
    results_obj,
    mask=mask_exclude_cmax,
    reason="Cmax was actually above the ULOQ"
  )

# Without any exclusions applied, the 'exclude' column is all NA.
results_obj %>%
  as.data.frame() %>%
  filter(Subject == 1)
# With exclusions applied, the 'exclude' column has the reason for exclusion.
results_excl_specific %>%
  as.data.frame() %>%
  filter(Subject == 1)

## ----exclude-multi------------------------------------------------------------
mask_exclude_lz <-
  results_obj %>%
  as.data.frame() %>%
  dplyr::mutate(
    mask_exclude=Subject == 1 & PPTESTCD == "lambda.z"
  ) %>%
  "[["("mask_exclude")

# Starting from the exclusion example above where short span ratios were
# excluded, exclude Cmax for Subject 1, too.
results_excl_multi <-
  exclude(
    results_excl_span,
    mask=mask_exclude_cmax,
    reason="Cmax was actually above the ULOQ"
  )
results_excl_multi <-
  exclude(
    results_excl_multi,
    mask=mask_exclude_lz,
    reason="Issue with lambda.z fit"
  )

# With exclusions applied, the 'exclude' column has the reason for exclusion.
# More than one reason may appear if more than one exclusion is applied.
results_excl_multi %>%
  as.data.frame() %>%
  filter(Subject == 1)

## ----summary------------------------------------------------------------------
summary(results_obj)

## ----summary-exclusion--------------------------------------------------------
summary(results_excl_span)

## ----listing------------------------------------------------------------------
as.data.frame(results_obj) %>%
  head(20)

## ----listing-exclusion--------------------------------------------------------
as.data.frame(results_excl_span) %>%
  head(20)

