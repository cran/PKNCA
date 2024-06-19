## ----setup, echo=FALSE, include=FALSE-----------------------------------------
library(PKNCA)

## ----setup_data---------------------------------------------------------------
library(PKNCA)
library(dplyr, quietly=TRUE)

## Load the PK concentration data
d_conc <-
  as.data.frame(datasets::Theoph) %>%
  mutate(Subject=as.numeric(as.character(Subject)))
## Generate the dosing data
d_dose <- d_conc[d_conc$Time == 0,]
d_dose$Time <- 0

## Create a concentration object specifying the concentration, time, and
## subject columns.  (Note that any number of grouping levels is
## supported; you are not restricted to just grouping by subject.)
conc_obj <-
  PKNCAconc(
    d_conc,
    conc~Time|Subject
  )
## Create a dosing object specifying the dose, time, and subject
## columns.  (Note that the grouping factors should be the same as or a
## subset of the grouping factors for concentration, and the grouping
## columns must have the same names between concentration and dose
## objects.)
dose_obj <-
  PKNCAdose(
    d_dose,
    Dose~Time|Subject
  )
## Combine the concentration and dosing information both to
## automatically define the intervals for NCA calculation and provide
## doses for calculations requiring dose.
data_obj <- PKNCAdata(conc_obj, dose_obj)

## Calculate the NCA parameters
results_obj <- pk.nca(data_obj)

## Summarize the results
summary(results_obj)

## -----------------------------------------------------------------------------
PKNCA.options()

## ----eval=FALSE---------------------------------------------------------------
#  PKNCA.options(default=TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  PKNCA.set.summary(
#    name = "cmax",
#    description = "geometric mean and geometric coefficient of variation",
#    point = business.geomean,
#    spread = business.geocv,
#    rounding = list(signif=3)
#  )

## ----eval=FALSE---------------------------------------------------------------
#  PKNCA.set.summary(
#    name = "tmax",
#    description = "median and range",
#    point = business.median,
#    spread = business.range,
#    rounding = list(round=2)
#  )

## ----custom_summary_fun, eval=FALSE-------------------------------------------
#  median_na <- function(x) {
#    median(x, na.rm = TRUE)
#  }
#  quantprob_na <- function(x) {
#    quantile(x, probs = c(0.05, 0.95), na.rm=TRUE)
#  }
#  PKNCA.set.summary(
#    name="auclast",
#    description = "median and 5th to 95th percentile",
#    point=median_na,
#    spread=quantprob_na,
#    rounding=list(signif=3)
#  )

## ----multi_summary_settings, eval=FALSE---------------------------------------
#  median_na <- function(x) {
#    median(x, na.rm=TRUE)
#  }
#  quantprob_na <- function(x) {
#    quantile(x, probs=c(0.05, 0.95), na.rm=TRUE)
#  }
#  PKNCA.set.summary(
#    name=c("auclast", "cmax", "tmax", "half.life", "aucinf.pred"),
#    description = "median and 5th to 95th percentile",
#    point=median_na,
#    spread=quantprob_na,
#    rounding=list(signif=3)
#  )

## ----grouping, eval=FALSE-----------------------------------------------------
#  ## Generate a faux multi-study, multi-analyte dataset.
#  d_conc_parent <- d_conc
#  d_conc_parent$Subject <- as.numeric(as.character(d_conc_parent$Subject))
#  d_conc_parent$Study <- d_conc_parent$Subject <= 6
#  d_conc_parent$Analyte <- "Parent"
#  d_conc_metabolite <- d_conc_parent
#  d_conc_metabolite$conc <- d_conc_metabolite$conc/2
#  d_conc_metabolite$Analyte <- "Metabolite"
#  d_conc_both <- rbind(d_conc_parent, d_conc_metabolite)
#  d_conc_both <- d_conc_both[with(d_conc_both, order(Study, Subject, Time, Analyte)),]
#  d_dose_both <- d_conc_both[d_conc_both$Time == 0 & d_conc_both$Analyte %in% "Parent",
#                             c("Study", "Subject", "Dose", "Time")]
#  
#  ## Create a concentration object specifying the concentration, time,
#  ## study, and subject columns.  (Note that any number of grouping
#  ## levels is supporting; you are not restricted to this list.)
#  conc_obj <- PKNCAconc(d_conc_both,
#                        conc~Time|Study+Subject/Analyte)
#  ## Create a dosing object specifying the dose, time, study, and
#  ## subject columns.  (Note that the grouping factors should be a
#  ## subset of the grouping factors for concentration, and the columns
#  ## must have the same names between concentration and dose objects.)
#  dose_obj <- PKNCAdose(d_dose_both,
#                       Dose~Time|Study+Subject)
#  
#  # Perform and summarize the PK data as previously described
#  data_obj <- PKNCAdata(conc_obj, dose_obj)
#  results_obj <- pk.nca(data_obj)
#  summary(results_obj)

## -----------------------------------------------------------------------------
intervals <-
  data.frame(
    start=0, end=c(24, Inf),
    cmax=c(FALSE, TRUE),
    tmax=c(FALSE, TRUE),
    auclast=TRUE,
    aucinf.obs=c(FALSE, TRUE)
  )

## ----asis=TRUE, echo=FALSE----------------------------------------------------
knitr::kable(PKNCA.options()$single.dose.aucs)

## -----------------------------------------------------------------------------
## find.tau can work when all doses have the same interval...
dose_times <- seq(0, 168, by=24)
print(dose_times)
PKNCA::find.tau(dose_times)

## or when the doses have mixed intervals (10 and 24 hours).
dose_times <- sort(c(seq(0, 168, by=24),
                     seq(10, 178, by=24)))
print(dose_times)
PKNCA::find.tau(dose_times)

## ----eval=FALSE---------------------------------------------------------------
#  intervals_manual <-
#    data.frame(
#      start=0, end=c(24, Inf),
#      cmax=c(FALSE, TRUE),
#      tmax=c(FALSE, TRUE),
#      auclast=TRUE,
#      aucinf.obs=c(FALSE, TRUE)
#    )
#  data_obj <-
#    PKNCAdata(
#      conc_obj, dose_obj,
#      intervals=intervals_manual
#    )

## ----eval=FALSE---------------------------------------------------------------
#  data_obj <- PKNCAdata(conc_obj, dose_obj)
#  intervals_manual <- data_obj$intervals
#  intervals_manual$aucinf.obs[1] <- TRUE
#  data_obj$intervals <- intervals_manual

## ----eval=FALSE---------------------------------------------------------------
#  data_obj <- PKNCAdata(conc_obj, dose_obj, options = list(keep_interval_cols = "dosetype"))

## ----eval=FALSE---------------------------------------------------------------
#  summary(o_nca)

