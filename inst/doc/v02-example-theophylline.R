## ----setup, echo=FALSE, include=FALSE-----------------------------------------
library(PKNCA)
library(knitr)

## ----showtheoph---------------------------------------------------------------
## It is always a good idea to look at the data
knitr::kable(head(datasets::Theoph))

## ----setupconcdose------------------------------------------------------------
## By default it is groupedData; convert it to a data frame for use
conc_obj <- PKNCAconc(as.data.frame(datasets::Theoph), conc~Time|Subject)

## Dosing data needs to only have one row per dose, so subset for
## that first.
d_dose <- unique(datasets::Theoph[datasets::Theoph$Time == 0,
                                  c("Dose", "Time", "Subject")])
knitr::kable(d_dose,
             caption="Example dosing data extracted from theophylline data set")
dose_obj <- PKNCAdose(d_dose, Dose~Time|Subject)

## ----autointervals------------------------------------------------------------
data_obj_automatic <- PKNCAdata(conc_obj, dose_obj)
knitr::kable(PKNCA.options("single.dose.aucs"))
knitr::kable(data_obj_automatic$intervals)

## ----manualintervals----------------------------------------------------------
intervals_manual <- data.frame(start=0,
                               end=Inf,
                               cmax=TRUE,
                               tmax=TRUE,
                               aucinf.obs=TRUE,
                               auclast=TRUE)
data_obj_manual <- PKNCAdata(conc_obj, dose_obj,
                             intervals=intervals_manual)
knitr::kable(data_obj_manual$intervals)

## ----calculationauto----------------------------------------------------------
results_obj_automatic <- pk.nca(data_obj_automatic)
knitr::kable(head(as.data.frame(results_obj_automatic)))

## ----calculationautoshow, eval=FALSE------------------------------------------
# summary(results_obj_automatic)

## ----calculationautoshowpretty, echo=FALSE------------------------------------
## Make a pretty table instead of the data.frame preformatted printout
knitr::kable(summary(results_obj_automatic))

## ----calculationmanual--------------------------------------------------------
results_obj_manual <- pk.nca(data_obj_manual)
knitr::kable(head(as.data.frame(results_obj_manual)))

## ----calculationmanualshow, eval=FALSE----------------------------------------
# summary(results_obj_manual)

## ----calculationmanualshowpretty, echo=FALSE----------------------------------
## Make a pretty table instead of the data.frame preformatted printout
knitr::kable(summary(results_obj_manual))

## ----superposition------------------------------------------------------------
d_conc <- PKNCAconc(as.data.frame(Theoph), conc~Time|Subject)
conc_obj_multi <-
  PKNCAconc(
    superposition(d_conc,
                  tau=168,
                  dose.times=seq(0, 144, by=24),
                  n.tau=1,
                  check.blq=FALSE),
    conc~time|Subject)
conc_obj_multi
dose_obj_multi <- PKNCAdose(expand.grid(Subject=unique(as.data.frame(conc_obj_multi)$Subject),
                                      time=seq(0, 144, by=24)),
                          ~time|Subject)
dose_obj_multi

## ----multi_auto_choose_intervals----------------------------------------------
data_obj <- PKNCAdata(conc_obj_multi, dose_obj_multi)
data_obj$intervals[,c("Subject", "start", "end")]

## ----multi_manual_choose_intervals--------------------------------------------
intervals_manual <- data.frame(start=c(0, 144),
                               end=c(24, 168),
                               cmax=TRUE,
                               auclast=TRUE)
data_obj <- PKNCAdata(conc_obj_multi, dose_obj_multi,
                      intervals=intervals_manual)
data_obj$intervals

## ----multi_calc---------------------------------------------------------------
results_obj <- pk.nca(data_obj)
print(results_obj)

summary(results_obj)

