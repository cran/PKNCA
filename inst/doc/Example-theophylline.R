## ----setup, echo=FALSE, include=FALSE------------------------------------
library(PKNCA)
library(knitr)

## ----showtheoph----------------------------------------------------------
## It is always a good idea to look at the data
knitr::kable(head(datasets::Theoph))

## ----setupconcdose-------------------------------------------------------
## By default it is groupedData; convert it to a data frame for use
my.conc <- PKNCAconc(as.data.frame(datasets::Theoph), conc~Time|Subject)

## Dosing data needs to only have one row per dose, so subset for
## that first.
d.dose <- unique(datasets::Theoph[datasets::Theoph$Time == 0,
                                  c("Dose", "Time", "Subject")])
knitr::kable(d.dose,
             caption="Example dosing data extracted from theophylline data set")
my.dose <- PKNCAdose(d.dose, Dose~Time|Subject)

## ----autointervals-------------------------------------------------------
my.data.automatic <- PKNCAdata(my.conc, my.dose)
knitr::kable(PKNCA.options("single.dose.aucs"))
knitr::kable(my.data.automatic$intervals)

## ----manualintervals-----------------------------------------------------
my.intervals <- data.frame(start=0,
                           end=Inf,
                           cmax=TRUE,
                           tmax=TRUE,
                           aucinf=TRUE,
                           auclast=TRUE)
my.data.manual <- PKNCAdata(my.conc, my.dose,
                            intervals=my.intervals)
knitr::kable(my.data.manual$intervals)

## ----calculationauto-----------------------------------------------------
my.results.automatic <- pk.nca(my.data.automatic)
knitr::kable(head(my.results.automatic$result))

## ----calculationautoshow, eval=FALSE-------------------------------------
#  summary(my.results.automatic)

## ----calculationautoshowpretty, echo=FALSE-------------------------------
## Make a pretty table instead of the data.frame preformatted printout
knitr::kable(summary(my.results.automatic))

## ----calculationmanual---------------------------------------------------
my.results.manual <- pk.nca(my.data.manual)
knitr::kable(head(my.results.manual$result))

## ----calculationmanualshow, eval=FALSE-----------------------------------
#  summary(my.results.manual)

## ----calculationmanualshowpretty, echo=FALSE-----------------------------
## Make a pretty table instead of the data.frame preformatted printout
knitr::kable(summary(my.results.manual))

