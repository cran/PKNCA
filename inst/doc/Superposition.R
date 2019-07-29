## ----setup, echo=FALSE, include=FALSE------------------------------------
library(PKNCA)
library(knitr)
library(ggplot2)
scale_colour_discrete <- scale_colour_hue
scale_fill_discrete <- scale_fill_hue
scale_colour_ordinal <- scale_colour_hue
scale_fill_ordinal <- scale_fill_hue

## ----check-ggplot, include=!requireNamespace("ggplot2"), results="asis"----
cat("ggplot2 is required for this vignette to work correctly.  Please install the ggplot2 library and retry building the vignette.")

## ----showtheoph----------------------------------------------------------
## It is always a good idea to look at the data
knitr::kable(head(datasets::Theoph))

## ----setupconc-----------------------------------------------------------
## By default it is groupedData; convert it to a data frame for use
conc_obj <- PKNCAconc(as.data.frame(datasets::Theoph), conc~Time|Subject)

## ----superposition, error=TRUE-------------------------------------------
steady_state <- superposition(conc_obj, tau=24)

## ----findnonzero---------------------------------------------------------
knitr::kable(subset(datasets::Theoph, Time == 0 & conc > 0),
             caption="Nonzero predose measurements",
             row.names=FALSE)

## ----allownonzero--------------------------------------------------------
## Correct nonzero concentrations at time 0 to be BLQ.
theoph_corrected <- datasets::Theoph
theoph_corrected$conc[theoph_corrected$Time == 0] <- 0
conc_obj_corrected <- PKNCAconc(theoph_corrected, conc~Time|Subject)

## Calculate the new steady-state concentrations with 24 hour dosing
steady_state <- superposition(conc_obj_corrected, tau=24)
knitr::kable(head(steady_state, n=14),
             caption="Superposition at steady-state")

## ----UnsteadyState-------------------------------------------------------
## Calculate the unsteady-state concentrations with 24 hour dosing
unsteady_state <- superposition(conc_obj_corrected, tau=24, n.tau=2)
knitr::kable(head(unsteady_state, n=14),
             caption="Superposition before steady-state")

## ----ComplexInterval-----------------------------------------------------
## Calculate the new steady-state concentrations with 24 hour dosing
complex_interval_steady_state <- superposition(conc_obj_corrected, tau=24, dose.times=c(0, 2, 4))
knitr::kable(head(complex_interval_steady_state, n=10),
             caption="Superposition at steady-state with complex dosing")

## ----ComplexInterval-visualization, eval=requireNamespace("ggplot2")-----
ggplot(complex_interval_steady_state,
       aes(y=conc, x=time, colour=Subject)) +
  geom_line()

## ----to_steady_state-----------------------------------------------------
up_to_steady_state <- superposition(conc_obj_corrected,
                                    tau=4*24,
                                    n.tau=1,
                                    dose.times=seq(0, 3*24, by=12))

## ----to_steady_state-visualization, eval=requireNamespace("ggplot2")-----
ggplot(up_to_steady_state, aes(x=time, y=conc, colour=Subject)) +
  geom_line()

## ----time-point-selection------------------------------------------------
steady_state$time[steady_state$Subject == 1]
sum(steady_state$Subject == 1)
complex_interval_steady_state$time[complex_interval_steady_state$Subject == 1]
sum(complex_interval_steady_state$Subject == 1)

