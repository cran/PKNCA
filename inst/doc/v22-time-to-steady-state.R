## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(PKNCA)
library(knitr)
library(ggplot2)
scale_colour_discrete <- scale_colour_hue
scale_fill_discrete <- scale_fill_hue
scale_colour_ordinal <- scale_colour_hue
scale_fill_ordinal <- scale_fill_hue

## ----superposition, error=TRUE------------------------------------------------
library(PKNCA)
theoph_corrected <- as.data.frame(datasets::Theoph)
theoph_corrected$conc[theoph_corrected$Time == 0] <- 0
conc_obj <- PKNCAconc(theoph_corrected, conc~Time|Subject)
steady_state <- superposition(conc_obj, dose.times = seq(0, 168 - 12, by=12), tau=168, n.tau=1)
# Add some noise to the data so that it seems more reasonable
steady_state_noise <- steady_state
steady_state_noise$conc <-
  withr::with_seed(
    seed = 5,
    steady_state_noise$conc*exp(rnorm(nrow(steady_state_noise), mean = 0, sd = 0.1))
  )

## ----superposition-fig--------------------------------------------------------
library(ggplot2)
ggplot(steady_state_noise, aes(x=time, y=conc, groups=Subject)) + geom_line()

## ----tss-mono-----------------------------------------------------------------
tss_mono <-
  pk.tss.monoexponential(
    conc = steady_state_noise$conc,
    time = steady_state_noise$time,
    subject = steady_state_noise$Subject,
    time.dosing = seq(0, 168 - 12, by=12)
  )
tss_mono

## ----tss-step-----------------------------------------------------------------
tss_step <-
  pk.tss.stepwise.linear(
    conc = steady_state_noise$conc,
    time = steady_state_noise$time,
    subject = steady_state_noise$Subject,
    time.dosing = seq(0, 168 - 12, by=12)
  )
tss_step

