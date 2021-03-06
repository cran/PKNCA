## ----check-ggplot, include=!requireNamespace("ggplot2"), results="asis"-------
cat("ggplot2 is required for this vignette to work correctly.  Please install the ggplot2 library and retry building the vignette.")

## ----setup--------------------------------------------------------------------
suppressPackageStartupMessages({
  library(PKNCA)
  library(dplyr)
  library(cowplot)
  library(knitr)
  library(ggplot2)
})
scale_colour_discrete <- scale_colour_hue
scale_fill_discrete <- scale_fill_hue

my_conc <- data.frame(conc=c(0, 2.5, 3, 2, 1.5, 1.2, 1.1, 0, 0),
                      time=c(0:5, 8, 12, 24),
                      subject=1)
my_conc$BLQ <- my_conc$conc == 0
my_conc$measured <- TRUE

## ----setup-visualization, eval=requireNamespace("ggplot2")--------------------
ggplot(my_conc,
       aes(x=time,
           y=conc,
           shape=BLQ,
           group=subject)) +
  geom_line() +
  geom_point(size=4) +
  scale_x_continuous(breaks=my_conc$time) +
  theme(legend.position=c(0.8, 0.8))

## ----calc_nca-----------------------------------------------------------------
conc_obj <- PKNCAconc(my_conc, conc~time|subject)
data_obj <- PKNCAdata(data.conc=conc_obj,
                      intervals=data.frame(start=0,
                                           end=24,
                                           aucall=TRUE,
                                           auclast=TRUE,
                                           aucinf.pred=TRUE,
                                           aucinf.obs=TRUE))
results_obj <- pk.nca(data_obj)
kable(as.data.frame(results_obj))

## ----auclast------------------------------------------------------------------
tlast <- pk.calc.tlast(conc=my_conc$conc,
                       time=my_conc$time)
tlast

my_conc$include_auclast <- my_conc$time <= tlast

## ----auclast-visualization, eval=requireNamespace("ggplot2")------------------
ggplot(my_conc,
       aes(x=time,
           y=conc,
           shape=BLQ,
           group=subject)) +
  geom_ribbon(data=my_conc[my_conc$include_auclast,],
              aes(ymin=0, ymax=conc),
              fill="lightblue") +
  geom_line() +
  geom_point(size=4) +
  scale_x_continuous(breaks=my_conc$time) +
  theme(legend.position=c(0.8, 0.8))

## ----aucall-------------------------------------------------------------------
first_after_tlast <- my_conc$time[my_conc$time > tlast][1]
first_after_tlast

my_conc$include_aucall <- my_conc$time <= first_after_tlast

## ----aucall-visualization, eval=requireNamespace("ggplot2")-------------------
ggplot(my_conc,
       aes(x=time,
           y=conc,
           shape=BLQ,
           group=subject)) +
  geom_ribbon(data=my_conc[my_conc$include_aucall,],
              aes(ymin=0, ymax=conc),
              fill="lightblue") +
  geom_line() +
  geom_point(size=4) +
  scale_x_continuous(breaks=my_conc$time) +
  theme(legend.position=c(0.8, 0.8))

## ----aucinf, fig.width=6------------------------------------------------------
# Add one row to illustrate extrapolation beyond observed data
my_conc <-
  rbind(my_conc,
        data.frame(conc=NA,
                   time=36,
                   subject=1,
                   BLQ=NA,
                   measured=FALSE,
                   include_auclast=FALSE,
                   include_aucall=FALSE))
# Extrapolate concentrations for aucinf.obs
my_conc$conc_aucinf.obs <- my_conc$conc
my_conc$conc_aucinf.obs[my_conc$BLQ | is.na(my_conc$BLQ)] <-
  interp.extrap.conc(conc=my_conc$conc,
                     time=my_conc$time,
                     time.out=my_conc$time[my_conc$BLQ | is.na(my_conc$BLQ)],
                     lambda.z=as.data.frame(results_obj)$PPORRES[as.data.frame(results_obj)$PPTESTCD %in% "lambda.z"])

# Extrapolate concentrations for aucinf.pred
my_conc$conc_aucinf.pred <- my_conc$conc
my_conc$conc_aucinf.pred[my_conc$BLQ | is.na(my_conc$BLQ)] <-
  interp.extrap.conc(conc=my_conc$conc,
                     time=my_conc$time,
                     time.out=my_conc$time[my_conc$BLQ | is.na(my_conc$BLQ)],
                     lambda.z=as.data.frame(results_obj)$PPORRES[as.data.frame(results_obj)$PPTESTCD %in% "lambda.z"],
                     clast=as.data.frame(results_obj)$PPORRES[as.data.frame(results_obj)$PPTESTCD %in% "clast.pred"])
my_conc$conc_aucinf.pred[my_conc$time == tlast] <-
  as.data.frame(results_obj)$PPORRES[as.data.frame(results_obj)$PPTESTCD %in% "clast.pred"]

## ----aucinf-visualization, eval=requireNamespace("ggplot2")-------------------
ggplot(my_conc[!is.na(my_conc$conc),],
       aes(x=time,
           y=conc,
           #shape=BLQ,
           group=subject)) +
  geom_ribbon(data=my_conc,
              aes(ymin=0, ymax=conc_aucinf.obs),
              fill="lightblue") +
  geom_line() +
  #geom_point(size=2) +
  scale_x_continuous(breaks=my_conc$time) +
  theme(legend.position=c(0.8, 0.8)) +
  labs(title="Extrapolation using AUCinf,obs")

ggplot(my_conc[!is.na(my_conc$conc),],
       aes(x=time,
           y=conc,
           #shape=BLQ,
           group=subject)) +
  geom_ribbon(
    data=arrange(
      bind_rows(mutate(my_conc,
                       before=FALSE),
                mutate(filter(my_conc, time == tlast),
                       conc_aucinf.pred=conc,
                       before=TRUE)),
      time, desc(before)),
    aes(ymin=0,
        ymax=conc_aucinf.pred),
    fill="lightblue") +
  geom_line() +
  #geom_point(size=2) +
  scale_x_continuous(breaks=my_conc$time) +
  theme(legend.position=c(0.8, 0.8)) +
  labs(title="Extrapolation using AUCinf,pred")

## ----partial_auc_observed-----------------------------------------------------
# Interpolation not required
data_obs_obj <- PKNCAdata(conc_obj, intervals=data.frame(start=0, end=2, auclast=TRUE))
results_obs_obj <- pk.nca(data_obs_obj)
kable(as.data.frame(results_obs_obj))

## ----partial_auc_interpolated-------------------------------------------------
# Interpolation required
my_conc_interp <-
  arrange(
    bind_rows(
      my_conc,
      data.frame(conc=interp.extrap.conc(conc=my_conc$conc, time=my_conc$time, time.out=1.5),
                 time=1.5,
                 subject=1)),
    time)
kable(my_conc_interp)

conc_interp_obj <- PKNCAconc(my_conc_interp, conc~time|subject)
data_interp_obj <- PKNCAdata(conc_interp_obj, intervals=data.frame(start=0, end=1.5, auclast=TRUE))
results_interp <- pk.nca(data_interp_obj)
as.data.frame(results_interp)

