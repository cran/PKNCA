## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
requireNamespace("pmxTools")
library(PKNCA)
library(dplyr)
library(ggplot2)
breaks_hours <- function(n=5, Q=c(1, 6, 4, 12, 2, 24, 168), ...) {
  n_default <- n
  Q_default <- Q
  function(x, n = n_default, Q=Q_default) {
    x <- x[is.finite(x)]
    if (length(x) == 0) {
      return(numeric())
    }
    rng <- range(x)
    labeling::extended(rng[1], rng[2], m=n, Q=Q, ...)
  }
}

scale_x_hours <- function(..., breaks=breaks_hours()) {
  ggplot2::scale_x_continuous(..., breaks=breaks)
}

## ----fig.width=6, fig.height=4------------------------------------------------
conc <-
  datasets::Theoph %>%
  filter(Subject %in% 1)
ggplot(conc, aes(x=Time, y=conc)) +
  geom_line() +
  scale_x_hours()

## -----------------------------------------------------------------------------
conc_data <-
  withr::with_seed(5, {
    data.frame(
      Subject=rep(1:2, each=6),
      Treatment=rep(c("A", "B", "A", "B"), each=3),
      Time=rep(c(0, 2, 8), 4),
      Conc=rep(c(0, 2, 0.5), 4)*exp(rnorm(n=12, sd=0.05))
    )
  })

## -----------------------------------------------------------------------------
pander::pander(conc_data %>% filter(Subject == 1))

## -----------------------------------------------------------------------------
pander::pander(conc_data %>% filter(Subject == 2))

## -----------------------------------------------------------------------------
dose_data <-
  data.frame(
    Subject=rep(1:2, each=2),
    Treatment=c("A", "B", "A", "B"),
    Time=0,
    Dose=10
  )

## -----------------------------------------------------------------------------
pander::pander(dose_data %>% filter(Subject == 1))

## -----------------------------------------------------------------------------
pander::pander(dose_data %>% filter(Subject == 2))

## ---- echo=TRUE---------------------------------------------------------------
d_interval_1 <-
  data.frame(
    start=0, end=8,
    cmax=TRUE, tmax=TRUE, auclast=TRUE
  )

## -----------------------------------------------------------------------------
pander::pander(d_interval_1)

## ----echo=TRUE----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(PKNCA)
# Concentration data setup
d_conc <-
  datasets::Theoph %>%
  filter(Subject %in% 1)
o_conc <- PKNCAconc(conc~Time, data=d_conc)
# Setup intervals for calculation
d_intervals <- data.frame(start=0, end=24, cmax=TRUE, tmax=TRUE, auclast=TRUE, aucint.inf.obs=TRUE)
# Combine concentration and dose
o_data <- PKNCAdata(o_conc, intervals=d_intervals)
# Calculate the results (suppressMessages() hides a message that isn't needed now)
o_result <- suppressMessages(pk.nca(o_data))
# summary(o_result)

## ----echo=TRUE----------------------------------------------------------------
# Concentration data setup
d_conc <-
  datasets::Theoph %>%
  filter(Subject %in% 1)
o_conc <- PKNCAconc(conc~Time, data=d_conc)
# Dose data setup
d_dose <-
  datasets::Theoph %>%
  filter(Subject %in% 1) %>%
  filter(Time == 0)
o_dose <- PKNCAdose(Dose~Time, data=d_dose)
# Combine concentration and dose
o_data <- PKNCAdata(o_conc, o_dose)
# Calculate the results
o_result <- pk.nca(o_data)

## ----echo=TRUE----------------------------------------------------------------
# Load your dataset as a data.frame
d_conc <-
  datasets::Theoph %>%
  filter(Subject %in% 1)
# Take a look at the data
pander::pander(head(d_conc, 2))
# Define the PKNCAconc object indicating the concentration and time columns, the
# dataset, and any other options.
o_conc <- PKNCAconc(conc~Time, data=d_conc)

## ----echo=TRUE----------------------------------------------------------------
# Load your dataset as a data.frame
d_dose <-
  datasets::Theoph %>%
  filter(Subject %in% 1) %>%
  filter(Time == 0)
# Take a look at the data
pander::pander(d_dose)
# Define the PKNCAdose object indicating the dose amount and time columns, the
# dataset, and any other options.
o_dose <- PKNCAdose(Dose~Time, data=d_dose)

## ----echo=TRUE----------------------------------------------------------------
# Combine the PKNCAconc and PKNCAdose objects.  You can add interval
# specifications and calculation options here.
o_data <- PKNCAdata(o_conc, o_dose)
# Calculate the results
o_result <- pk.nca(o_data)

## ----echo=TRUE----------------------------------------------------------------
# Look at summarized results
pander::pander(summary(o_result))

## -----------------------------------------------------------------------------
# Look at individual results
pander::pander(head(
  as.data.frame(o_result),
  n=3
))

## ----interval-vs-groups-setup, echo=FALSE-------------------------------------
last_dose_time <- 24
dose_interval <- 8
dose_times <- seq(0, last_dose_time-dose_interval, by=dose_interval)
d_conc_superposition <-
  superposition(
    o_conc,
    dose.times=dose_times,
    tau=last_dose_time,
    check.blq=FALSE,
    n.tau=1
  )

## ----fig.width=4, fig.height=4------------------------------------------------
d_intervals <-
  tibble(
    start=dose_times,
    end=dose_times + dose_interval
  ) %>%
  mutate(
    name=sprintf("Interval %g", row_number()),
    height=max(d_conc_superposition$conc)*1.03,
    width=dose_interval,
    x=(start+end)/2,
    y=height/2
  )
d_interval_arrows <-
  d_conc_superposition %>%
  filter(time != 0 & time %in% dose_times) %>%
  mutate(
    name1=sprintf("Interval %g", row_number()),
    name2=sprintf("Interval %g", row_number() + 1),
  )
ggplot(d_conc_superposition, aes(x=time, y=conc)) +
  geom_tile(
    data=d_intervals,
    aes(x=x, y=y, width=width, height=height, colour=name, fill=name),
    alpha=0.2,
    inherit.aes=FALSE,
    show.legend=FALSE
  ) +
  geom_segment(
    data=d_interval_arrows,
    aes(x=time - 0.8, xend=time - 0.1, y=conc-2.1, yend=conc - 0.1, colour=name2),
    arrow=arrow(length=unit(0.1, "inches")),
    inherit.aes=FALSE,
    show.legend=FALSE
  ) +
  geom_segment(
    data=d_interval_arrows,
    aes(x=time + 0.8, xend=time + 0.1, y=conc-2.1, yend=conc - 0.1, colour=name1),
    arrow=arrow(length=unit(0.1, "inches")),
    inherit.aes=FALSE,
    show.legend=FALSE
  ) +
  geom_line() +
  geom_point() +
  scale_x_hours() +
  labs(
    title=sprintf("Dosing Q%gH", dose_interval)
  )

## -----------------------------------------------------------------------------
PKNCA.options("single.dose.aucs") %>%
  select(c(all_of(c("start", "end")), where(~is.logical(.x) && any(.x)))) %>%
  pander::pander()

## ----echo=TRUE----------------------------------------------------------------
d_conc <-
  datasets::Theoph %>%
  mutate(
    Treatment=
      case_when(
        Dose <= median(Dose)~"Low dose",
        TRUE~"High dose"
      )
  )
# The study was single-dose
d_dose <-
  d_conc %>%
  select(Treatment, Subject, Dose) %>%
  unique() %>%
  mutate(dose_time=0)

## ----echo=TRUE----------------------------------------------------------------
o_conc <- PKNCAconc(conc~Time|Treatment+Subject, data=d_conc)
try({
  o_data <- PKNCAdata(o_conc)
  summary(pk.nca(o_data))
})

## ----echo=TRUE----------------------------------------------------------------
o_conc <- PKNCAconc(conc~Time|Treatment+Subject, data=d_conc)
d_intervals <- data.frame(start=0, end=Inf, cmax=TRUE, tmax=TRUE, half.life=TRUE, aucinf.obs=TRUE)
o_data_manual_intervals <- PKNCAdata(o_conc, intervals=d_intervals)
summary(pk.nca(o_data_manual_intervals))

## ----echo=TRUE----------------------------------------------------------------
o_conc <- PKNCAconc(conc~Time|Treatment+Subject, data=d_conc)
o_dose <- PKNCAdose(Dose~dose_time|Treatment+Subject, data=d_dose)
o_data_auto_intervals <- PKNCAdata(o_conc, o_dose)
o_data_auto_intervals$intervals$aucint.inf.obs <- TRUE
summary(pk.nca(o_data_auto_intervals))

## ----auc-considerations-setup, warning=FALSE----------------------------------
d_conc <-
  datasets::Theoph %>%
  filter(Subject == 1)
o_conc <- PKNCAconc(conc~Time, data=d_conc)
d_interval_int <- data.frame(start=0, end=Inf, half.life=TRUE)
o_data_int <- PKNCAdata(o_conc, intervals=d_interval_int)
o_nca_int <- suppressMessages(pk.nca(o_data_int))
lambda_z_int <-
  o_nca_int %>%
  as.data.frame() %>%
  filter(PPTESTCD %in% "lambda.z") %>%
  "[["("PPORRES")

d_interval_inf <- data.frame(start=0, end=24, half.life=TRUE)
o_data_inf <- PKNCAdata(o_conc, intervals=d_interval_inf)
o_nca_inf <- suppressMessages(pk.nca(o_data_inf))
lambda_z_inf <-
  o_nca_inf %>%
  as.data.frame() %>%
  filter(PPTESTCD %in% "lambda.z") %>%
  "[["("PPORRES")

tlast <- 
  o_nca_inf %>%
  as.data.frame() %>%
  filter(PPTESTCD %in% "tlast") %>%
  "[["("PPORRES")

d_auc_calcs <-
  d_conc %>%
  bind_rows(
    tibble(Time=seq(12, 60))
  ) %>%
  mutate(
    conc_all_int=
      interp.extrap.conc(
        conc=conc[!is.na(conc)],
        time=Time[!is.na(conc)],
        time.out=Time,
        lambda.z=lambda_z_int
      ),
    conc_all_inf=
      interp.extrap.conc(
        conc=conc[!is.na(conc) & Time <= 24],
        time=Time[!is.na(conc) & Time <= 24],
        time.out=Time,
        lambda.z=lambda_z_inf
      ),
    conc_last=
      case_when(
        Time <= 24~conc,
        TRUE~NA_real_
      ),
    conc_int=
      case_when(
        Time <= 24 & Time >= tlast~conc_all_int,
        TRUE~NA_real_
      ),
    conc_inf=
      case_when(
        Time >= tlast~conc_all_inf,
        TRUE~NA_real_
      )
  ) %>%
  arrange(Time)
auc_figure_time_max <- 36
p_auc_calcs <-
  ggplot(d_auc_calcs, aes(x=Time, y=conc)) +
  # AUCinf (with a work-around for https://github.com/tidyverse/ggplot2/issues/4661)
  geom_area(
    data=d_auc_calcs %>% filter(Time <= auc_figure_time_max),
    aes(y=conc_inf, colour="AUCinf", fill="AUCinf"),
    alpha=0.2,
    na.rm=TRUE
  ) +
  geom_line(
    data=d_auc_calcs,
    aes(y=conc_inf, colour="AUCinf"),
    na.rm=TRUE
  ) +
  # AUCint
  geom_area(
    aes(y=conc_int, colour="AUCint", fill="AUCint"),
    alpha=0.2,
    na.rm=TRUE
  ) +
  # AUClast
  geom_area(
    aes(y=conc_last, colour="AUClast", fill="AUClast"),
    na.rm=TRUE
  ) +
  geom_point(show.legend=FALSE,
    na.rm=TRUE) +
  geom_line(show.legend=FALSE,
    na.rm=TRUE) +
  geom_vline(xintercept=24, linetype="63") +
  scale_x_continuous(breaks=seq(0, auc_figure_time_max, by=6)) +
  coord_cartesian(xlim=c(0, auc_figure_time_max)) +
  labs(
    colour="AUC type",
    fill="AUC type"
  )

## ----warning=FALSE, out.width="100%"------------------------------------------
p_auc_calcs

## ----warning=FALSE, out.width="100%"------------------------------------------
p_auc_calcs

## ----warning=FALSE, out.width="100%"------------------------------------------
p_auc_calcs

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(PKNCA)
#  
#  d_conc <- read.csv("c:/tmp/whale_conc.csv")
#  d_dose <- read.csv("c:/tmp/whale_dose.csv")
#  head(d_conc)
#  head(d_dose)
#  
#  o_conc <- PKNCAconc(concentration~time|Animal, data=d_conc)
#  o_dose <- PKNCAdose(dose~time|Animal, data=d_dose)
#  o_data <- PKNCAdata(o_conc, o_dose)
#  o_data$intervals
#  o_nca <- pk.nca(o_data)
#  summary(o_nca)
#  summary(o_nca, drop.group=c())
#  as.data.frame(o_nca)

## ----exclude-example-1, echo=TRUE---------------------------------------------
d_before_exclude <-
  data.frame(
    time=0:4,
    conc=c(0, 2, 1, 0.5, 0.25),
    not_this=c(NA, "Not this", NA, NA, NA)
  )
o_conc <-
  PKNCAconc(
    data=d_before_exclude,
    conc~time,
    exclude="not_this"
  )

## ----exclude-example-2, echo=TRUE---------------------------------------------
pander::pander(
  d_before_exclude %>%
    filter(is.na(not_this))
)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  o_conc <- PKNCAconc(data=d_before_exclude, conc~time, exclude="not_this")

## ----echo=TRUE----------------------------------------------------------------
d_urine <-
  data.frame(
    conc=c(1, 2, 3),
    urine_volume=c(200, 100, 300),
    time=c(1, 2, 3)
  )
o_conc <- PKNCAconc(data=d_urine, conc~time, volume="urine_volume")
d_intervals <- data.frame(start=0, end=24, ae=TRUE)
o_data <- PKNCAdata(o_conc, intervals=d_intervals)
o_nca <- suppressMessages(pk.nca(o_data))
summary(o_nca)

## ----echo=TRUE----------------------------------------------------------------
o_conc <- PKNCAconc(data=data.frame(conc=2^-(1:4), time=0:3), conc~time)
o_data <- PKNCAdata(o_conc, intervals=data.frame(start=0, end=Inf, cmax=TRUE))
o_nca <- suppressMessages(pk.nca(o_data))
as.data.frame(o_nca)

## ----echo=TRUE----------------------------------------------------------------
o_data <-
  PKNCAdata(
    o_conc,
    intervals=
      data.frame(
        start=0, end=Inf,
        aucinf.obs=TRUE
      )
  )
o_nca <- suppressMessages(pk.nca(o_data))

## ----echo=TRUE----------------------------------------------------------------
as.data.frame(o_nca)

## ----fig.height=3-------------------------------------------------------------
d_prep <-
  datasets::Theoph %>%
  filter(Subject == 1) %>%
  mutate(
    conc=
      case_when(
        Time == 0~0,
        TRUE~conc
      )
  )
d_plot <-
  superposition(
    conc=d_prep$conc,
    time=d_prep$Time,
    tau=48,
    n.tau=1,
    dose.times=24*(0:1)
  ) %>%
  # Pretend that we missed the predose sample
  filter(
    !between(time, 23, 24.5)
  )
# Pretend that there was a dose at 24
ggplot(d_plot, aes(x=time, y=conc)) +
  geom_point() + geom_line() +
  geom_vline(xintercept=24) +
  scale_x_hours()

## ----exclude-setup------------------------------------------------------------
o_conc <- PKNCAconc(data=data.frame(conc=2^-(1:4), time=0:3), conc~time)
o_data <-
  PKNCAdata(
    o_conc,
    intervals=
      data.frame(
        start=0, end=Inf,
        aucinf.obs=TRUE
      )
  )
o_nca <- suppressMessages(pk.nca(o_data))

## ----exclude-not-best, echo=TRUE----------------------------------------------
as.data.frame(o_nca) %>%
  filter(PPTESTCD != "half.life")

## ----exclude-best, echo=TRUE--------------------------------------------------
o_nca_excluded <-
  o_nca %>%
  exclude(FUN=exclude_nca_span.ratio(3))
as.data.frame(o_nca_excluded)

## ----echo=TRUE----------------------------------------------------------------
summary(o_nca)
summary(o_nca_excluded)

## ----echo=TRUE----------------------------------------------------------------
# Subject 2 is selected for a BLQ time=0 concentration
d_prep <-
  datasets::Theoph %>%
  filter(Subject == 2)
# Superposition to steady-state is the default
d_ss <-
  superposition(
    conc=d_prep$conc,
    time=d_prep$Time,
    tau=24
  )
# Going to steady-state is also an option
# (n.tau=2 means the second dose)
d_second_dose <-
  superposition(
    conc=d_prep$conc,
    time=d_prep$Time,
    tau=24,
    n.tau=2
  )

## ----echo=TRUE----------------------------------------------------------------
# Want the profile for the first two doses
# together?
d_first_two <-
  superposition(
    conc=d_prep$conc,
    time=d_prep$Time,
    tau=48, # 48 hours
    n.tau=1, # One tau interval (0 to 48 hours)
    dose.times=c(0, 24)
  )

## ---- fig.height=3, fig.width=3-----------------------------------------------
ggplot(d_ss, aes(x=time, y=conc)) +
  geom_point() + geom_line() +
  scale_y_continuous(limits=c(0, NA))

## ----echo=TRUE----------------------------------------------------------------
dose_times <- seq(0, 96-1, by=6)
d_multidose <-
  superposition(
    conc=d_prep$conc,
    time=d_prep$Time,
    tau=96, # 48 hours
    n.tau=1, # One tau interval (0 to 48 hours)
    dose.times=dose_times
  )
pk.tss.monoexponential(
  conc=d_multidose$conc, time=d_multidose$time, subject=rep(1, nrow(d_multidose)),
  time.dosing=dose_times, subject.dosing=rep(1, length(dose_times)),
  output="single"
)

## ----echo=TRUE----------------------------------------------------------------
o_conc <- PKNCAconc(conc~Time|Subject, data=datasets::Theoph)
d_plot <-
  grouped_df(data=datasets::Theoph, vars=names(getGroups(o_conc))) %>%
  nest() %>%
  mutate(
    figure=
      lapply(
        pmap(
          .l=list(data=data),
          .f=ggplot,
          aes(x=Time, y=conc)
        ),
        FUN="+",
        geom_line()
      )
  )
# d_plot$figure

## ----echo=TRUE----------------------------------------------------------------
pander::pander(summary(o_nca))

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  pander::pander(as.data.frame(o_nca))

## ---- echo=TRUE---------------------------------------------------------------
d_conc <-
  datasets::Theoph %>%
  rename(time=Time) %>%
  mutate(
    Subject=as.character(Subject)
  )
d_multidose <-
  PKNCAconc(conc~time|Subject, data=d_conc) %>%
  superposition(tau=24, check.blq=FALSE)
d_singledose_single_analyte <-
  d_conc %>%
  mutate(
    Study_Part="Single"
  )
d_multidose_single_analyte <-
  d_conc %>%
  mutate(Day=1) %>%
  bind_rows(
    d_multidose %>% mutate(time=time + 120, Day=6)
  ) %>%
  mutate(
    Study_Part="Multiple"
  )

## ---- echo=TRUE---------------------------------------------------------------
d_single_multi_conc <- bind_rows(d_singledose_single_analyte, d_multidose_single_analyte)
d_single_multi_dose <-
  d_single_multi_conc %>%
  filter(
    (Study_Part %in% "Single" & time == 0) |
      (Study_Part %in% "Multiple" & (time %% 24) == 0)
  )

## ---- echo=TRUE---------------------------------------------------------------
o_conc <- PKNCAconc(data=d_single_multi_conc, conc~time|Study_Part+Subject)
o_dose <- PKNCAdose(data=d_single_multi_dose, Dose~time|Study_Part+Subject)
o_data <- PKNCAdata(o_conc, o_dose)
o_data$intervals %>% select(-Subject) %>% unique() %>% as.data.frame()
o_nca <- pk.nca(o_data)

## ----echo=TRUE----------------------------------------------------------------
d_intervals <-
  data.frame(
    start=0,
    end=24,
    Subject=c("1", "2"),
    Study_Part="Single",
    aucinf.obs=TRUE
  )
o_data <- PKNCAdata(o_conc, o_dose, intervals=d_intervals)
o_nca <- pk.nca(o_data)
summary(o_nca)

## ----echo=TRUE----------------------------------------------------------------
# Find the time closest to 12 hours
d_intervals_prep <-
  d_single_multi_conc %>%
  filter(Study_Part == "Single") %>%
  mutate(
    time_deviation=abs(time-12)
  ) %>%
  group_by(Subject, Study_Part) %>%
  filter(time %in% time[time_deviation == min(time_deviation)])
d_intervals <-
  d_intervals_prep %>%
  select(Study_Part, Subject, end=time) %>%
  mutate(
    start=0,
    aucinf.obs=TRUE
  )
o_data <- PKNCAdata(o_conc, o_dose, intervals=d_intervals)

o_nca <- pk.nca(o_data)
summary(o_nca, drop.group=c("Subject", "end"))

## ---- echo=TRUE---------------------------------------------------------------
d_single_multi_conc_multi_analyte <-
  bind_rows(
    d_single_multi_conc %>% mutate(Analyte="Parent"),
    d_single_multi_conc %>%
      mutate(
        Analyte="Metabolite",
        conc=conc/2
      )
  )
o_conc <-
  PKNCAconc(
    data=d_single_multi_conc_multi_analyte,
    conc~time|Study_Part+Subject/Analyte
  )
o_dose <- PKNCAdose(data=d_single_multi_dose, Dose~time|Study_Part+Subject)
o_data <- PKNCAdata(o_conc, o_dose)
o_nca <- pk.nca(o_data)
summary(o_nca)

