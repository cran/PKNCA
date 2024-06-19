## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PKNCA)

## ----create-units-auto--------------------------------------------------------
d_units_auto <- pknca_units_table(concu="ng/mL", doseu="mg", amountu="mg", timeu="hr")
# Show a selection of the units generated
d_units_auto[d_units_auto$PPTESTCD %in% c("cmax", "tmax", "auclast", "cl.obs", "vd.obs"), ]

## ----create-units-semi-manual-------------------------------------------------
d_units_clean <-
  pknca_units_table(
    concu="ng/mL", doseu="mg", amountu="mg", timeu="hr",
    conversions=
      data.frame(
        PPORRESU=c("mg/(hr*ng/mL)", "mg/(ng/mL)", "hr"),
        PPSTRESU=c("L/hr", "L", "day")
      )
  )
# Show a selection of the units generated
d_units_clean[d_units_clean$PPTESTCD %in% c("cmax", "tmax", "auclast", "cl.obs", "vd.obs"), ]

## ----create-units-manual------------------------------------------------------
d_units_clean_manual <-
  pknca_units_table(
    concu="ng/mL", doseu="mg", amountu="mg", timeu="hr",
    conversions=
      data.frame(
        PPORRESU=c("mg/(hr*ng/mL)", "mg/(ng/mL)", "hr", "ng/mL"),
        PPSTRESU=c("L/hr", "L", "day", "nmol/L"),
        conversion_factor=c(NA, NA, NA, 1000/123)
      )
  )
# Show a selection of the units generated
d_units_clean_manual[d_units_clean_manual$PPTESTCD %in% c("cmax", "tmax", "auclast", "cl.obs", "vd.obs"), ]

## ----create-conc-dose-obj-----------------------------------------------------
conc_obj <- PKNCAconc(as.data.frame(datasets::Theoph), conc~Time|Subject)
d_dose <- unique(datasets::Theoph[datasets::Theoph$Time == 0,
                                  c("Dose", "Time", "Subject")])
dose_obj <- PKNCAdose(d_dose, Dose~Time|Subject)

## ----create-data-obj----------------------------------------------------------
d_units <-
  pknca_units_table(
    concu="mg/L", doseu="mg/kg", timeu="hr",
    # use molar units for concentrations and AUCs
    conversions=
      data.frame(
        PPORRESU=c("(mg/kg)/(hr*mg/L)", "(mg/kg)/(mg/L)", "mg/L", "hr*mg/L"),
        PPSTRESU=c("L/hr/kg", "L/kg", "mmol/L", "hr*mmol/L"),
        conversion_factor=c(NA, NA, 1/180.164, 1/180.164)
      )
  )

data_obj <- PKNCAdata(conc_obj, dose_obj, units=d_units)
result_obj <- pk.nca(data_obj)
summary(result_obj)

## ----different-units-conc-dose-setup------------------------------------------
d_conc_theoph <- as.data.frame(datasets::Theoph)
d_conc_theoph$Analyte <- "Theophylline"
# Approximately 6% of theophylline is metabolized to caffeine
# (https://www.pharmgkb.org/pathway/PA165958541).  Let's pretend that means it
# has 6% of the theophylline concentration at all times.
d_conc_caffeine <- as.data.frame(datasets::Theoph)
d_conc_caffeine$conc <- 0.06*d_conc_caffeine$conc
d_conc_caffeine$Analyte <- "Caffeine"
d_conc <- rbind(d_conc_theoph, d_conc_caffeine)

d_dose <- unique(datasets::Theoph[datasets::Theoph$Time == 0,
                                  c("Dose", "Time", "Subject")])

## ----different-units-units-setup----------------------------------------------
d_units_theoph <-
  pknca_units_table(
    concu="mg/L", doseu="mg/kg", timeu="hr",
    # use molar units for concentrations and AUCs
    conversions=
      data.frame(
        PPORRESU=c("(mg/kg)/(hr*mg/L)", "(mg/kg)/(mg/L)", "mg/L", "hr*mg/L"),
        PPSTRESU=c("L/hr/kg", "L/kg", "mmol/L", "hr*mmol/L"),
        conversion_factor=c(NA, NA, 1/180.164, 1/180.164)
      )
  )
d_units_theoph$Analyte <- "Theophylline"
d_units_caffeine <-
  pknca_units_table(
    concu="mg/L", doseu="mg/kg", timeu="hr",
    # use molar units for concentrations and AUCs
    conversions=
      data.frame(
        PPORRESU=c("(mg/kg)/(hr*mg/L)", "(mg/kg)/(mg/L)", "mg/L", "hr*mg/L"),
        PPSTRESU=c("L/hr/kg", "L/kg", "mmol/L", "hr*mmol/L"),
        conversion_factor=c(NA, NA, 1/194.19, 1/194.19)
      )
  )
d_units_caffeine$Analyte <- "Caffeine"
d_units <- rbind(d_units_theoph, d_units_caffeine)

## ----different-units-calc-----------------------------------------------------
conc_obj <- PKNCAconc(d_conc, conc~Time|Subject/Analyte)
dose_obj <- PKNCAdose(d_dose, Dose~Time|Subject)
data_obj <- PKNCAdata(conc_obj, dose_obj, units=d_units)
result_obj <- pk.nca(data_obj)
summary(result_obj)

