## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
suppressPackageStartupMessages(
  library(PKNCA)
)

## ----create-conc-dose-obj-auto------------------------------------------------
d_conc <- as.data.frame(datasets::Theoph)
d_conc$concu_col <- "mg/L"
d_conc$timeu_col <- "hr"
d_dose <- datasets::Theoph[datasets::Theoph$Time == 0, c("Dose", "Time", "Subject")]
d_dose$doseu_col <- "mg/kg"
o_conc <- PKNCAconc(d_conc, conc~Time|Subject, concu = "concu_col", timeu = "timeu_col")
o_dose <- PKNCAdose(d_dose, Dose~Time|Subject, doseu = "doseu_col")

## ----create-data-obj-auto-----------------------------------------------------
o_data <- PKNCAdata(o_conc, o_dose)
o_nca <- pk.nca(o_data)
summary(o_nca)

## ----create-auto-direct-------------------------------------------------------
d_conc <- as.data.frame(datasets::Theoph)
d_dose <- datasets::Theoph[datasets::Theoph$Time == 0, c("Dose", "Time", "Subject")]
o_conc <- PKNCAconc(d_conc, conc~Time|Subject, concu = "mg/L", timeu = "hr")
o_dose <- PKNCAdose(d_dose, Dose~Time|Subject, doseu = "mg/kg")
o_data <- PKNCAdata(o_conc, o_dose)
o_nca <- pk.nca(o_data)
summary(o_nca)

## ----create-auto-direct-pref--------------------------------------------------
d_conc <- as.data.frame(datasets::Theoph)
d_dose <- datasets::Theoph[datasets::Theoph$Time == 0, c("Dose", "Time", "Subject")]
o_conc <- PKNCAconc(d_conc, conc~Time|Subject, concu = "mg/L", timeu = "hr", concu_pref = "ug/L", timeu_pref = "day")
o_dose <- PKNCAdose(d_dose, Dose~Time|Subject, doseu = "mg/kg")
o_data <- PKNCAdata(o_conc, o_dose)
o_nca <- pk.nca(o_data)
summary(o_nca)

## ----create-conc-dose-obj-----------------------------------------------------
o_conc <- PKNCAconc(as.data.frame(datasets::Theoph), conc~Time|Subject)
d_dose <- datasets::Theoph[datasets::Theoph$Time == 0, c("Dose", "Time", "Subject")]
o_dose <- PKNCAdose(d_dose, Dose~Time|Subject)

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

o_data <- PKNCAdata(o_conc, o_dose, units=d_units)
o_nca <- pk.nca(o_data)
summary(o_nca)

## ----create-units-auto--------------------------------------------------------
d_units_auto <- pknca_units_table(concu="ng/mL", doseu="mg", amountu="mg", timeu="hr")
# Show a selection of the units generated
d_units_auto[d_units_auto$PPTESTCD %in% c("cmax", "tmax", "auclast", "cl.obs", "vd.obs"), ]

## ----create-units-semi-manual-------------------------------------------------
d_units_clean <-
  pknca_units_table(
    concu="ng/mL", doseu="mg", amountu="ng", timeu="hr",
    conversions=
      data.frame(
        PPORRESU=c("mg/(hr*ng/mL)", "mg/(ng/mL)", "hr", "ng/mg"),
        PPSTRESU=c("L/hr", "L", "day", "fraction")
      )
  )
# Show a selection of the units generated
d_units_clean[d_units_clean$PPTESTCD %in% c("cmax", "tmax", "auclast", "cl.obs", "vd.obs", "fe"), ]

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
o_conc <- PKNCAconc(d_conc, conc~Time|Subject/Analyte)
o_dose <- PKNCAdose(d_dose, Dose~Time|Subject)
o_data <- PKNCAdata(o_conc, o_dose, units=d_units)
o_nca <- pk.nca(o_data)
summary(o_nca)

