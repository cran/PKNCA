## ----knitr-setup, include = FALSE---------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(PKNCA)
library(ggplot2)
auc_methods <- c("lin up/log down", "linear", "lin-log")
auc_types <- c("AUCinf", "AUClast", "AUCall")

comma_and <- function(x) {
  paste(
    paste(x[-length(x)], collapse = ", "),
    ngettext(length(x) - 1, ", and", "and"),
    x[length(x)]
  )
}

## ----setup-example, include=FALSE---------------------------------------------
examples <-
  list(
    "Clast is above the LLOQ" =
      data.frame(
        conc = c(0, 1.8, 3, 2, 1, 0.5, 0.25),
        time = 0:6
      ),
    "Clast is below the LLOQ" =
      data.frame(
        conc = c(0, 1.8, 3, 2, 1, 0.5, 0),
        time = 0:6
      ),
    "Concentrations increase and decrease after T~max~ with zeros in the middle" =
      data.frame(
        conc = c(0, 1.8, 0, 0, 3, 2, 2.5, 0, 0, 2.5, 1, 0.5, 0),
        time = 0:12
      )
  )

## ----example-figures, fig.cap = caption, echo = FALSE-------------------------
caption <- character()
for (current_example_nm in names(examples)) {
  for (current_method in auc_methods) {
    for (current_type in auc_types) {
      caption <-
        c(
          caption,
          sprintf(
            "Example PK where %s; %s interpolation method; %s extrapolation method (AUC type); and keeping all BLQ values (not default BLQ handling)",
            current_example_nm, current_method, current_type
          )
        )
      current_example <- examples[[current_example_nm]]

      # Handle BLQ
      current_example_noblq <- clean.conc.blq(conc = current_example$conc, time = current_example$time)
      current_example_noblq$BLQ <- "Above LLOQ"
      current_example_points <-
        merge(current_example, current_example_noblq, all = TRUE)
      current_example_points$BLQ[is.na(current_example_points$BLQ)] <- "Below LLOQ"
      
      # Calculate some NCA parameters required for interpolation and extrapolation
      tlast <- pk.calc.tlast(conc = current_example$conc, time = current_example$time)
      half_life <- pk.calc.half.life(conc = current_example$conc, time = current_example$time)
      
      # Define the methods used for interpolation and extrapolation
      current_example$method <-
        factor(
          PKNCA:::choose_interval_method(
            conc = current_example$conc,
            time = current_example$time,
            tlast = tlast,
            method = current_method,
            auc.type = current_type,
            options = list()
          ),
          levels = c("zero", "linear", "log", "extrap_log")
        )
      stopifnot(!any(is.na(current_example$method)))
      current_example_interpolate <-
        data.frame(
          time =
            sort(c(
              # make lines almost directly down at the transition after Tlast to
              # zero, where applicable
              current_example$time + sqrt(.Machine$double.eps),
              seq(min(current_example$time), max(current_example$time) + diff(current_example$time)[1], length.out = 200)
            ))
        )
      current_example_interpolate$conc <-
        interp.extrap.conc(
          conc = current_example$conc,
          time = current_example$time,
          time.out = current_example_interpolate$time,
          lambda.z = half_life$lambda.z,
          method = current_method,
          auc.type = current_type,
          conc.blq = "keep"
        )
      current_example$time_after <- c(current_example$time[-1], Inf)
      if (current_example$method[nrow(current_example)] == "zero") {
        current_example_plot <- current_example
      } else {
        current_example_before_tlast <- current_example[current_example$time < tlast, ]
        current_example_after_tlast <- current_example[current_example$time >= tlast, ][1, ]
        current_example_after_tlast$method <- current_example$method[nrow(current_example)]
        current_example_after_tlast$time_after <- Inf
        current_example_plot <-
          dplyr::bind_rows(
            current_example_before_tlast,
            current_example_after_tlast
          )
      }
      print(
        ggplot(current_example_points, aes(x = time, y = conc)) +
          geom_rect(data = current_example_plot, aes(xmin = time, xmax = time_after, ymin = -Inf, ymax = Inf, fill = method), alpha = 0.2) +
          geom_point(aes(shape = BLQ)) +
          geom_line(data = current_example_interpolate) +
          scale_colour_hue(drop = FALSE)
      )
    }
  }
}

