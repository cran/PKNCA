## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(PKNCA)
library(dplyr)

## ----generatecombs, echo=FALSE-------------------------------------------
method.choices <- names(PKNCA:::interp.extrap.conc.dose.select)
method.choices <-
  factor(method.choices, levels=method.choices, ordered=TRUE)
         
all_combs <-
  expand.grid(event_before=setdiff(unlist(PKNCA:::event_choices_interp.extrap.conc.dose), "output_only"),
              event=setdiff(unlist(PKNCA:::event_choices_interp.extrap.conc.dose), "none"),
              event_after=setdiff(unlist(PKNCA:::event_choices_interp.extrap.conc.dose), "output_only"),
              Method="",
              stringsAsFactors=FALSE)

for (n in method.choices) {
  mask <-
    do.call(PKNCA:::interp.extrap.conc.dose.select[[n]]$select,
            list(x=all_combs),
            envir=environment(pk.nca))
  all_combs$Method[mask] <- n
}

all_combs <-
  all_combs[do.call(order,
                    args=append(as.list(all_combs), list(na.last=FALSE))),]

## ----methodsummary, results="asis", echo=FALSE---------------------------
methodorder <- names(sort(summary(factor(all_combs$Method)), decreasing=TRUE))

for (n in methodorder) {
  cat("## ", n, "\n\n", sep="")
  cat(PKNCA:::interp.extrap.conc.dose.select[[n]]$description, "\n\n", sep="")
  print(knitr::kable(
    all_combs[all_combs$Method %in% n,
              c("event_before","event", "event_after")],
    row.names=FALSE,
    col.names=c("Event Before", "Event At", "Event After")))
  cat("\n")
}

## ----methodlisting, echo=FALSE-------------------------------------------
knitr::kable(
  all_combs[,c("event_before", "event", "event_after", "Method")],
    row.names=FALSE,
    col.names=c("Event Before", "Event At", "Event After", "Method Used"))

