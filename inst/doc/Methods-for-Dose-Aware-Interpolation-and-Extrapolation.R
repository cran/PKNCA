## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(PKNCA)
library(dplyr)

## ----generatecombs, echo=FALSE-------------------------------------------
event.before <-
  data.frame(event.before=c(NA, "conc", "dose", "dose", "both", "both"),
             iv.bolus.before=c(NA, NA, FALSE, TRUE, FALSE, TRUE))
event.at <-
  data.frame(event.at=c(NA, "conc", "dose", "dose", "both", "both"),
             iv.bolus.at=c(NA, NA, FALSE, TRUE, FALSE, TRUE))
event.after <-
  data.frame(event.after=c(NA, "conc", "dose", "both"))
time.request <-
  data.frame(after=c(FALSE, TRUE))

method.choices <- names(PKNCA:::interp.extrap.conc.dose.select)
method.choices <-
  factor(method.choices, levels=method.choices, ordered=TRUE)
         
all.combs <-
  merge(
    merge(
      merge(event.before, event.at),
      event.after),
    time.request)
all.combs$Method <- ""

for (n in method.choices) {
  mask <-
    PKNCA:::interp.extrap.conc.dose.select[[n]]$select(all.combs)
  mask.overlap <- mask &
    !(all.combs$Method %in% "")
  all.combs$Method[mask & ! mask.overlap] <- n
  #all.combs$Method[mask.overlap] <- paste(all.combs$Method[mask.overlap], n, sep="; ")
  #cat(n, sum(mask), "\n")
}

all.combs <-
  all.combs[do.call(order,
                    args=append(as.list(all.combs), list(na.last=FALSE))),]

## ----methodsummary, results="asis", echo=FALSE---------------------------
methodorder <- names(sort(summary(factor(all.combs$Method)), decreasing=TRUE))

for (n in methodorder) {
  cat("## ", n, "\n\n", sep="")
  cat(PKNCA:::interp.extrap.conc.dose.select[[n]]$description, "\n\n", sep="")
  print(knitr::kable(
    all.combs[all.combs$Method %in% n,
              c("event.before", "iv.bolus.before",
                "event.at", "iv.bolus.at", "event.after",
                "after")],
    row.names=FALSE,
    col.names=c("Event Before", "IV Bolus Before", "Event At", "IV Bolus At", "Event After", "Instant After")))
  cat("\n")
}

## ----methodlisting, echo=FALSE-------------------------------------------
knitr::kable(
  all.combs[,c("event.before", "iv.bolus.before",
               "event.at", "iv.bolus.at", "event.after",
               "after", "Method")],
    row.names=FALSE,
    col.names=c("Event Before", "IV Bolus Before",
                "Event At", "IV Bolus At", "Event After",
                "Instant After", "Method Used"))

