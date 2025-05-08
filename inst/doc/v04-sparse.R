## -----------------------------------------------------------------------------
# Setup the data
d_sparse <-
    data.frame(
      id = c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 4L, 5L, 6L, 4L, 5L, 6L, 7L, 8L, 9L, 7L, 8L, 9L),
      conc = c(0, 0, 0,  1.75, 2.2, 1.58, 4.63, 2.99, 1.52, 3.03, 1.98, 2.22, 3.34, 1.3, 1.22, 3.54, 2.84, 2.55, 0.3, 0.0421, 0.231),
      time = c(0, 0, 0, 1, 1, 1, 6, 6, 6, 2, 2, 2, 10, 10, 10, 4, 4, 4, 24, 24, 24),
      dose = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
    )

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(d_sparse, aes(x=time, y=conc, group=id)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(0, 24, by=6))

## ----eval=FALSE---------------------------------------------------------------
# d_sparse$id <- 1:nrow(d_sparse)

## -----------------------------------------------------------------------------
library(PKNCA)
o_conc_sparse <- PKNCAconc(d_sparse, conc~time|id, sparse=TRUE)
d_intervals <-
  data.frame(
    start=0,
    end=24,
    aucinf.obs=TRUE,
    cmax=TRUE,
    sparse_auclast=TRUE
  )
o_data_sparse <- PKNCAdata(o_conc_sparse, intervals=d_intervals)
o_nca <- pk.nca(o_data_sparse)

## -----------------------------------------------------------------------------
summary(o_nca)

## -----------------------------------------------------------------------------
as.data.frame(o_nca)

