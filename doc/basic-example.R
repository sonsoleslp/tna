## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 10,
  fig.height = 6,
  out.width = "100%",
  dpi = 300,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tna)
library(TraMineR)
data("engagement", package = "tna")
seq_data = seqdef(engagement)

## -----------------------------------------------------------------------------
tna_model <- build.tna(engagement)
plot.tna(tna_model, mar = c(4,4,4,4))

## -----------------------------------------------------------------------------
calculate.centralities(tna_model$Matrix)
plot.centralities(tna_model$Matrix)

## -----------------------------------------------------------------------------
calculate.centralities(tna_model$Matrix0)
plot.centralities(tna_model$Matrix0)

