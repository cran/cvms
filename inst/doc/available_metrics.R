## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/vignette_metrics-",
  dpi = 92,
  fig.retina = 2
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----echo=FALSE---------------------------------------------------------------
tb <- table(c(0,1),c(0,1))
tb[[1]] <- "TN"
tb[[2]] <- "FP"
tb[[3]] <- "FN"
tb[[4]] <- "TP"
names(dimnames(tb)) <- c("Prediction", "Target")
tb

