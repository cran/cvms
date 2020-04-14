## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/vignette_conf_mat-",
  dpi = 92,
  fig.retina = 2
)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(cvms)
library(knitr)  # kable()
library(dplyr)
set.seed(74)

# Prepare dataset
data <- participant.scores %>% as_tibble()
# Add probabilities and predicted classes
data[["probability"]] <- runif(nrow(data))
data[["predicted diagnosis"]] <- ifelse(data[["probability"]] > 0.5, 1, 0)

data %>% head(10) %>% kable()


## -----------------------------------------------------------------------------
ev <- evaluate(
  data = data,
  target_col = "diagnosis",
  prediction_cols = "probability",
  id_col = "participant",
  id_method = "mean",
  type = "binomial"
)

ev


## -----------------------------------------------------------------------------
ev$Predictions[[1]] %>% kable()


## ----fig.width=4, fig.height=4, fig.align='center'----------------------------
# Note: If ev had multiple rows, we would have to 
# pass ev$`Confusion Matrix`[[1]] to 
# plot the first row's confusion matrix
plot_confusion_matrix(ev)


## -----------------------------------------------------------------------------
ev_metrics <- select_metrics(ev)
ev_metrics %>% select(1:9) %>% kable(digits = 5)
ev_metrics %>% select(10:14) %>% kable(digits = 5)


## -----------------------------------------------------------------------------
ev_2 <- evaluate(
  data = data,
  target_col = "diagnosis",
  prediction_cols = "probability",
  id_col = "participant",
  id_method = "majority",
  type = "binomial"
)

ev_2


## -----------------------------------------------------------------------------
ev_2$Predictions[[1]] %>% kable()


## -----------------------------------------------------------------------------
# Duplicate data frame
data_2 <- data
# Change the probabilities and predicted classes
data_2[["probability"]] <- runif(nrow(data))
data_2[["predicted diagnosis"]] <- ifelse(data_2[["probability"]] > 0.5, 1, 0)

# Combine the two data frames
data_multi <- dplyr::bind_rows(data, data_2, .id = "model")

data_multi


## -----------------------------------------------------------------------------
ev_3 <- data_multi %>%
  dplyr::group_by(model) %>%
  evaluate(
    target_col = "diagnosis",
    prediction_cols = "probability",
    id_col = "participant",
    id_method = "mean",
    type = "binomial"
  )

ev_3


## -----------------------------------------------------------------------------
ev_3$Predictions[[2]] %>% kable()


## -----------------------------------------------------------------------------
data[["predicted age"]] <- sample(20:45, size = 30, replace = TRUE)


## -----------------------------------------------------------------------------
ev_4 <- evaluate(
  data = data,
  target_col = "age",
  prediction_cols = "predicted age",
  id_col = "participant",
  id_method = "mean",
  type = "gaussian"
)

ev_4


## -----------------------------------------------------------------------------
ev_4$Predictions[[1]] %>% kable()


## -----------------------------------------------------------------------------
data[["predicted score"]] <- round(runif(30, 10, 81))


## -----------------------------------------------------------------------------
data %>% 
  dplyr::group_by(participant) %>% 
  evaluate(
    target_col = "score",
    prediction_cols = "predicted score",
    type = "gaussian"
  )


## -----------------------------------------------------------------------------
# Extract the ~20% observations with highest prediction error
most_challenging(
  data = data,
  type = "gaussian",
  obs_id_col = "participant",
  target_col = "score",
  prediction_cols = "predicted score",
  threshold = 0.20
)

