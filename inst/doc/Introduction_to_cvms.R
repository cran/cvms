## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  dpi = 92,
  fig.retina = 2
)

## ----warning=FALSE, message=FALSE----------------------------------------
library(cvms)
library(groupdata2) # fold()
library(knitr) # kable()
library(dplyr) # %>% arrange()
library(ggplot2)

## ------------------------------------------------------------------------
data <- participant.scores

## ----warning=FALSE, message=FALSE----------------------------------------
# Set seed for reproducibility
set.seed(7)

# Fold data 
data <- fold(data, k = 4,
             cat_col = 'diagnosis',
             id_col = 'participant') %>% 
  arrange(.folds)

# Show first 15 rows of data
data %>% head(15) %>% kable()

## ----warning=FALSE, message=FALSE----------------------------------------
CV1 <- cross_validate(data, "score~diagnosis", 
                     fold_cols = '.folds', 
                     family='gaussian', 
                     REML = FALSE)

# Show results
CV1

# Let's take a closer look at the different parts of the output 

# Results metrics
CV1 %>% select_metrics() %>% kable()

# Nested predictions 
# Note that [[1]] picks predictions for the first row
CV1$Predictions[[1]] %>% head() %>% kable()

# Nested results from the different folds
CV1$Results[[1]] %>% kable()

# Nested model coefficients
# Note that you have the full p-values, 
# but kable() only shows a certain number of digits
CV1$Coefficients[[1]] %>% kable()

# Additional information about the model
# and the training process
CV1 %>% select(11:17) %>% kable()


## ------------------------------------------------------------------------
CV2 <- cross_validate(data, "diagnosis~score", 
                     fold_cols = '.folds', 
                     family='binomial')

# Show results
CV2

# Let's take a closer look at the different parts of the output 
# We won't repeat the parts too similar to those in Gaussian

# Results metrics
CV2 %>% select(1:9) %>% kable()
CV2 %>% select(10:14) %>% kable()

# ROC curve info
CV2$ROC[[1]] %>% head() %>% kable()

# Confusion matrix
CV2$`Confusion Matrix`[[1]] %>% kable()

## ------------------------------------------------------------------------
models <- c("score ~ diagnosis","score~age")
mixed_models <- c("score~diagnosis+(1|session)","score~age+(1|session)")

## ------------------------------------------------------------------------
CV3 <- cross_validate(data, models, 
                     fold_cols = '.folds', 
                     family='gaussian', 
                     REML = FALSE)

# Show results
CV3

## ------------------------------------------------------------------------
CV4 <- cross_validate(data, mixed_models, 
                     fold_cols = '.folds', 
                     family='gaussian', 
                     REML = FALSE)

# Show results
CV4

## ------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(2)

# Fold data 
data <- fold(data, k = 4,
             cat_col = 'diagnosis',
             id_col = 'participant',
             num_fold_cols = 3,
             handle_existing_fold_cols = "keep")

# Show first 15 rows of data
data %>% head(10) %>% kable()


## ------------------------------------------------------------------------
CV5 <- cross_validate(data, "diagnosis ~ score", 
                     fold_cols = paste0(".folds_", 1:4), 
                     family='binomial', 
                     REML = FALSE)

# Show results
CV5

# The binomial output now has a nested 'Results' tibble
# Let's see a subset of the columns
CV5$Results[[1]] %>% select(1:8) %>%  kable()

## ------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(1)

# Partition the dataset 
partitions <- groupdata2::partition(participant.scores, 
                                    p = 0.7,
                                    cat_col = 'diagnosis',
                                    id_col = 'participant',
                                    list_out = TRUE)
train_set <- partitions[[1]]
test_set <- partitions[[2]]

## ------------------------------------------------------------------------
baseline(test_data = test_set, train_data = train_set,
         n = 100, dependent_col = "score", family = "gaussian")

## ------------------------------------------------------------------------
baseline(test_data = test_set, n = 100, 
         dependent_col = "diagnosis", family = "binomial")

## ------------------------------------------------------------------------
cv_plot(CV1, type = "RMSE") +
  theme_bw()
cv_plot(CV1, type = "r2") +
  theme_bw()
cv_plot(CV1, type = "IC") +
  theme_bw()
cv_plot(CV1, type = "coefficients") +
  theme_bw()

## ------------------------------------------------------------------------
cv_plot(CV2, type = "ROC") +
  theme_bw()

## ------------------------------------------------------------------------
combine_predictors(dependent = "y",
                   fixed_effects = c("a","b","c"),
                   random_effects = "(1|d)")

## ------------------------------------------------------------------------
combine_predictors(dependent = "y",
                   fixed_effects = list("a", list("b","log_b")),
                   random_effects = "(1|d)")

