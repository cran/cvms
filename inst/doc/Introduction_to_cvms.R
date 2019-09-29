## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/vignette_intro-",
  dpi = 92,
  fig.retina = 2
)

## ----warning=FALSE, message=FALSE----------------------------------------
library(cvms)
library(groupdata2) # fold() partition()
library(knitr) # kable()
library(dplyr) # %>% arrange()
library(ggplot2)

## ------------------------------------------------------------------------
data <- participant.scores

## ------------------------------------------------------------------------
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
                      family = 'gaussian',
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
                      family = 'binomial')

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
models <- c("score~diagnosis", "score~age")
mixed_models <- c("score~diagnosis+(1|session)", "score~age+(1|session)")

## ------------------------------------------------------------------------
CV3 <- cross_validate(data, models,
                      fold_cols = '.folds',
                      family = 'gaussian',
                      REML = FALSE)

# Show results
CV3

## ------------------------------------------------------------------------
CV4 <- cross_validate(data, mixed_models,
                      fold_cols = '.folds',
                      family = 'gaussian',
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
                      family = 'binomial',
                      REML = FALSE)

# Show results
CV5

# The binomial output now has a nested 'Results' tibble
# Let's see a subset of the columns
CV5$Results[[1]] %>% select(1:8) %>%  kable()

## ------------------------------------------------------------------------
# Create model function
#
# train_data : tibble with the training data
# formula : a formula object

svm_model_fn <- function(train_data, formula){
  
  # Note that `formula` must be specified first
  # when calling svm(), otherwise it fails
  e1071::svm(formula = formula,
             data = train_data, 
             kernel = "linear",
             type = "C-classification")
}

## ------------------------------------------------------------------------
# Cross-validate svm_model_fn
CV6 <- cross_validate_fn(data = data,
                         model_fn = svm_model_fn,
                         formulas = c("diagnosis~score", "diagnosis~age"),
                         fold_cols = '.folds_1', 
                         type = 'binomial')

CV6

## ------------------------------------------------------------------------
# Create model function
#
# train_data : tibble with the training data
# formula : a formula object

nb_model_fn <- function(train_data, formula){
  e1071::naiveBayes(formula = formula, 
                    data = train_data)
}

## ------------------------------------------------------------------------
# Create predict function
#
# test_data : tibble with the test data
# model : fitted model object
# formula : a formula object
nb_predict_fn <- function(test_data, model, formula){
    stats::predict(object = model, newdata = test_data, 
                   type = "raw", allow.new.levels = TRUE)[,2]
  }

## ------------------------------------------------------------------------
CV7 <- cross_validate_fn(data,
                         model_fn = nb_model_fn,
                         formulas = c("diagnosis~score", "diagnosis~age"),
                         type = 'binomial',
                         predict_fn = nb_predict_fn,
                         fold_cols = '.folds_1')

CV7

## ------------------------------------------------------------------------
# Set seed
set.seed(1)

# Create class names
class_names <- paste0("class_", 1:4)

# Create random dataset with 100 observations 
# Partition into training set (75%) and test set (25%)
multiclass_partitions <- multiclass_probability_tibble(
  num_classes = 3, # Here, number of predictors
  num_observations = 100,
  apply_softmax = FALSE,
  FUN = rnorm,
  class_name = "predictor_") %>%
  dplyr::mutate(class = sample(
    class_names,
    size = 100,
    replace = TRUE)) %>%
  partition(p = 0.75,
            cat_col = "class")

# Extract partitions
multiclass_train_set <- multiclass_partitions[[1]]
multiclass_test_set <- multiclass_partitions[[2]]

multiclass_test_set

## ------------------------------------------------------------------------
# Train multinomial model
multiclass_model <- nnet::multinom(
   "class ~ predictor_1 + predictor_2 + predictor_3",
   data = multiclass_train_set)

# Predict the targets in the test set
predictions <- predict(multiclass_model, 
                       multiclass_test_set,
                       type = "probs") %>%
  dplyr::as_tibble()

# Add the targets
predictions[["target"]] <- multiclass_test_set[["class"]]

head(predictions, 10)

## ------------------------------------------------------------------------
# Evaluate predictions
ev <- evaluate(data = predictions,
               target_col = "target",
               prediction_cols = class_names,
               type = "multinomial")

ev

## ------------------------------------------------------------------------
ev$`Class Level Results`

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
multiclass_baseline <- baseline(
  test_data = multiclass_test_set, n = 100,
  dependent_col = "class", family = "multinomial")

# Summarized metrics
multiclass_baseline$summarized_metrics

# Summarized class level results for class 1
multiclass_baseline$summarized_class_level_results %>% 
  dplyr::filter(Class == "class_1") %>%
  tidyr::unnest(Results)

# Random evaluations
# Note, that the class level results for each repetition
# is available as well
multiclass_baseline$random_evaluations

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

