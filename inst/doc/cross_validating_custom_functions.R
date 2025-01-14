## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/vignette_cv_custom_fn-",
  dpi = 92,
  fig.retina = 2,
  eval = requireNamespace("e1071") # Only evaluate chunks when e1071 is installed!
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(cvms)
library(groupdata2)    # fold()
library(dplyr)
library(knitr)         # kable() : formats the output as a table
library(e1071)         # svm()

set.seed(1)

## -----------------------------------------------------------------------------
# Enable parallelization by uncommenting
# library(doParallel)
# registerDoParallel(4) # 4 cores

## -----------------------------------------------------------------------------
# Prepare dataset
data <- participant.scores
data$diagnosis <- factor(data$diagnosis)

# Create 5 fold columns with 3 folds each
data <- fold(
  data,
  k = 3,
  cat_col = "diagnosis",
  id_col = "participant",
  num_fold_cols = 5,
  parallel = FALSE # set to TRUE to run in parallel
)

# Order by participant
data <- data %>% 
  dplyr::arrange(participant)

# Look at the first 12 rows
# Note: kable() just formats the table 
data %>% head(12) %>% kable()


## -----------------------------------------------------------------------------
# Check the distribution of 'diagnosis' in the first fold column
# Note: this would be more even for a larger dataset
data %>%
  dplyr::count(.folds_1, diagnosis) %>% 
  kable()

# Check the distribution of 'participant' in the first fold column
# Note that all rows for a participant are in the same fold
data %>%
  dplyr::count(.folds_1, participant) %>% 
  kable()


## -----------------------------------------------------------------------------
# Split into train and test sets
test_set <- data %>% 
  dplyr::filter(.folds_1 == 3)
train_set <- data %>% 
  dplyr::filter(.folds_1 != 3)

# Fit SVM model
svm_model <- e1071::svm(
    formula = score ~ diagnosis + age + session,
    data = train_set,
    kernel = "linear",
    cost = 10,
    type = "eps-regression"
  )

# Predict scores in the test set
predicted_scores <- predict(
  svm_model,
  newdata = test_set,
  allow.new.levels = TRUE)

predicted_scores

# Add predictions to test set
test_set[["predicted score"]] <- predicted_scores

# Evaluate the predictions
evaluate(
  data = test_set,
  target_col = "score",
  prediction_cols = "predicted score",
  type = "gaussian"
)


## -----------------------------------------------------------------------------
svm_model_fn <- function(train_data, formula, hyperparameters) {
  e1071::svm(
    formula = formula,
    data = train_data,
    kernel = "linear",
    cost = 10,
    type = "eps-regression"
  )
}


## -----------------------------------------------------------------------------
# Try the model function
# Due to "lazy evaluation" in R, we don't have to pass
# the arguments that are not used inside the function
m0 <- svm_model_fn(train_data = train_set, 
                   formula = score ~ diagnosis + age + session)
m0

## -----------------------------------------------------------------------------
svm_predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
  predict(object = model,
          newdata = test_data,
          allow.new.levels = TRUE)
}


# Try the predict function
svm_predict_fn(test_data = test_set, model = m0)


## -----------------------------------------------------------------------------
cv_1 <- cross_validate_fn(
  data = data,
  formulas = c("score ~ diagnosis + age + session",
               "score ~ diagnosis + age",
               "score ~ diagnosis"),
  type = "gaussian",
  model_fn = svm_model_fn,
  predict_fn = svm_predict_fn,
  fold_cols = paste0(".folds_", 1:5),
  parallel = FALSE # set to TRUE to run in parallel
)

cv_1


## -----------------------------------------------------------------------------
svm_model_fn <- function(train_data, formula, hyperparameters) {
  
  # Required hyperparameters:
  #  - kernel
  #  - cost
  if (!"kernel" %in% names(hyperparameters))
    stop("'hyperparameters' must include 'kernel'")
  if (!"cost" %in% names(hyperparameters))
    stop("'hyperparameters' must include 'cost'")
  
  e1071::svm(
    formula = formula,
    data = train_data,
    kernel = hyperparameters[["kernel"]],
    cost = hyperparameters[["cost"]],
    scale = FALSE,
    type = "eps-regression"
  )
}

# Try the model function
svm_model_fn(
  train_data = train_set,
  formula = score ~ diagnosis + age + session,
  hyperparameters = list(
    "kernel" = "linear",
    "cost" = 5
  )
)

## -----------------------------------------------------------------------------
svm_model_fn <- function(train_data, formula, hyperparameters) {
  
  # Required hyperparameters:
  #  - cost
  # Optional hyperparameters:
  #  - kernel
  
  # 1) If 'cost' is not present in hyperparameters, throw error
  # 2) If 'kernel' is not present in hyperparameters, set to "radial"
  hyperparameters <- update_hyperparameters(
    kernel = "radial",
    hyperparameters = hyperparameters,
    required = "cost"
  )

  e1071::svm(
    formula = formula,
    data = train_data,
    kernel = hyperparameters[["kernel"]],
    cost = hyperparameters[["cost"]],
    type = "eps-regression"
  )
}

## -----------------------------------------------------------------------------
hparams <- list(
  "kernel" = c("linear", "radial"),
  "cost" = c(1, 5, 10)
)

## -----------------------------------------------------------------------------
hparams <- list(
  ".n" = 4,
  "kernel" = c("linear", "radial"),
  "cost" = c(1, 5, 10)
)

## -----------------------------------------------------------------------------
df_hparams <- data.frame(
  "kernel" = c("linear", "radial", "radial"),
  "cost" = c(10, 1, 10)
)

df_hparams

## -----------------------------------------------------------------------------
# Set seed for the sampling of the hyperparameter combinations
set.seed(1)

cv_2 <- cross_validate_fn(
  data = data,
  formulas = c("score ~ diagnosis + age + session",
               "score ~ diagnosis"),
  type = "gaussian",
  model_fn = svm_model_fn,
  predict_fn = svm_predict_fn,
  hyperparameters = hparams,   # Pass the list of values to test
  fold_cols = paste0(".folds_", 1:5)
)

cv_2

## -----------------------------------------------------------------------------
cv_2 %>% 
  # Create Model ID with values 1:8
  dplyr::mutate(`Model ID` = 1:nrow(cv_2)) %>% 
  # Order by RMSE
  dplyr::arrange(RMSE) %>% 
  # Extract formulas and hyperparameters
  select_definitions(additional_includes = c("RMSE", "Model ID")) %>% 
  # Pretty table
  kable()

## -----------------------------------------------------------------------------
# Extract fold results for the best model
cv_2$Results[[1]] %>% kable()

# Extract 10 predictions from the best model
cv_2$Predictions[[1]] %>% head(10) %>% kable()


## ----eval=FALSE---------------------------------------------------------------
# # NOTE: Don't run this
# preprocess_fn <- function(train_data, test_data, formula, hyperparameters) {
# 
#   # Do preprocessing
#   # Create data frame with applied preprocessing parameters
# 
#   # Return list with these names
#   list("train" = train_data,
#        "test" = test_data,
#        "parameters" = preprocess_parameters)
# }
# 

## -----------------------------------------------------------------------------
preprocess_fn <- function(train_data, test_data, formula, hyperparameters) {
  
  # Standardize the age column 
  
  # Get the mean and standard deviation from the train_data
  mean_age <- mean(train_data[["age"]])
  sd_age <- sd(train_data[["age"]])
  
  # Standardize both train_data and test_data
  train_data[["age"]] <- (train_data[["age"]] - mean_age) / sd_age
  test_data[["age"]] <- (test_data[["age"]] - mean_age) / sd_age
  
  # Create data frame with applied preprocessing parameters
  preprocess_parameters <- data.frame(
    "Measure" = c("Mean", "SD"),
    "age" = c(mean_age, sd_age)
  )
  
  # Return list with these names
  list("train" = train_data,
       "test" = test_data,
       "parameters" = preprocess_parameters)
}

# Try the preprocess function
prepped <- preprocess_fn(train_data = train_set, test_data = test_set)

# Inspect preprocessed training set
# Note that the age column has changed
prepped$train %>% head(5) %>% kable()

# Inspect preprocessing parameters
prepped$parameters %>% kable()


## -----------------------------------------------------------------------------
cv_3 <- cross_validate_fn(
  data = data,
  formulas = c("score ~ diagnosis + age + session",
               "score ~ diagnosis"),
  type = "gaussian",
  model_fn = svm_model_fn,
  predict_fn = svm_predict_fn,
  preprocess_fn = preprocess_fn,
  hyperparameters = list(
    "kernel" = "linear",
    "cost" = 1
  ),
  fold_cols = paste0(".folds_", 1:5)
)

cv_3


## -----------------------------------------------------------------------------
# Extract first 10 rows of the preprocess parameters
# for the first and best model
cv_3$Preprocess[[1]] %>% head(10) %>% kable()

## -----------------------------------------------------------------------------
# Get built-in preprocess function
preprocess_functions("standardize")


## -----------------------------------------------------------------------------
# SVM model function for classification 
clf_svm_model_fn <- function(train_data, formula, hyperparameters) {

  # Optional hyperparameters:
  #  - kernel
  #  - cost

  # Update missing hyperparameters with default values
  hyperparameters <- update_hyperparameters(
    kernel = "radial",
    cost = 1,
    hyperparameters = hyperparameters
  )

  e1071::svm(
    formula = formula,
    data = train_data,
    kernel = hyperparameters[["kernel"]],
    cost = hyperparameters[["cost"]],
    type = "C-classification",
    probability = TRUE  # Must enable probability here
  )
}

# Try the model function
m1 <- clf_svm_model_fn(train_data = data, formula = diagnosis ~ score, 
                       hyperparameters = list("kernel" = "linear"))
m1


## -----------------------------------------------------------------------------
# Predict function for binomial SVM
bnml_svm_predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
  # Predict test set
  predictions <- predict(
    object = model,
    newdata = test_data,
    allow.new.levels = TRUE,
    probability = TRUE
  )
  
  # Extract probabilities
  probabilities <- dplyr::as_tibble(attr(predictions, "probabilities"))
  
  # Return second column
  probabilities[[2]]
}

p1 <- bnml_svm_predict_fn(test_data = data, model = m1)
p1    # Vector with probabilities that diagnosis is 1

## ----warning=FALSE------------------------------------------------------------
cv_4 <- cross_validate_fn(
  data = data,
  formulas = c("diagnosis ~ score",
               "diagnosis ~ age"),
  type = "binomial",
  model_fn = clf_svm_model_fn,
  predict_fn = bnml_svm_predict_fn,
  hyperparameters = list(
    "kernel" = c("linear", "radial"),
    "cost" = c(1, 5, 10)
  ),
  fold_cols = paste0(".folds_", 1:5)
)

cv_4


## -----------------------------------------------------------------------------
cv_4 %>% 
  dplyr::mutate(`Model ID` = 1:nrow(cv_4)) %>% 
  dplyr::arrange(dplyr::desc(`Balanced Accuracy`)) %>% 
  select_definitions(additional_includes = c("Balanced Accuracy", "F1", "MCC", "Model ID")) %>% 
  kable()


## -----------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(1)

# Prepare dataset
data_mc <- musicians
data_mc[["ID"]] <- as.factor(data_mc[["ID"]])

# Create 5 fold columns with 5 folds each
data_mc <- fold(
  data = data_mc,
  k = 5,
  cat_col = "Class",
  num_col = "Age",
  num_fold_cols = 5
)

data_mc %>% head(10) %>% kable()

# You can use skimr to get a better overview of the dataset
# Uncomment:
# library(skimr) 
# skimr::skim(data_mc)

## -----------------------------------------------------------------------------
# Predict function for multinomial SVM
mc_svm_predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
  predictions <- predict(
    object = model,
    newdata = test_data,
    allow.new.levels = TRUE,
    probability = TRUE
  )
  
  # Extract probabilities
  probabilities <- dplyr::as_tibble(attr(predictions, "probabilities"))
  
  # Return all columns
  probabilities
}


## -----------------------------------------------------------------------------
cv_5 <- cross_validate_fn(
  data = data_mc,
  formulas = c("Class ~ Age + Height",
               "Class ~ Age + Height + Bass + Guitar + Keys + Vocals"),
  type = "multinomial",
  model_fn = clf_svm_model_fn,
  predict_fn = mc_svm_predict_fn,
  hyperparameters = list(
    "kernel" = c("linear", "radial"),
    "cost" = c(1, 5, 10)
  ),
  fold_cols = paste0(".folds_", 1:5)
)

cv_5


## -----------------------------------------------------------------------------
cv_5 %>% 
  dplyr::mutate(`Model ID` = 1:nrow(cv_5)) %>% 
  dplyr::arrange(dplyr::desc(`Balanced Accuracy`)) %>% 
  select_definitions(additional_includes = c(
    "Balanced Accuracy", "F1", "Model ID")) %>% 
  kable()


## -----------------------------------------------------------------------------
# Extract Class Level Results for the best model
cv_5$`Class Level Results`[[11]]


## -----------------------------------------------------------------------------
# Extract fold results for the best model
cv_5$Results[[11]]


## -----------------------------------------------------------------------------
# Extract multiclass confusion matrices for the best model
# One per fold column
cv_5$`Confusion Matrix`[[11]]


## ----fig.width=5.5, fig.height=5.5, fig.align='center'------------------------
# Sum the fold column confusion matrices
# to one overall confusion matrix
overall_confusion_matrix <- cv_5$`Confusion Matrix`[[11]] %>% 
  dplyr::group_by(Prediction, Target) %>% 
  dplyr::summarise(N = sum(N))

overall_confusion_matrix %>% kable()

# Plot the overall confusion matrix
plot_confusion_matrix(overall_confusion_matrix, add_sums = TRUE)


