## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/vignette_pick_k-",
  dpi = 92,
  fig.retina = 2
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----echo=FALSE, warning=FALSE, message=FALSE---------------------------------
rerun_analysis <- FALSE
save_analysis <- FALSE

if (isTRUE(rerun_analysis)){
  # Uncomment temporarily!!
  # library(doParallel)
  # doParallel::registerDoParallel(6)
}

## ----echo=FALSE, warning=FALSE, message=FALSE, fig.align='center'-------------
library(cvms)
library(dplyr)
library(ggplot2)
library(groupdata2)

generate_data <- function(num_points = 35, seed = 3) {
  xpectr::set_test_seed(seed)
  intuition_df <- data.frame("x" = runif(num_points))
  intuition_df <- intuition_df %>%
    dplyr::mutate(
      y = 1.5 * rnorm(num_points) * runif(num_points) + 1.2 * x + 1.5 * x ^ 2 + 2.8 * x ^ 3) %>%
    groupdata2::fold(k = c(3, 10), num_fold_cols = 2) %>%
    dplyr::mutate(`k = 3` = as.character(.folds_1),
                  `k = 10` = as.character(.folds_2)) %>%
    tidyr::gather(key = "Fold Column", value = "Fold", 5:6) %>%
    dplyr::mutate(Fold = factor(Fold))
  
  intuition_cv <- cross_validate(
    intuition_df,
    formulas = "y ~ x",
    family = "gaussian",
    fold_cols = paste0(".folds_", 1:2)
  )$Results[[1]] %>%
    dplyr::group_by(`Fold Column`) %>%
    dplyr::summarise(RMSE = mean(RMSE),
                     MAE = mean(MAE))

  list(intuition_df, intuition_cv)
}

# Largest difference at: 2
# for (i in 1:100){
#   out <- generate_data(num_points=35, seed=i)  
#   print(i)
#   print(out[[2]])
# }

out <- generate_data(num_points=35, seed=2)
res <- out[[2]]
res[["Fold Column"]] <- factor(c("k = 3", "k = 10"), levels = c("k = 3", "k = 10"))
res[["RMSE"]] <- paste0("RMSE: ", round(res[["RMSE"]], digits = 3))
data <- out[[1]]
data$`Fold Column` <- factor(data$`Fold Column`, levels = c("k = 3", "k = 10"))
data$Fold <- factor(data$Fold, levels = 1:10)

data %>%
  ggplot(aes(x = x, y = y, color = Fold)) + 
  geom_point() +
  stat_smooth(method = "lm", se = FALSE, alpha = 0.5, size = 0.3) +
  facet_wrap(`Fold Column` ~ .) + 
  geom_text(data = res, aes(label = RMSE, color = NULL), x = 0.75, y = -1) +
  theme_minimal() +
  labs(caption = paste0(
    "Generated data split into 3 (left) and 10 (right) folds.",
    "\nLines are optimal linear models for each fold.",
    "\nRMSE (Root Mean Square Error) is for a cross-validated linear model (higher = worse).",
    "\nIn k=3, the third fold differs, impacting the prediction negatively.",
    "\nIn k=10, the two 'outlier' folds don't impact the average error as much."
    )
  )


## ----echo=FALSE, warning=FALSE, message=FALSE---------------------------------
if (isTRUE(rerun_analysis)){
  bootstrapped <- plyr::ldply(1:100, function(i) generate_data(num_points=35, seed=i)[[2]])
  bootstrapped <- bootstrapped %>% 
    dplyr::mutate(`Fold Column` = dplyr::case_when(
      `Fold Column` == ".folds_1" ~ "k = 3",
      `Fold Column` == ".folds_2" ~ "k = 10",
      TRUE ~ `Fold Column`
    ))
  if (isTRUE(save_analysis)){
    save(bootstrapped, file="inst/vignette_data/bootstrapped_cv_picking_k.rda")      
  }
} else {
  bootstrapped_file_path <- cvms:::get_vignette_data_path("bootstrapped_cv_picking_k.rda")
  load(bootstrapped_file_path)
}

bootstrapped <- bootstrapped %>% 
  dplyr::mutate(`Fold Column` = factor(`Fold Column`, levels = c("k = 3", "k = 10")))

bootstrapped %>% 
  dplyr::group_by(`Fold Column`) %>% 
  dplyr::summarise_all(mean)

three_is_larger <- bootstrapped %>% 
  groupdata2::group(n = 2, method = "greedy") %>% 
  dplyr::summarize(larger = diff(RMSE) < 0) %>% 
  dplyr::summarize(larger = sum(larger))

## ----echo=FALSE, warning=FALSE, message=FALSE, fig.width=5.5, fig.height=4, fig.align='center'----
bootstrapped %>% 
  ggplot(aes(x = `Fold Column`, y = RMSE, fill = `Fold Column`)) +
  geom_violin() + 
  theme_minimal()


## ----warning=FALSE, message=FALSE---------------------------------------------
library(cvms)  # version >= 1.2.2 
library(groupdata2)  # version >= 1.4.1
library(dplyr)
library(ggplot2)

xpectr::set_test_seed(1)

## ----eval=FALSE---------------------------------------------------------------
#  # Enable parallelization
#  # NOTE: Uncomment to run
#  # library(doParallel)
#  # doParallel::registerDoParallel(6)
#  

## -----------------------------------------------------------------------------
# Load iris
data("iris")

# Convert iris to a tibble
iris <- dplyr::as_tibble(iris)
iris

## -----------------------------------------------------------------------------
iris %>% 
  dplyr::count(Species)


## -----------------------------------------------------------------------------
# Generate sequence of `k` settings in the 3-50 range
fold_counts <- round(seq(from = 3, to = 50, length.out = 10))
# Repeat each 3 times
fold_counts <- rep(fold_counts, each = 3)

fold_counts


## ----echo=FALSE, warning=FALSE, message=FALSE---------------------------------
if (isTRUE(rerun_analysis)){
  data <- iris %>% 
    groupdata2::fold(
      k = fold_counts, 
      cat_col = "Species", 
      num_fold_cols = length(fold_counts),  # Must match the length of `k` 
      parallel = TRUE
    )
  if (isTRUE(save_analysis)){
    save(data, file="inst/vignette_data/folded_iris_picking_k.rda")      
  }
} else {
  data_file_path <- cvms:::get_vignette_data_path("folded_iris_picking_k.rda")
  load(data_file_path)
}


## ----eval=FALSE---------------------------------------------------------------
#  data <- iris %>%
#    groupdata2::fold(
#      k = fold_counts,
#      cat_col = "Species",
#      num_fold_cols = length(fold_counts),  # Must match the length of `k`
#      parallel = TRUE
#    )
#  

## -----------------------------------------------------------------------------
data

## -----------------------------------------------------------------------------
# Quick way to generate the names of the fold columns
# Note: `seq_along()` is equal to `1:length(fold_counts)`
fold_columns <- paste0(".folds_", seq_along(fold_counts))
fold_columns

## -----------------------------------------------------------------------------
fold_stats <- groupdata2::summarize_group_cols(
    data = data,
    group_cols = fold_columns
  ) %>% 
  rename(`Fold Column` = `Group Column`)

# View fold column statistics
# We only look at one fold column per `k` setting
fold_stats %>% 
  dplyr::group_by(`Num Groups`) %>% 
  dplyr::filter(dplyr::row_number() == 1) %>% 
  knitr::kable()

## -----------------------------------------------------------------------------
# To use the e1071::svm() model function
# we specify the model and predict functions
model_fn <- cvms::model_functions("svm_multinomial")
predict_fn <- cvms::predict_functions("svm_multinomial")

# Specify hyperparameters
hyperparameters <- list('kernel' = 'radial', 'cost' = 10)

## -----------------------------------------------------------------------------
formulas <- c(
  "Species ~ Sepal.Length + Sepal.Width",
  "Species ~ Petal.Length + Petal.Width",
  "Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width",
  "Species ~ Sepal.Length * Sepal.Width + Petal.Length * Petal.Width"
)

## ----echo=FALSE---------------------------------------------------------------
if (isTRUE(rerun_analysis)){
  cv <- cross_validate_fn(
    data = data,
    formulas = formulas,
    type = "multinomial",
    model_fn = model_fn,
    predict_fn = predict_fn,
    hyperparameters = hyperparameters,
    fold_cols = fold_columns,
    parallel = TRUE
  )
  cv$Predictions <- "Removed to save memory"
  if (isTRUE(save_analysis)){
    save(cv, file="inst/vignette_data/cv_iris_picking_k.rda")      
  }
} else {
  cv_file_path <- cvms:::get_vignette_data_path("cv_iris_picking_k.rda")
  load(cv_file_path)
}

## ----eval=FALSE---------------------------------------------------------------
#  cv <- cross_validate_fn(
#    data = data,
#    formulas = formulas,
#    type = "multinomial",
#    model_fn = model_fn,
#    predict_fn = predict_fn,
#    hyperparameters = hyperparameters,
#    fold_cols = fold_columns,
#    parallel = TRUE
#  )
#  

## -----------------------------------------------------------------------------
cv


## -----------------------------------------------------------------------------
# Extract the fold column results
# This is a list of data frames (one per formula)
fold_column_results <- cv$Results

## -----------------------------------------------------------------------------
# Set the names of the data frames to their respective formula
names(fold_column_results) <- cv$Fixed

# Combine the data frames
# Create a 'Formula' column from the names
fold_column_results <- fold_column_results %>% 
  dplyr::bind_rows(.id = "Formula")

## -----------------------------------------------------------------------------
# Make the formula string prettier for plotting
# Sepal -> S; Petal -> P; Width -> W; Length -> L
fold_column_results <- fold_column_results %>% 
  dplyr::mutate(
    Formula = gsub(
      x = Formula,
      pattern = "[^SPWL+*]",
      replacement = ""
    )
  )

## -----------------------------------------------------------------------------
fold_column_results


## -----------------------------------------------------------------------------
# Select the `Num Groups` column and rename to `Num Folds`
fold_counts_df <- fold_stats %>% 
  dplyr::select(`Fold Column`, `Num Groups`) %>% 
  dplyr::rename(`Num Folds` = `Num Groups`)

fold_counts_df

# Add the counts with a join
fold_column_results <- fold_counts_df %>% 
  dplyr::left_join(fold_column_results, by = "Fold Column")

fold_column_results


## ----message=FALSE, warning=FALSE---------------------------------------------
# Add a column for indicating the repetition of the `k` setting
fold_column_results <- fold_column_results %>%
  # Group to restart groupdata2 group numbers for each `Num Folds` setting
  dplyr::group_by(`Num Folds`) %>% 
  # Create a group whenever the `Fold Column` column changes
  groupdata2::group(n = 'auto',
                    method = 'l_starts',
                    starts_col = "Fold Column",
                    col_name = "Repetition") %>% 
  dplyr::ungroup()

# Inspect the columns relevant to this
fold_column_results %>% 
  dplyr::select(`Fold Column`, `Num Folds`, Formula, Repetition) %>% 
  head(20) %>% 
  knitr::kable()

## -----------------------------------------------------------------------------
avg_balanced_acc <- fold_column_results %>%
  dplyr::group_by(`Num Folds`, Formula) %>%
  dplyr::summarise(`Balanced Accuracy` = mean(`Balanced Accuracy`),
                   .groups = "drop")

avg_balanced_acc

## ----warning=FALSE, fig.align='center'----------------------------------------
# Plot the balanced accuracy by the number of folds
# We add jitter to the points to separate overlapping points slightly
fold_column_results %>% 
  ggplot(aes(x = `Num Folds`, y = `Balanced Accuracy`, color = Formula)) +
  geom_point(
    aes(shape = Repetition), 
    size = 1,
    position = position_jitter(h = 0.0, w = 0.6)) + 
  geom_line(data = avg_balanced_acc) +
  theme_minimal()


## -----------------------------------------------------------------------------
# Extract class level fold column results for the best model
# It is a list of tibbles (one for each species) 
# so we concatenate them to a single tibble with bind_rows()
class_level_fold_results <- cv$`Class Level Results`[[4]]$Results %>% 
  dplyr::bind_rows()

# Add the Num Folds counts
class_level_fold_results <- fold_counts_df %>% 
  dplyr::left_join(class_level_fold_results, by = "Fold Column")

class_level_fold_results


## -----------------------------------------------------------------------------
# Add a column for indicating the repetition of the `k` setting
class_level_fold_results <- class_level_fold_results %>% 
  # Group to restart groupdata2 group numbers for each `Num Folds` setting
  dplyr::group_by(`Num Folds`) %>% 
  # Create a group whenever the `Fold Column` column changes
  groupdata2::group(n = 'auto',
                    method = 'l_starts',
                    starts_col = "Fold Column",
                    col_name = "Repetition") %>% 
  dplyr::ungroup()

# Inspect the columns relevant to this
class_level_fold_results %>% 
  dplyr::select(`Fold Column`, `Num Folds`, Class, Repetition)

## -----------------------------------------------------------------------------
class_level_avg_balanced_acc <- class_level_fold_results %>%
  dplyr::group_by(`Num Folds`, Class) %>%
  dplyr::summarise(`Balanced Accuracy` = mean(`Balanced Accuracy`),
                   .groups = "drop")

class_level_avg_balanced_acc

## ----warning=FALSE, fig.align='center'----------------------------------------
# Plot the balanced accuracy by the number of folds
# We add jitter to the points to separate overlapping points slightly
class_level_fold_results  %>% 
  ggplot(aes(x = `Num Folds`, y = `Balanced Accuracy`, color = Class)) +
  geom_point(aes(shape = Repetition), size = 1, 
             position = position_jitter(h = 0.0, w = 0.6)) + 
  geom_line(data = class_level_avg_balanced_acc) +
  theme_minimal()


