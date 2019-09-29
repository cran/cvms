# cvms 0.3.0

* Breaking change: In `evaluate()`, when `type` is `multinomial`, the output is now a single tibble. The `Class Level Results` are included as a nested tibble.

* Breaking change: In `baseline()`, `lmer` models are now fitted with `REML = FALSE` by default.

* Adds `REML` argument to `baseline()`.

* `cross_validate_fn()` is added. Cross-validate custom model functions.

* Bug fix: the `control` argument in `cross_validate()` was not being used. Now it is.

* In `cross_validate()`, the model is no longer fitted twice when a warning is thrown during fitting.

* Adds `metrics` argument to `cross_validate()` and `validate()`. Allows enabling the regular `Accuracy` metric
in `binomial` or to disable metrics (will currently still be computed but not included in the output).

* `AICc` is now computed with the `MuMIn` package instead of the `AICcmodavg` package, which
is no longer a dependency.

* Adds `lifecycle` badges to the function documentation.

# cvms 0.2.0

* `evaluate()` is added. Evaluate your model's predictions with the same metrics as used in `cross_validate()`.

* Adds `'multinomial'` family/type to `baseline()` and `evaluate()`.

* Adds `multiclass_probability_tibble()` for generating a random probability tibble. 

* Adds `random_effects` argument to `baseline()` for adding random effects to the Gaussian baseline model.

* Adds Zenodo DOI for easier citation.

* In nested confusion matrices, the Reference column is renamed to Target, to use the same naming scheme as in the nested predictions.

# cvms 0.1.2

* Bug fix: p-values are correctly added to the nested coefficients tibble. Adds tests of this table as well.

* Adds extra unit tests to increase code coverage.

* When argument `"model_verbose"` is `TRUE`, the used model function is now messaged instead of printed.

* Adds badges to README, including travis-ci status, AppVeyor status, 
Codecov, min. required R version, CRAN version and monthly CRAN downloads. Note: Zenodo badge will be added post release.

# cvms 0.1.1

* Unit tests have been made compatible with `R v. 3.5`

# cvms 0.1.0

* Adds optional parallelization.

* Results now contain a count of singular fit messages. See `?lme4::isSingular` for more information.

* Argument `"positive"` changes default value to 2. Now takes either 1 or 2 (previously 0 and 1). If your dependent variable has values 0 and 1, 1 is now the positive class by default.

* AUC calculation has changed. Now explicitly sets the direction in `pROC::roc`.

* Unit tests have been updated for the new random sampling generator in `R 3.6.0`. They will NOT run previous versions of R. 

* Adds `baseline()` for creating baseline evaluations.

* Adds `reconstruct_formulas()` for reconstructing formulas based on model definition columns in the results tibble.

* Adds `combine_predictors()` for generating model formulas from a set of fixed effects.

* Adds `select_metrics()` for quickly selecting the metrics and model definition columns.

* Breaking change: Metrics have been rearranged and a few metrics have been added.  

* Breaking change: Renamed argument `folds_col` to `fold_cols` to better fit the new repeated cross-validation option.  

* New: repeated cross-validation.  

* Created package :)  
