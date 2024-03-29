% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choosing_metrics_functions.R
\name{multinomial_metrics}
\alias{multinomial_metrics}
\title{Select metrics for multinomial evaluation}
\usage{
multinomial_metrics(
  all = NULL,
  overall_accuracy = NULL,
  balanced_accuracy = NULL,
  w_balanced_accuracy = NULL,
  accuracy = NULL,
  w_accuracy = NULL,
  f1 = NULL,
  w_f1 = NULL,
  sensitivity = NULL,
  w_sensitivity = NULL,
  specificity = NULL,
  w_specificity = NULL,
  pos_pred_value = NULL,
  w_pos_pred_value = NULL,
  neg_pred_value = NULL,
  w_neg_pred_value = NULL,
  auc = NULL,
  kappa = NULL,
  w_kappa = NULL,
  mcc = NULL,
  detection_rate = NULL,
  w_detection_rate = NULL,
  detection_prevalence = NULL,
  w_detection_prevalence = NULL,
  prevalence = NULL,
  w_prevalence = NULL,
  false_neg_rate = NULL,
  w_false_neg_rate = NULL,
  false_pos_rate = NULL,
  w_false_pos_rate = NULL,
  false_discovery_rate = NULL,
  w_false_discovery_rate = NULL,
  false_omission_rate = NULL,
  w_false_omission_rate = NULL,
  threat_score = NULL,
  w_threat_score = NULL,
  aic = NULL,
  aicc = NULL,
  bic = NULL
)
}
\arguments{
\item{all}{Enable/disable all arguments at once. (Logical)

 Specifying other metrics will overwrite this, why you can
 use (\code{all = FALSE, accuracy = TRUE}) to get only the Accuracy metric.}

\item{overall_accuracy}{\code{Overall Accuracy} (Default: TRUE)}

\item{balanced_accuracy}{\code{Macro Balanced Accuracy} (Default: TRUE)}

\item{w_balanced_accuracy}{\code{Weighted Balanced Accuracy} (Default: FALSE)}

\item{accuracy}{\code{Accuracy} (Default: FALSE)}

\item{w_accuracy}{\code{Weighted Accuracy} (Default: FALSE)}

\item{f1}{\code{F1} (Default: TRUE)}

\item{w_f1}{\code{Weighted F1} (Default: FALSE)}

\item{sensitivity}{\code{Sensitivity} (Default: TRUE)}

\item{w_sensitivity}{\code{Weighted Sensitivity} (Default: FALSE)}

\item{specificity}{\code{Specificity} (Default: TRUE)}

\item{w_specificity}{\code{Weighted Specificity} (Default: FALSE)}

\item{pos_pred_value}{\code{Pos Pred Value} (Default: TRUE)}

\item{w_pos_pred_value}{\code{Weighted Pos Pred Value} (Default: FALSE)}

\item{neg_pred_value}{\code{Neg Pred Value} (Default: TRUE)}

\item{w_neg_pred_value}{\code{Weighted Neg Pred Value} (Default: FALSE)}

\item{auc}{\code{AUC} (Default: FALSE)}

\item{kappa}{\code{Kappa} (Default: TRUE)}

\item{w_kappa}{\code{Weighted Kappa} (Default: FALSE)}

\item{mcc}{\code{MCC} (Default: TRUE)

 Multiclass Matthews Correlation Coefficient.}

\item{detection_rate}{\code{Detection Rate} (Default: TRUE)}

\item{w_detection_rate}{\code{Weighted Detection Rate} (Default: FALSE)}

\item{detection_prevalence}{\code{Detection Prevalence} (Default: TRUE)}

\item{w_detection_prevalence}{\code{Weighted Detection Prevalence} (Default: FALSE)}

\item{prevalence}{\code{Prevalence} (Default: TRUE)}

\item{w_prevalence}{\code{Weighted Prevalence} (Default: FALSE)}

\item{false_neg_rate}{\code{False Neg Rate} (Default: FALSE)}

\item{w_false_neg_rate}{\code{Weighted False Neg Rate} (Default: FALSE)}

\item{false_pos_rate}{\code{False Pos Rate} (Default: FALSE)}

\item{w_false_pos_rate}{\code{Weighted False Pos Rate} (Default: FALSE)}

\item{false_discovery_rate}{\code{False Discovery Rate} (Default: FALSE)}

\item{w_false_discovery_rate}{\code{Weighted False Discovery Rate} (Default: FALSE)}

\item{false_omission_rate}{\code{False Omission Rate} (Default: FALSE)}

\item{w_false_omission_rate}{\code{Weighted False Omission Rate} (Default: FALSE)}

\item{threat_score}{\code{Threat Score} (Default: FALSE)}

\item{w_threat_score}{\code{Weighted Threat Score} (Default: FALSE)}

\item{aic}{AIC. (Default: FALSE)}

\item{aicc}{AICc. (Default: FALSE)}

\item{bic}{BIC. (Default: FALSE)}
}
\description{
\Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}

 Enable/disable metrics for multinomial evaluation. Can be supplied to the
 \code{`metrics`} argument in many of the \code{cvms} functions.

 Note: Some functions may have slightly different defaults than the ones supplied here.
}
\examples{
\donttest{
# Attach packages
library(cvms)

# Enable only Balanced Accuracy
multinomial_metrics(all = FALSE, balanced_accuracy = TRUE)

# Enable all but Balanced Accuracy
multinomial_metrics(all = TRUE, balanced_accuracy = FALSE)

# Disable Balanced Accuracy
multinomial_metrics(balanced_accuracy = FALSE)
}
}
\seealso{
Other evaluation functions: 
\code{\link{binomial_metrics}()},
\code{\link{confusion_matrix}()},
\code{\link{evaluate}()},
\code{\link{evaluate_residuals}()},
\code{\link{gaussian_metrics}()}
}
\author{
Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
}
\concept{evaluation functions}
