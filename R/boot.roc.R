#' @title boot.roc
#' @description computes bootstrap AUC and AUCPR for the ROC curve
#' @importFrom stats sd
#' @importFrom ROCit rocit
#' @importFrom yardstick pr_auc_vec
#' @importFrom boot boot boot.ci
#' @param score A numeric array of diagnostic score i.e. the estimated probability of each diagnosis
#' @param class A numeric array of equal length of \code{"score"}, including the
#'              actual class of the observations
#' @param n number of bootstrap samples.
#' @param method Specifies the method for estimating the ROC curve. Three methods
#'               are supported, which are \code{"empirical"}, \code{"binormal"},
#'               and \code{"nonparametric"}
#' @param event_level character. only needed for bootstrapping AUCPR. this
#'                    argument specifies which level of the "class" should be
#'                    considered the positive event. the values can only be
#'                    \code{"first"} or \code{"second"}.
#' @param metric character. specify the metric of interest which can be
#'               \code{"AUC"} (Area Under the Curve, default) or  \code{"AUCPR"}
#'               (Area Under the Precision-Recall Curve).
#' @return list including mean and CI of bootstrap value (sensitivity, specificity, or
#'     the crossing point) and the bootstrap data.
#'@examples
#'# random classification and probability score
#'score <- runif(10000, min=0, max=1)
#'class <- sample(x = c(1,0), 10000, replace=TRUE)
#'
#'# calculate bootstrap AUC of the ROC curve
#'boot.roc(score = score, class = class, n = 100, metric = "AUC")
#'
#'# calculate bootstrap AUCPR of the ROC curve
#'boot.roc(score = score, class = class, n = 100, metric = "AUCPR")
#' @export

boot.roc <- function(score,
                     class,
                     metric = "AUC",
                     n = 100,
                     method = "emp",
                     event_level = "first") {

  # define the statistics function
  # ============================================================
  if (metric == "AUC") {
    statistic <- function(df = df, indices, ...) {
      df <- df[indices, ]    # subset the bootstrapped data
      roc <- ROCit::rocit(score = df$score, class = df$class, method = method)
      return(roc$AUC)
    }
  } else if (metric == "AUCPR") {
    statistic <- function(df = df, indices, ...) {
      df <- df[indices, ]    # subset the bootstrapped data
      df$class <- as.factor(df$class)
      return(yardstick::pr_auc_vec(df$class, df$score, event_level = event_level))
    }
  }

  # create the dataframe
  # ============================================================
  df <- as.data.frame(cbind(score = score, class = class))

  # run bootstrap adjroc
  # ============================================================
  results <- boot::boot(data = df, statistic = statistic, R = n, event_level = event_level)

  # mean and CI of adjroc
  # ============================================================
  mean <- mean(results$t)
  ci   <- boot::boot.ci(results, type = "norm")
  names(ci$normal) <- c("ci", "low", "high")
  cat("Mean (95% CI) =", mean, "(", ci$normal[1,2:3], ")\n\n")

  return(list(mean = mean,
              ci = ci$normal,
              sd = sd(results$boot$t),
              boot = results))
}
