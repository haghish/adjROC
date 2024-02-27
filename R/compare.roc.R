#' @title compare.roc
#' @description computes bootstrap AUC and AUCPR for two different ROC curves (models)
#'              and performs significance testing
#' @importFrom stats sd
#' @importFrom ROCit rocit
#' @importFrom yardstick pr_auc_vec
#' @importFrom boot boot boot.ci
#' @importFrom boot.pval boot.pval
#' @param score A numeric array of diagnostic score i.e. the estimated probability
#'              of each diagnosis for model 1
#' @param class A numeric array of equal length of \code{"score"}, including the
#'              actual class of the observations for model 1
#' @param score2 A numeric array of diagnostic score i.e. the estimated probability
#'              of each diagnosis for model 2
#' @param class2 A numeric array of equal length of \code{"score"}, including the
#'              actual class of the observations for model 2
#' @param metric character. specify the metric of interest which can be
#'               \code{"AUC"} (Area Under the Curve, default),  \code{"AUCPR"}
#'               (Area Under the Precision-Recall Curve) or \code{"meting_point"},
#'               which evaluates the crossing-point between sensitivity and
#'               specificity of two different models.
#' @param n number of bootstrap samples.
#' @param method Specifies the method for estimating the ROC curve. Three methods
#'               are supported, which are \code{"empirical"}, \code{"binormal"},
#'               and \code{"nonparametric"}
#' @param event_level character. only needed for bootstrapping AUCPR. this
#'                    argument specifies which level of the "class" should be
#'                    considered the positive event. the values can only be
#'                    \code{"first"} or \code{"second"}.
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

compare.roc <- function(score,
                     class,
                     score2,
                     class2,
                     metric = "AUC",
                     n = 100,
                     method = "emp",
                     event_level = "first") {

  # define the statistics function
  # ============================================================
  if (metric == "AUC") {
    statistic <- function(df = df, indices, ...) {
      df <- df[indices, ]    # subset the bootstrapped data
      a <- ROCit::rocit(score = df$score, class = df$class, method = method)$AUC
      b <- ROCit::rocit(score = df$score2, class = df$class2, method = method)$AUC
      return(a - b)
    }
  } else if (metric == "AUCPR") {
    statistic <- function(df = df, indices, ...) {
      df <- df[indices, ]    # subset the bootstrapped data
      df$class <- as.factor(df$class)
      df$class2 <- as.factor(df$class2)
      a <- yardstick::pr_auc_vec(df$class, df$score, event_level = event_level)
      b <- yardstick::pr_auc_vec(df$class2, df$score2, event_level = event_level)
      return(a - b)
    }
  } else if (metric == "meeting_point") {
    statistic <- function(df = df, indices, ...) {
      df <- df[indices, ]    # subset the bootstrapped data
      a <- adjroc(score = df$score, class = df$class, plot = FALSE)
      b <- adjroc(score = df$score2, class = df$class2, plot = FALSE)
      return(a$meeting_point - b$meeting_point)
    }
  }

  # create the dataframe
  # ============================================================
  df <- as.data.frame(cbind(score = score, class = class,
                            score2 = score2, class2 = class2))

  # run bootstrap adjroc
  # ============================================================
  results <- boot::boot(data = df, statistic = statistic, R = n, event_level = event_level)

  # compute the p-value
  # ============================================================
  D <- mean(results$t, na.rm = TRUE) / sd(results$t, na.rm = TRUE)
  p <- boot.pval::boot.pval(results, type = "norm", theta_null = 0)

  # mean and CI of adjroc
  # ============================================================
  mean <- mean(results$t)
  ci   <- boot::boot.ci(results, type = "norm")
  names(ci$normal) <- c("ci", "low", "high")
  cat("Mean (95% CI) =", mean, "(", ci$normal[1,2:3], ")\n")
  cat("D =", D, ",", "p-value =", p, ")\n\n")

  return(list(mean = mean,
              ci = ci$normal,
              sd = sd(results$boot$t),
              D = D,
              p = p,
              boot = results))
}



