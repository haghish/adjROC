#' @title boot.adjroc
#' @description computes bootstrap adjusted sensitivity, bootstrap adjusted
#'     specificity, or bootstrap crossing point between sensitivity and
#'     specificity for different thresholds
#' @importFrom stats sd
#' @importFrom boot boot boot.ci
#' @param score A numeric array of diagnostic score i.e. the estimated probability of each diagnosis
#' @param class A numeric array of equal length of \code{"score"}, including the actual class of the observations
#' @param n number of bootstrap samples.
#' @param method Specifies the method for estimating the ROC curve. Three methods are supported, which are \code{"empirical"}, \code{"binormal"}, and \code{"nonparametric"}
#' @param sensitivity numeric. Specify the threshold of sensitivity.
#' @param specificity numeric. Specify the threshold of specificity.
#' @return list including mean and CI of bootstrap value (sensitivity, specificity, or
#'     the crossing point) and the bootstrap data.
#'@examples
#'# random classification and probability score
#'score <- runif(10000, min=0, max=1)
#'class <- sample(x = c(1,0), 10000, replace=TRUE)
#'
#'# calculate adjusted sensitivity, when specificity threshold is 0.90:
#'adjroc(score = score, class = class, specificity = 0.9, plot = TRUE)
#'
#'# calculate adjusted specificity, when sensitivity threshold equals 0.9
#'boot.adjroc(score = score, class = class, n = 100, sensitivity = 0.9)
#'
#'# calculate the bootstrap meeting point between sensitivity and specificity
#'boot.adjroc(score = score, class = class, n = 100)
#' @export

boot.adjroc <- function(score,
                        class,
                        n = 100,
                        method = "emp",
                        sensitivity = NULL,
                        specificity = NULL) {

  # define the statistics function
  # ============================================================
  statistic <- function(df = df,
                        method = "emp",
                        sensitivity = sensitivity,
                        specificity = specificity,
                        indices,
                        ...) {

    # subset the bootstrapped data
    df <- df[indices, ]

    # run adjroc
    perf <- adjroc(score = df$score,
                   class = df$class,
                   method = "emp",
                   sensitivity = sensitivity,
                   specificity = specificity)

    # Specify the metric of interest
    # ----------------------------------------------------------
    if (!is.null(specificity)) test <- "sensitivity"
    else if (!is.null(sensitivity)) test <- "specificity"
    else test <- "meeting_point"

    return(as.numeric(perf[test]))
  }

  # create the dataframe
  # ============================================================
  df <- as.data.frame(cbind(score = score,
                            class = class))

  # run bootstrap adjroc
  # ============================================================
  results <- boot::boot(data = df,
                        statistic = statistic,
                        R = n,
                        method = method,
                        sensitivity = sensitivity,
                        specificity = specificity)

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



