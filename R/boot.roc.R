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
  cat("\nmean (CI) =", mean, "(", ci$normal[1,2:3], ")\n\n")

  return(list(mean = mean,
              ci = ci$normal,
              sd = sd(results$boot$t),
              boot = results))
}
