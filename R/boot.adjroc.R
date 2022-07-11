


boot.adjroc <- function(data, b,
                        method = "emp",
                        sensitivity = NULL,
                        specificity = NULL) {
  if (!is.null(sensitivity)) test <- "sensitivity"
  DF <- adjROC::adjroc(score = data$score[b],
                       class = data$class[b],
                       method = "emp",
                       sensitivity = sensitivity,
                       specificity = specificity)
  return(DF[, test])
}
