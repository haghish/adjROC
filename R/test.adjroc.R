

test.adjroc<- function(data, i,
                       method = "emp",
                       sensitivity = NULL,
                       specificity = NULL) {

  # define the test
  if (!is.null(sensitivity)) test <- "sensitivity"
  else if (!is.null(specificity)) test <- "specificity"

  a <- adjROC::adjroc(score = data$score1[i],
                       class = data$class1[i],
                       method = "emp",
                       sensitivity = sensitivity,
                       specificity = specificity)
  a <- a[, test]

  b <- adjROC::adjroc(score = data$score2[i],
                      class = data$class2[i],
                      method = "emp",
                      sensitivity = sensitivity,
                      specificity = specificity)
  b <- b[, test]

  return(a - b)
}
