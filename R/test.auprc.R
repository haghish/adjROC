
test.auprc <- function(data, i) {
  require(yardstick)
  a <- yardstick::pr_auc_vec(data$class1[i],
                             data$score1[i])
  b <- yardstick::pr_auc_vec(data$class2[i], data$score2[i])
  return(a - b)
}
