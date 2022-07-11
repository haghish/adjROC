
boot.auprc <- function(data, i) {
  require(yardstick)
  return(yardstick::pr_auc_vec(data$class[i],
                               data$score[i]))
}
