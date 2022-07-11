

bootpval <- function(x) {
  require(boot.pval)
  D <- mean(x$t, na.rm = TRUE) / sd(x$t, na.rm = TRUE)
  p <- boot.pval::boot.pval(x, type = "norm", theta_null = 0)
  result <- list(D = D, p = p)
  return(result)
}
