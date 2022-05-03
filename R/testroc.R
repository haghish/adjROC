library(boot)
library(adjROC)

compare.adjroc <- function(df,
                           method = "emp",
                           sensitivity = NULL,
                           specificity = NULL) {

  if (!is.null(sensitivity)) test <- "specificity"
  else if (!is.null(specificity)) test <- "sensitivity"
  index1 <- df$group==1
  index2 <- df$group==2
  #print(sensitivity)
  #print(specificity)
  #print(test)
  DF1 <- adjROC::adjroc(score = df$score[index1], class = df$class[index1], method = "emp",
                        sensitivity = sensitivity, specificity = specificity)
  DF2 <- adjROC::adjroc(score = df$score[index2], class = df$class[index2], method = "emp",
                        sensitivity = sensitivity, specificity = specificity)

  #print(DF1[, test] - DF2[, test])
  return(DF1[, test] - DF2[, test])
}


boot.adjroc <- function(df,
                        method = "emp",
                        sensitivity = NULL,
                        specificity = NULL) {
  if (!is.null(sensitivity)) test <- "specificity"
  else if (!is.null(specificity)) test <- "sensitivity"
  DF <- adjROC::adjroc(score = df$score, class = df$class, method = "emp",
                        sensitivity = sensitivity, specificity = specificity)

  return(DF[, test])
}

score <- runif(1000, min=0, max=1)
class <- sample(x = c(1,0), 1000, replace=T)
boot(data = as.data.frame(cbind(score,class)), statistic = boot.adjroc, R = 100, stype = "i", sim = "ordinary", specificity = .9)



boot(df, compare.adjroc, R = 10, stype = "i", sim = "ordinary", specificity = .9)

testroc <- function(score1,score2,
                   class1,class2=NULL,
                   test = NULL,
                   boot.n = 20,
                   method = "emp",
                   sensitivity = NULL,
                   specificity = NULL
) {

  suppressPackageStartupMessages({
    requireNamespace("ROCit")
    requireNamespace("boot")
    requireNamespace("boot.pval")
  })

  # Create the combined dataset
  # ============================================================
  results <- NULL
  if (is.null(class2)) class2 <- class1
  # create a dataset for boot package
  df1 <- as.data.frame(cbind(score1, class1))
  df1$group <- 1
  names(df1) <- c("score","class","group")
  df2 <- as.data.frame(cbind(score2, class2))
  df2$group <- 2
  names(df2) <- c("score","class","group")
  df <- rbind(df1, df2)

  # Syntax check
  # ============================================================
  teststat <- c("sensitivity", "specificity", "accuracy")
  teststat2 <- c("TP", "TN", "FP", "FN")
  if (test %in% teststat) {
    boot <- boot(df, compare.adjroc, R = boot.n, stype = "i", sim = "ordinary")
  } else if (test %in% teststat2) {
    # group comparison based on categories

  } else {
    stop(paste(test, "is unrecognized"))
  }


  return(results)
}

score1 <- runif(10, min=0, max=1)
score2 <- runif(10, min=0, max=1)
class1 <- sample(x = c(1,0), 10, replace=T)
class2 <- sample(x = c(1,0), 10, replace=T)
df1 <- as.data.frame(cbind(score1, class1))
df1$group <- 1
names(df1) <- c("score","class","group")
df2 <- as.data.frame(cbind(score2, class2))
df2$group <- 2
names(df2) <- c("score","class","group")
df <- rbind(df1, df2)

booty <- boot(df, compare.adjroc, R = 3, stype = "i", sim = "ordinary")

#adjroc(score = score, class = class, specificity = 0.2, plot = F)
#
#roc <- ROCit::rocit(score = score, class = class)
#df <- as.data.frame(cbind(Cutoff=roc$Cutoff,
#                          TPR = roc$TPR,
#                          FPR = roc$FPR))


#library(boot.pval)
#ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)
#city.boot <- boot(city, ratio, R = 99, stype = "w", sim = "ordinary")
#boot.pval(city.boot, theta_null = 1.4)
