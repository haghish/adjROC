
#' @title adjroc
#' @description computes adjusted sensitivity, adjusted specificity, or the crossing
#'     point between sensitivity and specificity for different thresholds
#' @importFrom  ROCit rocit
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab
#' @param score A numeric array of diagnostic score i.e. the estimated probability of each diagnosis
#' @param class A numeric array of equal length of \code{"score"}, including the actual class of the observations
#' @param method Specifies the method for estimating the ROC curve. Three methods are supported, which are \code{"empirical"}, \code{"binormal"}, and \code{"nonparametric"}
#' @param sensitivity numeric. Specify the threshold of sensitivity
#' @param specificity numeric. Specify the threshold of specificity
#' @param plot logical. if TRUE, the sensitivity and specificity will be plotted
#' @return data.frame including cutoff point, and adjusted sensitivity and specificity
#'     based on the specified threshold
#'@examples
#'# random classification and probability score
#'score <- runif(10000, min=0, max=1)
#'class <- sample(x = c(1,0), 10000, replace=TRUE)
#'
#'# calculate adjusted sensitivity, when specificity threshold is 0.90:
#'adjroc(score = score, class = class, specificity = 0.9, plot = TRUE)
#'
#'# calculate adjusted specificity, when sensitivity threshold equals 0.9
#'adjroc(score = score, class = class, sensitivity = 0.9, plot = TRUE)
#'
#'# calculate the meeting point between sensitivity and specificity
#'adjroc(score = score, class = class, plot = TRUE)
#' @export

adjroc <- function(score,
                   class,
                   method = "emp",
                   sensitivity = NULL,
                   specificity = NULL,
                   plot = FALSE
                   ) {

  suppressPackageStartupMessages({
    requireNamespace("ROCit")
    # requireNamespace("ggplot2")
  })

  DF <- NULL

  # Calculate the ROC curve
  # ============================================================
  roc <- ROCit::rocit(score = score, class = class, method = method)
  df <- as.data.frame(cbind(Cutoff=roc$Cutoff,
                            TPR = roc$TPR,
                            FPR = roc$FPR))

  # Calculate the Specificity
  # ============================================================
  df$FPR <- 1 - df$FPR
  colnames(df) <- c("cutoff", "sensitivity", "specificity")

  # Prepare the plot
  # ============================================================
  if (plot) {
    #plot(df$sensitivity~df$cutoff, type = "l", col = "hotpink", lwd = 2,
    #     xlab = "Cutoff", ylab = "")
    #lines(df$specificity~df$cutoff, type = "l", col = "royalblue", lwd = 2)
    #legend(x = "topright",
    #       lty = c(1,1),
    #       col = c("hotpink", "royalblue"),
    #       lwd = 2,
    #       bty = "n",
    #       cex = .75,
    #       xpd = TRUE,
    #       inset = c(0, -.2))

    # GGPLOT2
    plt1 <- df[, c("cutoff","sensitivity")]
    plt1$group <- "Sensitivity"
    names(plt1) <- c("cutoff","val","group")
    plt2 <- df[, c("cutoff","specificity")]
    plt2$group <- "Specificity"
    names(plt2) <- c("cutoff","val","group")
    plt <- rbind(plt1, plt2)
  }

  # Calculate the adjusted scores
  # ============================================================
  if (is.null(sensitivity) & is.null(specificity)) {
    # calculate the mean at the closest distance
    DF <- df[which.min(abs(df$specificity - df$sensitivity)), ]
    DF$meeting_point <- mean(c(DF$sensitivity, DF$specificity))
  }
  else if (is.null(sensitivity) & !is.null(specificity)) {
    DF <- df[which.min(abs(df$specificity - specificity)), ]
  }
  else if (!is.null(sensitivity) & is.null(specificity)) {
    DF <- df[which.min(abs(df$sensitivity - sensitivity)), ]
  }
  else {
    stop("sensitivity and specificity cannot be specified together")
  }

  # Plot the sensitivity and specificity curves
  # ============================================================
  if (plot) {
    cutoff <- NA #Rstudio gives annoying message in the build
    val    <- NA #Rstudio gives annoying message in the build
    group  <- NA #Rstudio gives annoying message in the build
    print(
      ggplot2::ggplot(plt, ggplot2::aes(x=cutoff, y=val, color=group)) +
        ggplot2::geom_line(size = 1) +
        ggplot2::ylab("Sensitivity and specificity\n") +
        ggplot2::xlab("\nCutoff") +
        ggplot2::geom_vline(xintercept = DF$cutoff, linetype="dashed", color = "gray", size=1) +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(size = 0.5, linetype = "solid", colour = "black"),
          legend.direction="horizontal",
          legend.position = "top"
        )
    )
  }

  # Calculate accuracy based on the specified cutoff
  # ============================================================
  TP <- sum(score >= DF$cutoff & class == 1)
  FP <- sum(score >= DF$cutoff & class != 1)
  TN <- sum(score < DF$cutoff & class != 1)
  FN <- sum(score < DF$cutoff & class == 1)

  DF$accuracy <- (TN+TP)/(TN+TP+FP+FN)
  DF$TP <- TP
  DF$TN <- TN
  DF$FP <- FP
  DF$FN <- FN

  # cat("\nNote: class 1 means positive cases\n")

  return(DF)
}



#score <- runif(10000, min=0, max=1)
#class <- sample(x = c(1,0), 10000, replace=T)
#adjroc(score = score, class = class, specificity = 0.2, plot = F)


