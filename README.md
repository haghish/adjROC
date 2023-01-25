> Haghish, E. F. (2022). adjROC: Computing Sensitivity at a Fix Value of Specificity and Vice Versa [Computer software]. https://CRAN.R-project.org/package=adjROC.

- - -

<a href="https://github.com/haghish/adjROC"><img src='man/figures/logo.png' align="right" height="200" /></a>

`adjROC`: ROC Curve Evaluation at a Given Threshold
============================================================================================================

[![CRAN version](http://www.r-pkg.org/badges/version/adjROC?color=258076)](https://cran.r-project.org/package=adjROC)  [![](https://cranlogs.r-pkg.org/badges/grand-total/adjROC?color=e8a0c6)](https://cran.r-project.org/package=adjROC) [![](https://raw.githubusercontent.com/haghish/mlim/main/man/figures/manual.svg)](https://CRAN.R-project.org/package=adjROC)



`adjROC` is an R package for computing adjusted sensitivity and specificity at particular thresholds. There are 
methods for estimating the best balance betwen sensitivity and specificity. However, in clinical settings, 
there might be an interest in calculating the sensitivity based on a particular fixed value of specificity or 
in contrast, calculating specificity for a particular value of sensitivity which is of interest. 

For a screening test, specificity of 0.95 might be too high and lower values of specificity may also be acceptable. 
In another settings, researchers might wish to know the mount of specificity, while keeping sensitivity high. And finally, 
in some situations, a roughly equal value might be desired. Depending on the application, `adjROC` package allows 
users to calculate:

- Specificity at a particular fix value of specificity
- Specificity at the specified value of sensitivity
- The crossing point (meeting point) between sensitivity and specificity curves

and on top of these, it can also visualize the curves and the selected cutoff threshold. 
