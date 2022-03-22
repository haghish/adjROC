`adjROC` : Computing Sensitivity at a Fix Value of Specificity and Vice Versa
============================================================================================================

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
