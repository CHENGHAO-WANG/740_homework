
############
# (C) Example 3.30
############

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(astsa)

fit_mle <- ar.mle(cmort, order.max = 2)
fit_mle$x.mean # 88.70
fit_mle$ar # 0.43 0.44
sqrt(diag(fit_mle$asy.var.coef)) # 0.04 0.04
fit_mle$var.pred # 32.37
