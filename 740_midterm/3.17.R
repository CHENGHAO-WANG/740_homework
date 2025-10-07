############
# problem 3.17(a)
############

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

fit_ols <- ar.ols(cmort, order.max = 2, demean = FALSE, intercept = TRUE)
fit_ols

# Call:
# ar.ols(x = cmort, order.max = 2, demean = FALSE, intercept = TRUE)

# Coefficients:
#     1       2
# 0.4286  0.4418

# Intercept: 11.45 (2.394)

# Order selected 2  sigma^2 estimated as  32.32

fit_ols$asy.se.coef
# $x.mean
# [1] 2.393673

# $ar
# [1] 0.03979433 0.03976163

fit_yz <- ar.yw(cmort, order.max = 2)
fit_yz$x.mean # 88.69888
fit_yz$ar # 0.4339481 0.4375768
sqrt(diag(fit_yz$asy.var.coef)) # 0.04001303 0.04001303
fit_yz$var.pred # 32.842056

# They give different but very similar results, except for the intercept.
# This is because the Yule-Walker method estimates the intercept as the mean of the series,
# (which isn't an intercept actually)
# while the OLS method estimates the intercept as the mean of x_t minus AR coefficients times x_{t-1} and x_{t-2}.
# Therefore, as t increases, the intercept terms will get "accumulated" in x_t.
# This explains why the intercept by OLS is much smaller than by Yule-Walker.

# The AR coefficients estimates, standard errors and error variance estimates are similar,
# because the Yule-Walker estimators are optimal in the sense that its asymptotic distribution
# is the best asymptotic distribution.
# And this is because, given initial conditions, the AR(2) model is a linear model,
# and the Yule-Walker estimators are essentially least squares estimators.

############
# problem 3.17(b)
############

# standard errors obtained by OLS:
fit_ols$asy.se.coef
# $x.mean
# [1] 2.393673

# $ar
# [1] 0.03979433 0.03976163

# According to Property 3.9, Section B.4, Formula (3.132) and Property 3.7 in the texbook,
# Yule-Walker gives the asymptotic approximations.

sqrt(diag(fit_yz$asy.var.coef)) # 0.04001303 0.04001303