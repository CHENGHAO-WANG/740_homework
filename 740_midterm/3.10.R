############
# problem 3.10(a)
############

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(astsa)

fit_ar2 <- ar.ols(cmort, order.max = 2, demean = FALSE, intercept = TRUE)
fit_ar2

# Call:
# ar.ols(x = cmort, order.max = 2, demean = FALSE, intercept = TRUE)

# Coefficients:
#     1       2
# 0.4286  0.4418

# Intercept: 11.45 (2.394)

# Order selected 2  sigma^2 estimated as  32.32

############
# problem 3.10(b)
############

forecasts <- predict(fit_ar2, n.ahead = 4)
forecasts

pred_mean <- forecasts$pred
pred_se   <- forecasts$se

upper <- pred_mean + 1.96 * pred_se
lower <- pred_mean - 1.96 * pred_se

data.frame(
  Week = 1:4,
  Forecast = pred_mean,
  Lower95 = lower,
  Upper95 = upper
)

#  Week Forecast  Lower95   Upper95
#    1 87.59986 76.45756  98.74217
#    2 86.76349 74.64094  98.88604
#    3 87.33714 73.35405 101.32022
#    4 87.21350 72.33052 102.09648

# This gives us the forecast for the next 4 weeks beyond the last week of the series.