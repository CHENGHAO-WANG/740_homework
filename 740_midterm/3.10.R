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
# 0.43  0.44

# Intercept: 11.45 (2.39)

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
  Forecast = round(pred_mean, 2),
  Lower95 = round(lower, 2),
  Upper95 = round(upper, 2)
)

#  Week Forecast Lower95 Upper95
#    1    87.60   76.46   98.74
#    2    86.76   74.64   98.89
#    3    87.34   73.35  101.32
#    4    87.21   72.33  102.10

# This gives us the forecast for the next 4 weeks beyond the last week of the series.