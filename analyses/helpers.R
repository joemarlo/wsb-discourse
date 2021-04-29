# create special notin function
`%notin%` <- negate(`%in%`)

# root mean squared error
RMSE <- function(y, y_hat) sqrt(mean((y_hat - y)^2))
