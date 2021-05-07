# create special notin function
`%notin%` <- negate(`%in%`)

# root mean squared error
RMSE <- function(y, y_hat) sqrt(mean((y_hat - y)^2))

# semi monthly date breaks
semi_monthly <- function(x) {
  x_range <- range(x, na.rm = TRUE)
  
  date_range <- c(
    floor_date(x_range[1], "month"),
    ceiling_date(x_range[2], "month")
  )
  monthly <- seq(date_range[1], date_range[2], by = "1 month")
  
  sort(c(monthly, monthly + days(14)))
}
