linear_model <- function(x_data, y_data)  {
  par(mar = c(2,2,2,1))
  plot(x_data, y_data, main = "Linear model", pch = 4, xlab = "", ylab = "")
  coef = coef(lm(y_data ~ x_data))
  abline(coef = coef)
  grid()
  coef
}