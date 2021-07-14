###############################################
#                                             #
# Exercise Sheet 8 (Agam Kapur - 200861845)   #
#                                             #
###############################################

library(fields)
library(bootstrap)

data = read.csv("~/R/exercise8_200861845.txt", sep = "")

y = data$y
x = data$x

################      Q1 a)       ############################################################

find_press_statistic = function(x, y, lambda){
  n = length(y)
  pred = vector(length=n)
  for(i in 1:n){
    modeli = sreg(x[-i], y[-i], lambda=lambda)
    pred[i] = predict(modeli, x[i])
  }
  cv_res = y - pred
  press = sum(cv_res^2)
  return(press)
}

find_and_plot_lambda_in_range = function(from, to, by){
  lambda = 10^(seq(from=from, to=to, by=by))
  press = vector(length=length(lambda))
  for(j in 1:length(lambda)){
    press[j] = find_press_statistic(x, y, lambda[j])
  }
  plot(x=log10(lambda), y=press)
  return(lambda[press==min(press)])
}

# At each step we increment the number of significant digits by 1

lambda = find_and_plot_lambda_in_range(-6, 6, 0.5)
lambda = find_and_plot_lambda_in_range(1, 2, 0.1)
lambda = find_and_plot_lambda_in_range(1.4, 1.5, 0.01)
lambda = find_and_plot_lambda_in_range(1.43, 1.44, 0.001)
lambda = find_and_plot_lambda_in_range(1.434, 1.436, 0.0001)

#optimal lambda
lambda

#press statistic for this lambda calculated as
find_press_statistic(x, y, lambda)


################      Q1 b)       ############################################################

# optimal lambda as calculated using sreg
model_generalized_cross_validation = sreg(x, y)
model_generalized_cross_validation$lambda

# press statistic for this optimal lambda
find_press_statistic(x, y, model_generalized_cross_validation$lambda)

################      Q1 c)       ############################################################

model0 = sreg(x, y, lambda)

x_pred = seq(from=0, to=50, by=0.05)
y_pred = predict(model0, x_pred)
y_pred_gcv = predict(model_generalized_cross_validation, x_pred)
plot(x=x, y=y, col="blue")
lines(x=x_pred, y=y_pred, col="red")
lines(x=x_pred, y=y_pred_gcv, col="seagreen3")

# As we can see, the red and green lines are almost the same