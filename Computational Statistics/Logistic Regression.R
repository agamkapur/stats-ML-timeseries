###############################################
#                                             #
# Exercise Sheet 9 (Agam Kapur - 200861845)   #
#                                             #
###############################################

library(bootstrap)

treatment_data = read.csv("~/R/exercise9_200861845.txt", sep = "")

################      Q1 a)       ############################################################

n = nrow(treatment_data)

calculate_log_likelihood = function(yi, pi){
  l = yi*log(pi) + (1-yi)*log(1-pi)
  return(l)
}

log_likelihood_sex = vector(length=n, mode="numeric")
log_likelihood_age = vector(length=n, mode="numeric")
log_likelihood_smoke = vector(length=n, mode="numeric")

for(i in 1:n){
  
  model_with_sex = glm(success ~ sex, family=binomial, data=treatment_data[-i,])
  pi_sex = predict(model_with_sex, newdata = treatment_data[i,], type="response")
  log_likelihood_sex[i] = calculate_log_likelihood(treatment_data[i,]$success, pi_sex)
  
  model_with_age = glm(success ~ age, family=binomial, data=treatment_data[-i,])
  pi_age = predict(model_with_age, newdata = treatment_data[i,], type="response")
  log_likelihood_age[i] = calculate_log_likelihood(treatment_data[i,]$success, pi_age)
  
  model_with_smoke = glm(success ~ smoke, family=binomial, data=treatment_data[-i,])
  pi_smoke = predict(model_with_smoke, newdata = treatment_data[i,], type="response")
  log_likelihood_smoke[i] = calculate_log_likelihood(treatment_data[i,]$success, pi_smoke)
  
}

sum(log_likelihood_sex)
sum(log_likelihood_age)
sum(log_likelihood_smoke)

################      Q1 b)       ############################################################

log_likelihood_smoke_proc = vector(length=n, mode="numeric")

for(i in 1:n){
  
  model_with_smoke_proc = glm(success ~ smoke + proc, family=binomial, data=treatment_data[-i,])
  pi_smoke_proc = predict(model_with_smoke_proc, newdata = treatment_data[i,], type="response")
  log_likelihood_smoke_proc[i] = calculate_log_likelihood(treatment_data[i,]$success, pi_smoke_proc)
  
}

sum(log_likelihood_smoke_proc)

model_with_smoke_proc = glm(success ~ smoke + proc, family=binomial, data=treatment_data)

model_with_smoke_proc

################      Q1 c)       ############################################################

table(treatment_data$proc)
table(treatment_data$smoke)

predict(model_with_smoke_proc, newdata = treatment_data[1,], type="response")