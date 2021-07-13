##############################################################
# Ex 8, Q1

library(fields)
data8 = read.table("...", header=TRUE)

x = data8$x
y = data8$y

find_press = function(x, y, lambda){
	# Does cross-validation for this lambda value
	n = length(y)
	pred = vector(length=n) # vector for cross-validated predictions
	for(i in 1:n){
		modeli = sreg(x[-i], y[-i], lambda=lambda)
		pred[i] = predict(modeli, x[i])
	}
	return(sum((y-pred)^2)) # PRESS statistic
}

# part a
lambda = 10^(seq(from=-6, to=6, by=0.5))
lambda
press = vector(length=length(lambda))
for(i in 1:length(lambda)){
	press[i] = find_press(x, y, lambda[i])
}

plot(x=log10(lambda), y=press)

lambda0 = lambda[press==min(press)]
lambda0

# suppose that lambda0=31.6
# then try a range of lambda values around this value
lambda = seq(from=10, to=100, by=0.5)
press = vector(length=length(lambda))
for(i in 1:length(lambda)){
	press[i] = find_press(x, y, lambda[i])
}

# take the lambda value with the smallest PRESS to be
# the optimal lambda
lambda_opt = lambda[press==min(press)]
lambda_opt

modela = sreg(x, y, lambda=lambda_opt)

# b
modelb = sreg(x, y)
lambda_sreg = modelb$lambda
lambda_sreg
lambda_opt

# c
x_pred = seq(from=min(x), to=max(x), length.out=200)
y_preda = predict(modela, x_pred)
y_predb = predict(modelb, x_pred)
plot(x=x, y=y, col="blue")
lines(x=x_pred, y=y_preda, col="red")
lines(x=x_pred, y=y_predb, col="seagreen3", lty="dashed")
# making the last line "dashed" helps to see both curves
# if one is on top of the other

##############################################################
# Ex 8, Q2
library(bootstrap)
head(tooth)
summary(tooth)

n = nrow(tooth)
predd = vector(length=n)
for(i in 1:n){
	modeli = lm(strength ~ D1 + D2, tooth[-i,])
	predd[i] = predict(modeli, tooth[i,])
}
cvd = tooth$strength - predd
pressd = sum(cvd^2)

prede = vector(length=n)
for(i in 1:n){
	modeli = lm(strength ~ E1 + E2, tooth[-i,])
	prede[i] = predict(modeli, tooth[i,])
}
cve = tooth$strength - prede
presse = sum(cve^2)

pressd
presse
# The output is 3.91 and 4.23
# PRESS for model with D1 and D2 is lower


# Put the cross-validation code inside a function
# for bootstrapping. The function returns the difference in PRESS
# between the two models.
press_func = function(index){
	dt = tooth[index,] # bootstrap dataset
	n = nrow(dt)
	predd = vector(length=n)
	for(i in 1:n){
		modeli = lm(strength ~ D1 + D2, dt[-i,])
		predd[i] = predict(modeli, dt[i,])
	}
	cvd = dt$strength - predd

	prede = vector(length=n)
	for(i in 1:n){
		modeli = lm(strength ~ E1 + E2, dt[-i,])
		prede[i] = predict(modeli, dt[i,])
	}
	cve = dt$strength - prede

	return(sum(cvd^2) - sum(cve^2))
}

n = nrow(tooth)
NB = 2000
boot_press = bootstrap(1:n, nboot=NB, theta =press_func)

sd(boot_press$thetastar)
quantile(boot_press$thetastar, probs=c(0.025, 0.975))

bca_press = bcanon(1:n, nboot=NB, theta=press_func, alpha=c(0.025, 0.975))
bca_press$confpoints[,2]
# This confidence interval is around (-5.77, 3.62)
# This shows that it is very uncertain which model is better,
# based on the tooth dataset.

##############################################################
# Ex 8, Q3

# The spline models are fitted by minimizing the penalized
# sum of squares
# SP = S + lambda*P
# S is the ordinary sum of squares
# P is the penalty term. P = integral of (f'')^2 dx

# S is on the same scale as y^2
# f is on the same scale as y, since E(Y) = f
# So f'' is on the same scale as y/(x^2)
# So (f'')^2 is on the same scale as y^2/(x^4)
# So P is on the same scale as y^2/(x^3)
# Hence in order for lambda*P to be on the same
# scale as S, lambda must be on the same scale
# as x^3

# This suggests that if we multiply y by 10, the optimal
# lambda would not change.
# If we multiply x by 10, the optimal lambda would be
# multiplied by 1000.

library(bootstrap)
y = diabetes$logCpeptide
x = diabetes$age

lambda1 = sreg(x, y)$lambda
lambda2 = sreg(x, 10*y)$lambda
lambda3 = sreg(10*x, y)$lambda

lambda1
lambda2
lambda3
# This roughly confirms what was stated above.

##############################################################
# Ex 9, Q1

data9 = read.table("...", header=TRUE)
head(data9)

# a
n = nrow(data9)
ll_cv1 = vector(length=n)
for(i in 1:n){
	modeli = glm(success ~ sex, family=binomial, data=data9[-i,])
	pi = predict(modeli, newdata=data9[i,], type="response")
	yi = data9$success[i]
	ll_cv1[i] = yi*log(pi) + (1-yi)*log(1-pi)
}
sum(ll_cv1)

ll_cv2 = vector(length=n)
for(i in 1:n){
	modeli = glm(success ~ age, family=binomial, data=data9[-i,])
	pi = predict(modeli, newdata=data9[i,], type="response")
	yi = data9$success[i]
	ll_cv2[i] = yi*log(pi) + (1-yi)*log(1-pi)
}
sum(ll_cv2)

ll_cv3 = vector(length=n)
for(i in 1:n){
	modeli = glm(success ~ smoke, family=binomial, data=data9[-i,])
	pi = predict(modeli, newdata=data9[i,], type="response")
	yi = data9$success[i]
	ll_cv3[i] = yi*log(pi) + (1-yi)*log(1-pi)
}
sum(ll_cv3)

# The model with the largest sum(ll_cv) has the
# best predictive ability.

# b
# Suppose that the second model (with age) was the best
ll_cv4 = vector(length=n)
for(i in 1:n){
	modeli = glm(success ~ age+proc, family=binomial, data=data9[-i,])
	pi = predict(modeli, newdata=data9[i,], type="response")
	yi = data9$success[i]
	ll_cv4[i] = yi*log(pi) + (1-yi)*log(1-pi)
}
sum(ll_cv4)
# compare sum(ll_cv4) to sum(ll_cv2) to decide if adding proc
# improves the predictive ability

model4 = glm(success ~ age+proc, family=binomial, data=data9)
summary(model4)
model4$coefficients
# If the coefficient for proc is +ve, then the new procedure
# has a higher chance of success.
# If the coefficient for proc is -ve, then the old procedure
# has a higher chance of success.

# c

table(data9$proc)
# Suppose that 1 appears more often than 0
# Then we need the prediction at proc=1

# We also want the prediction at the mean age
ma = mean(data9$age)

# The linear predictor is beta0 + mean(age)*beta1 + beta2
eta = model4$coefficients[1] + ma*model4$coefficients[2] + model4$coefficients[3]

# The predicted probability of success is
exp(eta)/(1+exp(eta))

##############################################################
# Ex9, Q2

data7 = read.table("...", header=TRUE)
head(data7)

k = 5
n = nrow(data7)
batch = rep(1:k, length.out=n)
shuffled_batch = sample(batch)
cv_res = vector()
for(j in 1:k) {
	test_sample = which(shuffled_batch == j)
	modeli = lm(y ~ x1, data7[-test_sample,])
	predi = predict(modeli, data7[test_sample,])
	cv_res = c(cv_res, data7$y[test_sample]-predi)
}
PRESS1 = sum(cv_res^2)
PRESS1
# Could run this several times, and average the PRESS values

# Repeat with x2, x3 instead of x1, and similar code for 2b

##############################################################
