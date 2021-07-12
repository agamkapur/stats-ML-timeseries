#########################################################
# Bootstrap predictions for a linear model
library(bootstrap)

model1 = lm(LSAT ~ GPA, data=law)
summary(model1)
summary(law)

pred3 = predict(model1, newdata=data.frame(GPA=3))
pred3

beta0 = model1$coefficients[1]
beta1 = model1$coefficients[2]
pred3_check = beta0 + beta1*3

n = nrow(law)
N = 5000
bootreps = vector(length=N, mode="numeric")
for(i in 1:N){
	index = sample(1:n, replace=TRUE)
	law_boot = law[index,]
	modeli = lm(LSAT ~ GPA, data=law_boot)
	pred3 = predict(modeli, newdata=data.frame(GPA=3))
	bootreps[i] = pred3
}

# bootstrapping cases results:
# bootstrap standard error
sd(bootreps)

# 95% percentile CI
quantile(bootreps, probs=c(0.025, 0.975))

# linear model CI
predict(model1, newdata=data.frame(GPA=3), interval="confidence")

#########################################################
# Bootstrap cases, but using a function so that we
# can find BCA confidence interval
BetaHat = function(units, boot_data){
	m = lm(LSAT ~ GPA, data=boot_data[units,])
	pred3 = predict(m, newdata=data.frame(GPA=3))
	return(pred3)
}

N = 2000
n=nrow(law)
lm_bca = bcanon(1:n, BetaHat, nboot=N, law, alpha=c(0.025, 0.975))

lm_bca$confpoints[,2]

#########################################################
# Bootstrap a vector of predictions
library(bootstrap)


n = nrow(law)
N = 1000
pred_x = seq(from=min(law$GPA), to=max(law$GPA), length=100)

# matrix of bootstrap replications
boot_pred = matrix(nrow=N, ncol=length(pred_x))
for(i in 1:N){
	index = sample(1:n, replace=TRUE)
	law_boot = law[index,]
	modeli = lm(LSAT ~ GPA, data=law_boot)
	boot_pred[i,] = predict(modeli, newdata=data.frame(GPA=pred_x))
}


# matrix of percentile CIS for each prediction
pred_ci = matrix(nrow=length(pred_x), ncol=2)
for(j in 1:nrow(pred_ci)){
	pred_ci[j,] = quantile(boot_pred[,j], probs=c(0.025, 0.975))
}

# plot original data
plot(x=law$GPA, y=law$LSAT)

# add bootstrap CIs to plot
lines(x=pred_x, y=pred_ci[,1], col="red")
lines(x=pred_x, y=pred_ci[,2], col="red")

# prediction with normal theory CIs
pred_lm = predict(model1, newdata=data.frame(GPA=pred_x), interval="confidence")

dim(pred_lm)

# add prediction and normal theory CIs to plot
lines(x=pred_x, y=pred_lm[,1], col="brown")
lines(x=pred_x, y=pred_lm[,2], col="blue")
lines(x=pred_x, y=pred_lm[,3], col="blue")

#########################################################
