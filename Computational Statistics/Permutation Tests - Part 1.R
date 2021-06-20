# Practical 2

library(perm)

################### Permutation Tests ###################################################################

A = c(6.8, 5.2)
B = c(2.3, 5.8, 3.1)


# alternative can be less or greater for alternative hypotheses
# method can be exact.mc for monte carlo
# tsmethod can be central for Plu (abs is for Pabs)

permTS(x=A, y=B, alternative="two.sided", method="exact.ce", control=permControl(tsmethod="abs"))
permTS(x=A, y=B, alternative="two.sided", method="exact.ce", control=permControl(tsmethod="central"))

# when length of A not equal to length of B i.e m!=n there will be difference in abs and central
# because for m!=n the null distribution is not symmetric

# Below listed for same length vectors

A = c(6.8, 5.2, 5.3)
B = c(2.3, 5.8, 3.1)

permTS(x=A, y=B, alternative="two.sided", method="exact.ce", control=permControl(tsmethod="abs"))
permTS(x=A, y=B, alternative="two.sided", method="exact.ce", control=permControl(tsmethod="central"))

# The above affects only for the two.sided case. The less or greater alternatives are not affected by 
# asymmetry in the null distribution as shown below

A = c(6.8, 5.2)
B = c(2.3, 5.8, 3.1)

permTS(x=A, y=B, alternative="less", method="exact.ce", control=permControl(tsmethod="abs"))
permTS(x=A, y=B, alternative="less", method="exact.ce", control=permControl(tsmethod="central"))

################### Importing Datasets ###################################################################

setwd("C:/QMUL/Computational Statistics with R/WEEK 2 - PERMUTATION TESTS")
data =  read.table("practical2.txt", header=TRUE)

# because m=n=30 and m+n C n is choose(60,30)
choose(60,30) 

################### Monte Carlo Permutation Tests ########################################################

A = data$control
B = data$treatment

permTS(x=A, y=B, alternative="two.sided", method="exact.mc", control=permControl(tsmethod="abs", nmc=100000))
t.test(x=A, y=B)

################### Permutation Test Hand Coded ##########################################################

A = c(6.8, 5.2)
B = c(2.3, 5.8, 3.1)

v = c(A, B)

m = length(A)
n = length(B)

# these two are complementary from the opposite sides as starting points
# useful for manual construction of table for manual perm test

C_A = combn(v, 2)
C_B = combn(v, 3)

C_A
C_B

NC = ncol(C_A)
tc = vector(length=NC)
s = sum(v)
for(i in 1:NC){
  xi = C_A[,i]     # choose ith column from C_B
  sx = sum(xi)
  sy = s - sx
  mx = sx/m
  my = sy/n
  tc[i] = mx - my      # test statistic for this sample
}

t0 = mean(A) - mean(B)
g_ABS = abs(tc)>=abs(t0)
k_ABS = sum(g)
p_ABS = k_ABS/NC
p_ABS
permTS(x=A, y=B, alternative="two.sided", method="exact.ce", control=permControl(tsmethod="abs"))

g_greater = tc >= t0
k_greater = sum(g_greater)
p_greater = k_greater/NC
p_greater
permTS(x=A, y=B, alternative="greater", method="exact.ce")


# This initially shows incorrect value because precision of t0 is not high enough.
# When doing manual calculation, ensure every variable has same level pf precision
g_less = t0 >= tc
k_less = sum(g_less)
p_less = k_less/NC
p_less
permTS(x=A, y=B, alternative="less", method="exact.ce")

# so using "Rmpfr" package we fix the precision level of all the floats in our calculations to 32 bits

NC = ncol(C_A)
tc = vector(mode="numeric", length=NC)
s = sum(v)
t0 = mpfr(mean(A), 32) - mpfr(mean(B), 32)
for(i in 1:NC){
  xi = C_A[,i]     # choose ith column from C_B
  sx = sum(xi)
  sy = s - sx
  mx = mpfr(sx/m, 32)
  my = mpfr(sy/n, 32)
  tc[i] = as.numeric(mpfr(mx - my, 32))      # even though it is converted to numeric, it retains the accuracy of mpfr
}

# compute k_less like this
k_less = 0
for(i in 1:NC){
  if(mpfr(tc[i], 32) <= t0){
    k_less = k_less + 1
  }
}
p_less = k_less/NC
p_less
permTS(x=A, y=B, alternative="less", method="exact.ce")
