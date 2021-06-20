# Tutorial 2


setwd("C:/QMUL/Computational Statistics with R/WEEK 2 - PERMUTATION TESTS")
data =  read.table("practical2.txt", header=TRUE)

A = data$control
B = data$treatment


# by default, perm test is done using asymptotic approximation
permTS(x=A, y=B)

# this does not work because 60 choose 30 is too large to iterate over
permTS(x=A, y=B, method="exact.ce")

# this does the monte carlo method with default number of MC replications = 999
permTS(x=A, y=B, alternative="two.sided", method="exact.mc")

# here we provide the number of MC replications manually
permTS(x=A, y=B, method="exact.mc", control=permControl(nmc=10000))

# if set seed is not FALSE then everytime the output will be the same
# the randomness will be "constant"
# but with setSEED as FALSE, there will be a new random MC result everytime it is executed
permTS(x=A, y=B, method="exact.mc", control=permControl(nmc=10000, setSEED=FALSE))

# below, we increase the number of MC replications to compare the abs and central method in the case of monte carlo
# note that they are not exactly the same here even though m=n=30 because each of these are an approximation
# to the null distribution and not the exact null dist. but the p values are fairly similar
permTS(x=A, y=B, method="exact.mc", alternative="two.sided", control=permControl(nmc=100000, setSEED=FALSE, tsmethod="abs"))
permTS(x=A, y=B, method="exact.mc", alternative="two.sided", control=permControl(nmc=100000, setSEED=FALSE, tsmethod="central"))

# comparison with t test as a sanity test, shown below
t.test(x=A, y=B, alternative="two.sided")

# always specify alternative explicitly, and other arguments manually as well. don't rely on defaults
permTS(x=A, y=B, method="exact.mc", control=permControl(nmc=100000, setSEED=FALSE, tsmethod="abs"))
permTS(x=A, y=B, method="exact.mc", alternative="two.sided", control=permControl(nmc=100000, setSEED=FALSE, tsmethod="abs"))
