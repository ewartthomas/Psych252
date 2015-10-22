### Chisquared
###########################
# goodness of fit (1-way classification)
group_proportions = c(.3, .3, .4)
group_actual = c(103, 78, 89)
rs1 = chisq.test(group_actual, p=group_proportions, rescale.p=T, simulate.p.value=F)

# contingency (2-way classification)
data0 = matrix(c(57, 51, 46, 27), byrow=T,ncol=2) # 2x2 table
#data0 = matrix(c(57, 51, 46, 27, 24, 23), byrow=T,ncol=3) # 2x3 table
print(data0)
cTest = chisq.test(data0)
print(cTest)

### General linear models using lm()
###########################
X1 =           # continuous predictor var
X2 =           # factor var (group)
X0 =           # yvar

# test for = variances
rs1a = bartlett.test(X0 ~ X2, data=d0, na.action=na.omit)

print(rs1a)

res1 = lm(X0 ~ X1 + X2) # additive, linear
res1_inter = lm(X0 ~ X1 * X2) # interactive, linear
res2 = lm(X0 ~ poly(X1, 2) + X2) # additive w/quad term
res3 = lm(X0 ~ poly(X1, 2) * X2) # interactive w/quad

anova(res1, res2, res3)
summary(res1)

# testing for nonlinear effects
summary(lm(X0~scale(X1)+I(scale(X1)^2))) # center X1, so slope is at mean X1

# interaction continuous & categorical vars
summary(lm(X0~X1*X2))
summary(lm(X0~I(X1-mean(X1))*X2)) # X1=continuous, center it

# model comparison
anova(res1, res1_inter, res2, res3)

### Basic statistics
###########################

# Population
d0 = c() # unique values that occur
p0 = c() # probability value occurs# occuring
mu0 = sum(d0*p0)/sum(p0)
var0 = sum(p0*(d0-mu0)^2)/sum(p0)
sigma0 = sqrt(var0)

# Sample
values = c(1, 2, 3, 4, 2, 1)

n = length(values); n
df = n-1; df
mean = sum(values)/n; mean
SS = sum((values - mean)^2); SS
variance = SS/df; variance
sd = sqrt(variance); sd

#####################
### Calculate R coeff
x = rnorm(n=25, m = 10, sd = 3); x = sort(x)
y = rnorm(n=25, m = 30, sd = 5); y = sort(y)
plot(x, y)

Sx = sd(x)
Sy = sd(y)

N = length(x)
X_sum = sum(x)
Y_sum = sum(y)
X_mean = X_sum/N
Y_mean = Y_sum/N
SSx = sum((x - X_mean)^2)
SSy = sum((y - Y_mean)^2)
SP =
SP = sum((x- X_mean)*(y- Y_mean))
cov_XY = SP/(N-1)
SSx =
SSy =
Sx =
Sx = sqrt(SSx/(N-1))
Sy =
Sy = sqrt(SSy/(N-1))

r = SP/sqrt(SSx*SSy)
r = cov_XY/(Sx*Sy)

## intercept/slope

slope = SP/SSx
slope = cov_XY/(Sx^2)

intercept = Y_mean - slope*X_mean

#line of best fit
xvals = 6:16
plot(xvals, slope*xvals+intercept);

### distributions
###########################
x =  # z, t, or chi-square
pvalue = # p-value
df = # for t or chi-squared

pvalue = pnorm(x, mean=0, sd=1, lower.tail=F)
zscore = qnorm(pvalue, mean=0, sd=1, lower.tail=F)

pvalue = pt(x, df=10, lower.tail=F)
tscore = qt(pvalue, df=df, lower.tail=F)

pvalue = pchisq(x, df=10, lower.tail=F)
chisquared = qchisq(pvalue, df=df)
