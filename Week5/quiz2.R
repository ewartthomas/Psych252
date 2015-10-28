# A

F_1 = 56.9
F_0 = 38.2
SS_F1 = 8225.8
SS_F0 = 6995.4
SP = 5521.3
SD_D = 13.2
n = 25

## 1

tscore = (56.9-38.2)/(13.2/sqrt(25)); tscore
  
## 2
  
corr_F = SP/sqrt(SS_F1*SS_F0); corr_F
tstat = corr_F * sqrt((n-2)/(1-corr_F^2)); tstat
pt(q = tstat, df = n-2, lower.tail = F)

## 3
slope = SP/SS_F0; slope
intercept = F_1 - slope*F_0; intercept
SE_est = sqrt((SS_F1/(n-2)) * (1-(corr_F^2))); SE_est


## 4
estimate = intercept + 65 * slope; estimate
merr = qt((1-.68)/2, n-2, lower.tail=F)*SE_est; merr
estimate + merr
estimate - merr

# change
estimate - 65

## 5
estimate = intercept + 20 * slope

#change
estimate - 20


# C
sigma = 18
null = 0
ha = 8
n = 36
alpha = 0.025


power.t.test(n = 36, delta = 8, sd = 18, 
             sig.level = 0.025, 
             alternative = 'one.sided',
             type='one.sample')

sterr = sigma/sqrt(n); sterr
crit_z = qnorm(p = alpha, lower.tail = F)

# P(mean > 5.879892 | mu = 8)
null_val = null + crit_z*sterr; null_val
zval = (null_val - ha) / sterr; zval

# P(Z > -0.7067027)
pnorm(zval, lower.tail = F)



## 12

table_guilt = as.table(cbind(guilty=c(S1=30, S0=20), not_guilty=c(S1=30, S0=140-60-20)))
addmargins(table_guilt)
chisq.test(table_guilt, correct=F)
chisq.test(table_guilt, correct=T)

