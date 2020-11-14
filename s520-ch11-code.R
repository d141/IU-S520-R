### Etruscan skulls

# Load the data
skulls = scan("http://mypage.iu.edu/~mtrosset/StatInfeR/Data/skulls.dat")
etruscan = skulls[1:84]
italian = skulls[85:154]

# Numerical summaries
summary(etruscan)
summary(italian)

# Plots
boxplot(etruscan, italian)
plot(density(etruscan))
lines(density(italian), col="red")

qqnorm(etruscan)
qqnorm(italian)

# Welch's two-sample t-test
Delta = mean(etruscan) - mean(italian)
se = sqrt(var(etruscan)/84 + var(italian)/70)
T.Welch = Delta/se
nu = (var(etruscan)/84+var(italian)/70)^2/
  ((var(etruscan)/84)^2/83+(var(italian)/70)^2/69)
P.value = 2*(1-pt(abs(T.Welch), df=nu))

# Welch 95% confidence interval
q = qt(0.975, df=nu)
lower = Delta - q*se
upper = Delta + q*se

# The magical easy way
t.test(etruscan, italian)
# If you must do Student's test:
t.test(etruscan, italian, var.equal=TRUE)


### Stereogram fusion times

# Data: Time to produce a fused stereogram image
stereograms = read.table("stereograms.txt", header=TRUE)

# Control: No visual information
# Treatment: Visual information
treatment = stereograms$time[stereograms$group==2]
control = stereograms$time[stereograms$group==1]

# Look at data
boxplot(treatment, control)
plot(density(treatment))
lines(density(control), col="red")
qqnorm(treatment)
qqnorm(control)

# Welch's test
t.test(treatment, control)
# Student's test
t.test(treatment, control, var.equal=TRUE)


# Nonparametric alternative
wilcox.test(treatment, control)
# If you're really pedantic:
# install.packages("coin")
library(coin)
wilcox_test(time ~ factor(group), data = stereograms, distribution = "exact")


# What if we took logs?
log.treatment = log(treatment)
log.control = log(control)
boxplot(log.treatment, log.control)
plot(density(log.treatment))
lines(density(log.control), col="red")
qqnorm(log.treatment)
qqnorm(log.control)
t.test(log.treatment, log.control)
t.test(log.treatment, log.control, var.equal=T)

# Confidence interval: -0.80 to -0.06
# Take the exponential: 0.45, 0.94
# The treatment reduces fusion time by 6% to 55%

