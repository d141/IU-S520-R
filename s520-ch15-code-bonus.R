### Playing around with residual plots

# Linear, homoskedastic (good)

x = rnorm(400, mean = 4)
y = x + rnorm(400)
plot(x, residuals(lm(y ~ x)))

# Linear, heteroskedastic (could be better)

x = rnorm(400, mean = 4)
y = x + x * rnorm(400)
plot(x, residuals(lm(y ~ x)))

# Nonlinear, homoskedastic (bad)

x = rnorm(400)
y = 0.2 * x^2 + rnorm(400)
plot(x, residuals(lm(y ~ x)))

# Nonlinear, heteroskedastic (really bad)

x = rnorm(400)
y = exp(x + rnorm(400))
plot(x, residuals(lm(y ~ x)))



### Real(er) height and weight

# install.packages("NHANES")
library(NHANES)
plot(NHANES$Height, NHANES$Weight)
Adults = NHANES[NHANES$Age >= 18,]
plot(Adults$Height, Adults$Weight)
qqnorm(Adults$Height)
qqnorm(Adults$Weight)
qqnorm(log(Adults$Weight))
plot(Adults$Height, log(Adults$Weight))
log.model = lm(log(Weight) ~ Height,
               data=Adults)
log.model
abline(log.model, col="red")
# Do the residuals look okay?
coefs = coefficients(log.model)
fitted = coefs[1] +
  coefs[2] * Adults$Height
resids.logmodel = log(Adults$Weight) -
  fitted
plot(Adults$Height, resids.logmodel)
qqnorm(resids.logmodel)
# Model assumptions:
# Linear: close enough
# Independent: presumably
# Equal variance: close
# Normality: not quite

# Is the regression line
# a good description of the data?
# - yes
# Can we do classical intervals, tests, etc.?
# - yes
# (though there might be better approaches)
# Can we do probabilistic prediction
# using the normal?
# - no

# Back to original scale
plot(Adults$Height, Adults$Weight,
     main="Median weight by height")
height.dummy = seq(130, 210, by=0.1)
logweight.dummy = coefs[1] +
  coefs[2] * height.dummy
weight.dummy = exp(logweight.dummy)
lines(height.dummy, weight.dummy, col="red")

### Stuff from here is mostly nonexaminable
# To get the mean: Use a nonparametric method
loess.model = loess(Weight ~ Height,
                    data=Adults)
weight.loess = predict(loess.model,
                       data.frame(Height=height.dummy))
lines(height.dummy, weight.loess,
      col="yellow")
# For more: Take S625

# What about age and weight?
plot(Adults$Age, Adults$Weight, pch=".")
loess.age = loess(Weight ~ Age,
                  data=Adults)
age.dummy = 18:80
weightfromage.loess = predict(loess.age,
                              data.frame(Age=age.dummy))
lines(age.dummy, weightfromage.loess,
      col="red")
# It's a curve:
# linear regression ain't gonna work
# Some solutions:
# Use nonparametric stuff
#  (above, see also S625)
# Use a categorical version of age
boxplot(log(Weight) ~ AgeDecade, data=Adults)
lm(log(Weight) ~ AgeDecade, data=Adults)
# Much better if you use height as well
lm(log(Weight) ~ Height + AgeDecade,
   data=Adults)
# Learn multiple regression in S631
# Bayesian regression in S626
# Which is better? It depends

# Does gender matter?
boxplot(Weight ~ Gender, data=Adults)
# Does gender matter
# after accounting for height?
color.list = rep("white", length(Adults$Gender))
color.list[Adults$Gender == "male"] = "blue"
color.list[Adults$Gender == "female"] = "pink"
plot(Adults$Height, Adults$Weight,
     col=color.list, pch=".")
# Women are typically shorter
# but is the relationship between height
# and weight the same?
# Simplest approach: Fit two models,
# compare
male.model = lm(log(Weight) ~ Height,
  data=Adults[Adults$Gender=="male",])
female.model = lm(log(Weight) ~ Height,
  data=Adults[Adults$Gender=="female",])
summary(male.model)
summary(female.model)
plot(Adults$Height, log(Adults$Weight))
abline(male.model, col="blue")
abline(female.model, col="pink")
# Multiple regression
summary(lm(log(Weight) ~ Height * Gender,
           data=Adults))
# To learn wtf this actually means,
# take S631

# Can you predict gender from
# height/weight?
# "Gender" isn't Normal...
# Need *generalized* linear models
gender.model = glm(Gender ~ Height + Weight,
                   data=Adults, family=binomial,
                   na.action=na.exclude)
gender.fit = fitted.values(gender.model)
gender.predix = round(gender.fit)
plot(Adults$Height, Adults$Weight,
     type="n")
points(Adults$Height[gender.predix==0],
       Adults$Weight[gender.predix==0],
       col="pink", pch=".")
points(Adults$Height[gender.predix==1],
       Adults$Weight[gender.predix==1],
       col="blue", pch=".")
# Not the best classifier ever...
# To learn classification
# take S675 or S632
# To learn how to draw better graphs
# of this stuff
# take S670
