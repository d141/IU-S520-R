# See s520-ch15-notes.html for fuller explanations

### Motivation: UCLA height-weight data

data = read.table("hw-ucla.txt", header=TRUE)
Height = data$Height
Weight = data$Weight
plot(Height, Weight, pch=".")
summary(data)
sd(Height)
sd(Weight)
cor(Height, Weight)
par(mfrow=c(1,2))
qqnorm(Height, main="Normal QQ plot of heights")
qqnorm(Weight, main="Normal QQ plot of weights")

# How heavy are six foot kids?

sixfoot = which(Height>71.5 & Height<72.5)
mean(Weight[sixfoot])
# Write a function
predict.weight = function(h){
  kids = which(Height>(h-0.5) & Height<(h+0.5))
  mean(Weight[kids])
}
height.list = seq(61, 75, 0.1)
weight.list = rep(NA, length(height.list))
for(J in 1:length(height.list)){
  weight.list[J] = predict.weight(height.list[J])
}
par(mfrow=c(1,1))
plot(Height, Weight, pch=".")
lines(height.list, weight.list, col="green")

predict.weight(mean(Height))
mean(Weight)

# SD line
b = sd(Weight) / sd(Height) ### NOT THE REGRESSION LINE
a = mean(Weight) - b * mean(Height)
plot(Height, Weight, pch=".")
lines(height.list, weight.list, col="green")
abline(a, b, col="blue")

# Regression line
b = cor(Height, Weight) * sd(Weight) / sd(Height)
a = mean(Weight) - b * mean(Height)
plot(Height, Weight, pch=".")
lines(height.list, weight.list, col="green")
abline(a, b, col="red")

# Regression to the mean

quantile(Height, 0.95)
quantile(Weight, 0.95)
h = quantile(Height, 0.95)
a + b * h
tallkids = which(Height>(h-0.5) & Height<(h+0.5))
plot(Height, Weight, cex=0.2, xlim=c(70,72), ylim=c(100,175))
points(Height[tallkids], Weight[tallkids], cex=0.2, col="red")
abline(h=quantile(Weight, 0.95), col="green")
abline(h=quantile(Weight, 0.8), col="blue")
hist(Weight[tallkids],
     main="Weights of kids near the 95th percentile of height",
     xlab="Weight",
     breaks=103:171)
abline(v=quantile(Weight, 0.95), col="green")
abline(v=quantile(Weight, 0.8), col="blue")

# Does the regression line work?

x = rnorm(1000)
y = - 5 + 3 * x + rnorm(1000)
# Intercept = -5, slope = 3: can we reproduce these?
r = cor(x, y)
print(r)
b = r * sd(y) / sd(x)
a = mean(y) - b * mean(x)
print(c(a,b))
plot(x, y)
abline(a, b, col="red")

x = rnorm(1000)
y = - 5 + 3 * x + rnorm(1000) * 10 # more error
r = cor(x, y)
print(r)
b = r * sd(y) / sd(x)
a = mean(y) - b * mean(x)
print(c(a, b))
plot(x, y)
abline(a, b, col="red")
# How good are the predictions?
errors = y - (a + b * x) # "residuals"
# Proportion of variance "unexplained"
var(errors) / var(y)
# Proportion of variance "explained"
1 - var(errors) / var(y)
r^2

# Using the bivariate normal for probability calculations

pnorm(3, mean=5, sd=2)
kids.69 = which(Height>68.5 & Height<69.5)
weights.69 = Weight[kids.69]
hist(weights.69)
qqnorm(weights.69)
slope = cor(Height, Weight) * sd(Weight) / sd(Height)
intercept = mean(Weight) - slope * mean(Height)
predict.69 = intercept + 69 * slope
print(predict.69)
predict.69 = intercept + 69 * s `1qa~lope
r = cor(Height, Weight)
pred.error = sd(Weight) * sqrt(1 - r^2)
pnorm(140, mean=predict.69, sd=pred.error)
# Check
mean(weights.69 < 140)

# Regression tests and confidence intervals

model = lm(Weight ~ Height)
summary(model)


### Checking regression assumptions

set.seed(320520)
x = rt(100, df=8)
y1 = 3 * x + rnorm(100)
y2 = 0.5 * x^2 + rnorm(100)
y3 = 3 * x + rnorm(100, sd=exp(x/5))
y4 = 3 * x + rt(100, df=4)

# LINEAR
plot(x, y1)
plot(x, y3)
plot(x, y4)
# NONLINEAR
plot(x, y2)
# Fit a better model

# HOMOSCEDASTIC
lm1 = lm(y1 ~ x)
plot(x, residuals(lm1))
lm4 = lm(y4 ~ x)
plot(x, residuals(lm4))
# HETEROSCEDASTIC
lm3 = lm(y3 ~ x)
plot(x, residuals(lm3))
# Bad for inference

# NORMAL ERRORS
qqnorm(residuals(lm1))
# NON-NORMAL ERRORS
qqnorm(residuals(lm4))
# Not a huge concern if large n (except for probabilities)



### Example: MLB batters

batting = read.table("batting.txt",
                     header=TRUE)
summary(batting)
plot(batting$OB2012, batting$OB2013)
# Model 1: Predict the mean
# for everyone
mean(batting$OB2013)
SST = sum((batting$OB2013 - mean(batting$OB2013))^2)
abline(h=mean(batting$OB2013), col="blue")

# Model 2: Predict using regression
slope = cor(batting$OB2012, batting$OB2013) *
  sd(batting$OB2013) / sd(batting$OB2012)
intercept = mean(batting$OB2013) -
  slope * mean(batting$OB2012)
predictions = intercept + slope*batting$OB2012
SSE = sum((batting$OB2013 - predictions)^2)
abline(intercept, slope, col="red")
# Ratio
SSE/SST
1 - cor(batting$OB2012, batting$OB2013)^2

# Model 3: Predict 2013 OB = 2012 OB
abline(0, 1, col="orange")
sum((batting$OB2013 - batting$OB2012)^2)
# Regression to the mean as usual

# Check heteroscedasticity
residuals = batting$OB2013 - predictions
plot(batting$OB2012, residuals)

# Inference for the slope
# Test hyp that slope = 0
se = sd(batting$OB2013)/sd(batting$OB2012) *
  sqrt((1-cor(batting$OB2012, batting$OB2013)^2)/
         (length(batting$OB2012)-2))
T = slope / se
# P-value
2 * (1-pt(T, length(batting$OB2012)-2))
# 95% CI
slope - qt(.975, df=length(batting$OB2012)-2)*se
slope + qt(.975, df=length(batting$OB2012)-2)*se

# F-test is the same thing
r = cor(batting$OB2012, batting$OB2013)
F = (length(batting$OB2012)-2)*r^2/(1-r^2)
# P-value
1 - pf(F, 1, length(batting$OB2012)-2)

# Prediction
# Robinson Cano:
# 2012: 264 OB, 697 AB
# 2013: ? OB, 681 AB
qqnorm(batting$OB2012)
qqnorm(batting$OB2013)
residuals = batting$OB2013 - predictions
qqnorm(residuals)
# Not quite normal

# Create two new ratio variables
obp2012 = batting$OB2012 / batting$PA2012
obp2013 = batting$OB2013 / batting$PA2013
qqnorm(obp2012)
qqnorm(obp2013)
plot(obp2012, obp2013)

# Fit regression
slope = cor(obp2012,obp2013)*sd(obp2013)/
  sd(obp2012)
intercept = mean(obp2013)-slope*mean(obp2012)
# Check
lm(obp2013 ~ obp2012)

cano2012 = 264/697
# Point prediction for 2013 OBP
slope * cano2012 + intercept
# Point prediction for 2013 OB
(slope * cano2012 + intercept) * 681
# Prediction error
resOBP = obp2013 - (slope*obp2012+intercept)
qqnorm(resOBP) # Pretty normal
SSE.OBP = sum(resOBP^2)
pred.error = sqrt(SSE.OBP/(length(obp2012)-2))
pred.error * 681


