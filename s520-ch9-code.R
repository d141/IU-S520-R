### Confidence intervals: Choral singers

singer = read.table("singer.txt", header=TRUE)

# 90% CI for the mean
x_bar = mean(singer$height)
q = qnorm(.95)
s = sd(singer$height)
n = length(singer$height)
# Lower bound
x_bar - q * s / sqrt(n)
# Upper bound
x_bar + q * s / sqrt(n)

# 80% CI for the proportion 6 feet or taller
x_bar = sum(singer$height >= 72) / n
q = qnorm(.90)
# Lower bound
x_bar - q * sqrt(x_bar * (1 - x_bar) / n)
# Upper bound
x_bar + q * sqrt(x_bar * (1 - x_bar) / n)



# Do confidence intervals work? A simulation

p = 0.2
p.hats = rbinom(10000,235,p)/235
lower.bounds = p.hats - q * sqrt(p.hats*(1 - p.hats)/n)
upper.bounds = p.hats + q * sqrt(phats*(1 - p.hats)/n)
summary(lower.bounds)
summary(upper.bounds)
sum(p < lower.bounds) # should be 10% of the time
sum(p > upper.bounds) # should be 10% of the time
# A bit off



### Hypothesis testing or Significance testing

# I toss a coin 1000 times and get 489 heads
# Is this enough to show that it's biased?

# Compare to simulation
heads = rbinom(20000, 1000, 0.5)
summary(heads)
hist(heads, prob=T,
     breaks=(min(heads)-0.5):(max(heads)+0.5))

# Compare to binomial distribution
x = 440:560
f = dbinom(x, 1000, 0.5)
plot(x, f, type="h")
abline(v = 489, col="red")

# 489 is not at all unusual
# What about 544?
abline(v = 544, col="green")

# This is unusual. How unusual?
# Prob. of 544 or more heads:
1 - pbinom(543, 1000, 0.5)
# Prob. of being at least 44 heads from 500:
pbinom(456, 1000, 0.5) + 1 - pbinom(543, 1000, 0.5)



### Example: Do beautiful parents have more daughters?

# General population: 48.5% of births are girls
# People's Most Beautiful People: 157 girls out of 329 children
# One tailed P-value
1 - pbinom(156, 329, 0.485)
# (could also use CLT)

# Confidence interval
x_bar = 157/329
sd_error = sqrt(x_bar * (1 - x_bar) / 329)
# Lower bound:
x_bar - qnorm(.975) * sd_error
# Upper bound:
x_bar + qnorm(.975) * sd_error



### Example: Jeb's feeling thermometer

ANES = read.csv("anes_pilot_2016.csv")
nrow(ANES)
summary(ANES$ftjeb)
Jeb = ANES$ftjeb[ANES$ftjeb <= 100]
length(Jeb)
summary(Jeb)
sd(Jeb)

# Test H0 : mu = 50 vs. H1 : mu not 50
x_bar = mean(Jeb)
s = sd(Jeb)
n = length(Jeb)
t.Jeb = (mean(Jeb) - 50) / (sd(Jeb) / sqrt(n))
P.value = 2 * (1 - pnorm(abs(t.Jeb)))

# 95% confidence interval for Jeb's pop. mean
# Lower bound
x_bar - qnorm(0.975) * s / sqrt(n)
# Upper bound
x_bar + qnorm(0.975) * s / sqrt(n)

# A bit better: t-methods (see ch. 10)
# P-value
2 * (1 - pt(abs(t.Jeb), df = n-1))
# Interval
x_bar - qt(0.975, df = n-1) * s / sqrt(n)
x_bar + qt(0.975, df = n-1) * s / sqrt(n)






