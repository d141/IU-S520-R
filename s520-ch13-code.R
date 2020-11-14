# 20,000 die rolls
observed = c(3407, 3631, 3176, 2916, 3448, 3422)
# H0: Die is fair
expected = rep(20000/6, 6)
# LR chi-squared test
G2 = 2 * sum(observed * log(observed/expected))
1 - pchisq(G2, df=5)
# Pearson's chi-squared
X2 = sum((observed - expected)^2 / expected)
1 - pchisq(X2, df=5)


# Observe the genders of the first three children
# in families with 3 or more kids (in Denmark)
# 0 girls: 23236
# 1 girl: 58529
# 2 girls: 53908
# 3 girls: 18770
# Does the number of girls follow a binomial?

# Treat as random process
# First: Estimate the proportion of girls
families = 18770 + 53908 + 58529 + 23236
girls = 3*18770 + 2*53908 + 58529
p.girl = girls / (3*families)

# Now do chi-squared test
observed = c(23236, 58529, 53908, 18770)
expected = c(families*dbinom(0, 3, p.girl),
             families*dbinom(1, 3, p.girl),
             families*dbinom(2, 3, p.girl),
             families*dbinom(3, 3, p.girl))
# Shorter code:
expected = families * dbinom(0:3, 3, p.girl)
# Pearson's X^2
X2 = sum((observed - expected)^2/expected)
1 - pchisq(X2, df=2)
observed - expected
# Likelihood ratio
G2 = 2 * sum(observed * log(observed/expected))
1 - pchisq(G2, df=2)


### Premier League goals, 2000-01
# Observed number of games with 0 to 8 goals
observed = c(28, 67, 100, 88, 52, 25, 13, 6, 1)
plot(0:8, observed, type="h")
# Maybe Poisson?
games = sum(observed)
goals = sum((0:8)*observed)
ave = goals/games
expected = games * dpois(0:20, ave)
expected
round(expected, 1)
# Try again
observed = c(28, 67, 100, 88, 52, 25, 13, 7)
expected = rep(NA, 8)
expected[1:7] = games * dpois(0:6, ave)
expected[8] = games * (1 - ppois(6, ave))
sum(expected)
# Do test
X2 = sum((observed - expected)^2/expected)
1 - pchisq(X2, df=6)


### Handedness by sex
# Right-handed: 934 men, 1070 women
# Left-handed: 113 men, 92 women
# Ambidextrous: 20 men, 8 women
observed = c(934, 1070, 113, 92, 20, 8)
N = sum(observed)
RH = (934 + 1070) / N
LH = (113 + 92) / N
ambi = (20 + 8) / N
men = (934 + 113 + 20) / N
women = c(1070 + 92 + 8) / N
expected = c(RH*men*N, RH*women*N, LH*men*N,
             LH*women*N, ambi*men*N, ambi*women*N)
X2 = sum((observed - expected)^2/expected)
1 - pchisq(X2, df=2)



# Death penalty
observed = c(30, 184, 6, 104)
expected = c(214*36/324, 214*288/324,
             110*36/324, 110*288/324)
X2 = sum((observed - expected)^2/expected)
1 - pchisq(X2, df=1)
