fair.means = replicate(100000,
  mean(sample(1:6, 100, replace=TRUE)))
plot(density(fair.means))
qqnorm(fair.means)

biased.means = replicate(100000,
  mean(sample(1:6, 100, replace=TRUE,
  prob=c(.1,.1,.2,.2,.2,.2))))
plot(density(biased.means))
qqnorm(biased.means)

