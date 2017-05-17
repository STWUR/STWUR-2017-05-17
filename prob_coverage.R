library(dplyr)
library(binom)
library(ggplot2)

dat <- binom.confint(0.02*25L:100, 25L:100)

prob_cov <- rbind(binom.coverage(0.02, 25L:100, conf.level = 0.95, method = "asymptotic"),
                  binom.coverage(0.02, 25L:100, conf.level = 0.95, method = "wilson"))

ggplot(prob_cov, aes(x = n, y = coverage, color = method)) +
  geom_line() +
  geom_hline(yintercept = 0.95) +
  theme_bw() +
  scale_y_continuous("Coverage probability")
