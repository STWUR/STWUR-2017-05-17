library(dplyr)
library(binom)
library(ggplot2)

dat <- binom.confint(1L:100, 100)

prob_cov <- do.call(rbind, lapply(1L:1000, function(dummy)
  do.call(rbind, lapply(1L:100, function(ith_n_success)
  data.frame(x = ith_n_success, real_mean = rbinom(1, 100, prob = ith_n_success/100)/100)
)) %>% 
  inner_join(dat))) %>% 
  mutate(inside = real_mean > lower & real_mean < upper) %>% 
  group_by(x, method) %>% 
  summarise(cov = mean(inside))

ggplot(prob_cov, aes(x = x, y = cov, color = method)) +
  geom_line()
