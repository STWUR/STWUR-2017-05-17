library(dplyr)
library(binom)
library(ggplot2)

dat <- binom.confint(1L:100, 700)

ggplot(dat, aes(x = x, y = cov, color = method)) +
  geom_line()

tmp <- do.call(rbind, lapply(1L:100, function(dummy)
  do.call(rbind, lapply(1L:100, function(ith_n_success)
  data.frame(x = ith_n_success, real_mean = rbinom(1, 1000, prob = ith_n_success/1000)/1000)
)) %>% 
  inner_join(dat))) %>% 
  mutate(cov = real_mean > lower & real_mean < upper) %>% 
  group_by(x, method) %>% 
  summarise(cov = mean(cov)) %>% 
  filter(cov != 1)

