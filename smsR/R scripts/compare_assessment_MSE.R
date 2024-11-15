# Open updated summary file
library(tidyverse)

df <- read.csv('data/summary.csv') %>% filter(years < 2022)

# Read MSE

load('data/mse8.Rdata')

# summary from mse
sum.mse <- mse8[[1]] %>% filter(years < 2022, year_assess == 'run-1') # Just do one run

plot(df$years, df$SSB)
lines(sum.mse$years, sum.mse$SSB)


df$model <- 'assessment'


df.plot <- bind_rows(df %>% select(years, R, SSB, Catch, Fbar,model),
                     sum.mse %>% select(years, R, SSB, Catch, Fbar, model)) %>% pivot_longer(c(R, SSB, Catch, Fbar))

p1 <- ggplot(df.plot, aes(x = years, y = value, color = model))+geom_line()+facet_wrap(~name, scales = 'free_y')+
  theme_bw()

ggsave(p1, filename = 'compare_MSE_EM_OM.png', width = 16, height = 12)
