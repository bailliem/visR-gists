library(tibble)
library(dplyr)
library(ggplot2)

set.seed(1972)

N <- 24
id <- 1:N

mu <- 2.3
sigma <- 0.07
delta <- 0.2

y <- rnorm(N, mu, sqrt(sigma))
phi <- rnorm(N, 0, sqrt(0.001))


df <- tibble(
  id = id,
  y_pbo = y - (phi/2),
  y_act = y + delta + (phi/2)
)

df %>%
  ggplot(aes(y = y_pbo, x = id)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_point(aes(y = y_act, x = id), colour = "red", alpha = 0.5) +
  geom_linerange(aes(ymin = y_pbo, ymax = y_act), alpha = 0.2) +
  coord_flip() + 
  theme_minimal()
