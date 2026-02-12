library(tidyverse)

set.seed(1980)

N <- 100000
rho <- 1.2

# total time
T_total <- 41*60 + 57.64

# Average speed (Mieto)
v_segments <- c(
  5000 / (13*60 + 25.00),
  5000 / ((25*60 + 51.97) - (13*60 + 25.00)),
  5000 / ((41*60 + 57.64) - (25*60 + 51.97))
)

# Track profile (proxy)
profile <- c(up = 0.4, flat = 0.3, down = 0.3)
speed_mult <- c(up = 0.7, flat = 1.0, down = 1.3)

# Priors
A_body <- 0.5
A_beard <- runif(N, 0.005, 0.015)
Cd_body <- runif(N, 0.7, 0.9)
P_total <- rnorm(N, 400, 30)

# Alpha (speed of beard)
alpha <- runif(N, 0.25, 0.4)

# mean v^3
v3_mean <- numeric(N)

for (k in 1:3) {
  v_obs <- v_segments[k]
  scale <- v_obs / sum(profile * speed_mult)
  
  for (seg in names(profile)) {
    v <- rnorm(N, speed_mult[seg] * scale, 0.2)
    v[v < 1] <- 1
    v3_mean <- v3_mean + profile[seg] * v^3
  }
}

v3_mean <- v3_mean / 3

# Kehon ilmanvastusteho
P_drag_body <- 0.5 * rho * Cd_body * A_body * v3_mean

# Power of drag beard
P_drag_beard <- P_drag_body *
  (A_beard / A_body) *
  (alpha^3)

# Delta time
delta_time <- T_total * (P_drag_beard / P_total)

# Todennäköisyys, että parran vaikutus realisoituu
p_active <- runif(N, 0.05, 0.2)  # esim. 5–20 % ajasta

delta_time_real <- delta_time * p_active

summary(delta_time_real)

ggplot(df, aes(x = delta_time_real)) +
  geom_density(fill = "lightgray") +
  geom_vline(xintercept = 0.01, linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = 0.011, y = 2.5, label = "0.01 s (todellinen ero)") +
  labs(
    title = "Posteriorijakauma parran aiheuttamalle ajanhäviölle",
    subtitle = sprintf("P(Δt ≥ 0.01 s)"),
    x = "Ajanhäviö (sekunteina)",
    y = "Tiheys"
  ) +
  theme_minimal()