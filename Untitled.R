rtruncnorm <- function(N, mean = 0, sd = 1, a = -Inf, b = Inf) {
  U <- runif(N, pnorm(a, mean, sd), pnorm(b, mean, sd))
  qnorm(U, mean, sd) 
}


df_data <- 
  tibble(
    predictor = rtruncnorm(100, mean = 5, sd = 2.5, a = 0, b = 10),
    outcome = 2 + 0.9 * predictor + rnorm(100, mean = 0, sd = 1),
    b0 = rnorm(100, 0, 1),
    b1 = rnorm(100, 0.02, 0.1)
  )

df_lm <- lm(outcome ~ predictor, df_data)
  df_lm$coefficients[["(Intercept)"]]
  df_lm$coefficients[["predictor"]]

ggplot(df_data, aes(predictor, outcome)) +
  geom_point(color = "black", size = 2) +
  geom_smooth(method = "lm", fullrange = TRUE, se = FALSE) +
  geom_hline(yintercept = df_lm$coefficients[["(Intercept)"]], color = 'black', size = 0.5, linetype = 'dotted') +
  annotate("text", x = 8, y = df_lm$coefficients[["(Intercept)"]]+0.5, label = "Intercept \u03b2\u2080") +
  annotate('segment', x = 8, xend = 9, y = df_lm$coefficients[["(Intercept)"]] + 8*df_lm$coefficients[["predictor"]], yend = df_lm$coefficients[["(Intercept)"]] + 8*df_lm$coefficients[["predictor"]], color = 'red') +
  annotate('segment', x = 6, xend = 6, y = 5, yend = 6, color = 'red') +
  annotate("text", x = 7.5, y = 5.5, label = "Slope \u03b2\u2081") +
  scale_x_continuous(breaks = c(0, seq(0:10)), limits = c(0, 10)) +
  scale_y_continuous(limits = c(0, 10)) +
  # scale_y_continuous(breaks = seq(0:10)) +
  theme_bw()
  
  
  