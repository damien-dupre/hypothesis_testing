library(plotly)
library(tidyverse)
data(gapminder, package = "gapminder")
base <- gapminder %>%
  plot_ly(x = ~gdpPercap, y = ~lifeExp, size = ~pop, 
          text = ~country, hoverinfo = "text") %>%
  layout(xaxis = list(type = "log"))

base %>%
  add_markers(color = ~continent, frame = ~year, ids = ~country) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(color="red"))
  )

df_data <- 
  tibble(
    predictor = rtruncnorm(100, mean = 5, sd = 2.5, a = 0, b = 10),
    outcome = 2 + 0.9 * predictor + rnorm(100, mean = 0, sd = 1)
  )

df_lm <- lm(outcome ~ predictor, df_data)

df_data <- df_data %>% 
  mutate(b1 = df_lm$coefficients[["predictor"]])

df_data_pos <- df_data %>% 
  mutate(
    outcome = predictor,
    b1 = 1
  )

df_data_neg <- df_data %>% 
  mutate(
    outcome = (max(predictor)+min(predictor))-predictor,
    b1 = -1
  )

df_data_null <- df_data %>% 
  mutate(
    outcome = mean(predictor),
    b1 = 0
  )

df_data_null <- df_data %>% 
  mutate(
    outcome = mean(outcome),
    b1 = 0
  )

df_data_null <- df_data %>% 
  mutate(
    outcome = ((outcome - mean(outcome))/1.5) + mean(outcome),
    b1 = 0
  )

df_data_inverse <- df_data %>% 
  mutate(
    outcome = (max(outcome)+min(outcome))-outcome,
    b1 = -df_lm$coefficients[["predictor"]]
  )

df_data %>% 
  bind_rows(df_data_pos, df_data_neg, df_data_null, df_data_inverse) %>%
  plot_ly(x = ~predictor, y = ~outcome) %>%
  add_markers(color = ~b1, frame = ~b1, ids = ~predictor) %>%
  animation_opts(frame = 1000, easing = "linear") %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Cor ", font = list(color="red"))
  )
