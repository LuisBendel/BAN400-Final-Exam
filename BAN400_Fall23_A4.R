library(tidyverse)
library(tictoc)

# start timer
tic()

# add a column with estimated models using function from A3 to explanatory_combs df
explanatory_combs$model_A4 <- map(explanatory_combs$explanatory_comb,
  is_variable_significant,
  response = "GR6096",
  variable_to_assess = "SOCIALIST",
  data = bacedata
)

# report elapsed time
toc()

# filter for significant positive coefficients
explanatory_combs %>%
  unnest(model_A4) %>%
  filter(coefficient_estimate > 0, p_value < 0.05)

# plot estimates and pvalues
coef_pval_plot <- explanatory_combs %>%
  unnest(model_A4) %>%
  ggplot(aes(x = coefficient_estimate, y = p_value)) +
  geom_point() +
  geom_vline(xintercept = 0, color = "red", linewidth = 1.5) +
  geom_hline(yintercept = 0.05, color = "blue", linewidth = 1.5) +
  labs(
    title = "P-values of coefficient estimates",
    x = "Coefficient estimate",
    y = "p-value"
  ) +
  theme_minimal() +
  annotate("text", x = -0.002, y = 0.6, label = "Coefficient = 0", vjust = 1.1, hjust = 3, color = "red", size = 6) +
  annotate("text", x = -0.002, y = 0.08, label = "p = 0.05", hjust = 1.5, vjust = -0.5, color = "blue", size = 6)

# make interactive
ggplotly(coef_pval_plot)
