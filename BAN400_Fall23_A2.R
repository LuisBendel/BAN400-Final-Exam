library(tidyverse)
library(combinat)
library(plotly)

# function to estimate model, takes dataframe and vector of explanatory variables
reg_fraud <- function(df, v) {
  df %>% 
    select(GR6096, SOCIALIST, which(names(df) %in% v)) %>% 
    lm(GR6096 ~ ., data = .)
}

# create vector with all 1024 possible explanatory combinations
# first, create vector with explanatories of interest
explanatory_var <- c("LANDAREA",
                    "PRIEXP70",
                    "EAST",
                    "AVELF",
                    "ORTH00",
                    "POP6560",
                    "REVCOUP",
                    "OTHFRAC",
                    "MINING",
                    "PROT00")

# generate all possible subsets of this vector
# do not forget the possibility to use only SOCIALIST, without controls
explanatory_combs <- lapply(1:length(explanatory_var),
                                 combinat::combn,
                                 x = explanatory_var,
                                 simplify = F) %>% 
  unlist(., recursive = F) %>% 
  as.matrix() %>% 
  as_tibble(.name_repair = "unique") %>% 
  rename(explanatory_comb = ...1) %>% 
  rbind("")

# now estimate the model for every combination generated above
# I separate this from the pipe above for documentation purposes
explanatory_combs$model_A2 <- map(explanatory_combs$explanatory_comb,
                               reg_fraud,
                               df = bacedata)

# function to extract the coefficient on SOCIALIST in each model
get_coef_SOCIALIST <- function(model) {
  model[1]$coefficients[names(model[1]$coefficients) == "SOCIALIST"]
}

# plot coefficients on SOCIALIST for all models
coef_hist <- explanatory_combs %>% 
  mutate(coef_SOCIALIST = map_dbl(model_A2, get_coef_SOCIALIST)) %>% 
  ggplot(aes(x = coef_SOCIALIST)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of coefficient on SOCIALIST",
       x = "Coefficient on SOCIALIST") +
  theme_minimal()

# make interactive plot
ggplotly(coef_hist)
