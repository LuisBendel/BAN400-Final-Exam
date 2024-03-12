library(tidyverse)
library(stargazer)

# vector containing all optional explanatory variables that can be combinated
expl_all <- names(bacedata)[!names(bacedata) %in% c("GR6096", "SOCIALIST")]

# first, try the random approach. Create function to conveniently call later
est_random_models <- function(variables = expl_all,
                              coef_th = 0,
                              pval_th = 0.05,
                              n_var = c(0:15),
                              n_mod = 100000, timeout = 30,
                              setseed = FALSE) {
  # set starting values for coef and pval
  coef <- 0
  pval <- 1
  i <- 0

  # create tibble with all models that are assessed
  results <- tibble(
    expl_var = list(),
    results = data.frame()
  )

  # set start time
  start_time <- Sys.time()

  # set seed for reproducability, but not for the pval differences below
  if (setseed == TRUE) {
    set.seed(123)
  }

  # randomly estimate models for a certain amount of time
  while ((coef <= coef_th | pval >= pval_th) && difftime(Sys.time(), start_time, units = "secs") < timeout && i < n_mod) {
    # generate a random number between 0 and n_var (parameter for function)
    n_expl <- sample(n_var, size = 1)

    # now get a random sample with n_expl explanatory variables from the expl_var vector
    expl_incl <- sample(variables, size = n_expl)

    # estimate the model with the function from A5
    model_tmp <- is_variable_significant_cpp(
      response = "GR6096",
      variable_to_assess = "SOCIALIST",
      covariates = expl_incl
    )

    # reset the coefficient and pvalue and i (n_mod)
    coef <- model_tmp$coefficient_estimate
    pval <- model_tmp$p_value
    i <- i + 1

    # append row to tibble where we keep track of all tries
    results <- results %>%
      add_row(
        expl_var = list(expl_incl),
        results = model_tmp
      )
  }

  if (coef > 0 && pval < 0.05) {
    print(paste0("Estimated ", as.character(nrow(results)), " random models and found one significant positive estimate."))
  }

  results
}

# get the fraud model results from the fastLmPure estimation
fraud_results_fastLmPure <- est_random_models(n_var = 15, timeout = 100, setseed = TRUE) %>%
  unnest(results) %>%
  filter(coefficient_estimate > 0, p_value < 0.05)

# estimate this model with the lm function
fraud_model_lm <- fraud_results_fastLmPure %>%
  filter(coefficient_estimate > 0, p_value < 0.05) %>%
  pull(model_call) %>%
  lm(., data = bacedata)

# display regression results in table with stargazer package
stargazer(fraud_model_lm,
  type = "text",
  title = "Regression Results of Fraud Model",
  label = "tab:regression_results",
  header = FALSE,
  model.names = FALSE,
  dep.var.labels.include = FALSE
)

# check if coefficient and p-value are the same in lm vs fastLmPure
print(paste0("coefficient Lm estimation: ",
             summary(fraud_model_lm)$coefficients["SOCIALIST", "Estimate"]))
print(paste0("coefficient fastLmPure estimation: ",
             fraud_results_fastLmPure$coefficient_estimate))
print(paste0("p-value Lm estimation: ",
             summary(fraud_model_lm)$coefficients["SOCIALIST", "Pr(>|t|)"]))
print(paste0("p-value fastLmPure estimation: ",
             fraud_results_fastLmPure$p_value))





# lets see about the threshold value for when standard errors become different

tic()

# create data frame where we store the simulations
random_model_sim <- tibble(
  n_var = as.numeric(),
  expl_var = list(),
  model_call = as.character(),
  coef_fastLmPure = as.numeric(),
  pval_fastLmPure = as.numeric(),
  stderr_fastLmPure = as.numeric()
)

# estimate 100 models per n_var, starting from n_var = 10 until n_var = 66
# we can set the coef threshold and the pval threshold to unrealistic values,
# so the function est_random_models does not stop when a positive significant is found
for (i in 10:66) {
  tmp_res <- est_random_models(
    coef_th = 100,
    pval_th = -1,
    n_var = c(i:i),
    n_mod = 100,
    timeout = 100
  ) %>%
    unnest(results)

  random_model_sim <- random_model_sim %>%
    add_row(
      n_var = i,
      expl_var = tmp_res$expl_var,
      model_call = tmp_res$model_call,
      coef_fastLmPure = tmp_res$coefficient_estimate,
      pval_fastLmPure = tmp_res$p_value,
      stderr_fastLmPure = tmp_res$stderr
    )
}

toc()


tic()
# estimate the same models with the lm function
random_model_sim$model_lm <-
  map(random_model_sim$expl_var,
    is_variable_significant,
    response = "GR6096",
    variable_to_assess = "SOCIALIST",
    data = bacedata
  )

toc()

# create plot to show how difference increases with n_var
random_model_sim_plot <- random_model_sim %>%
  select(n_var, model_lm, pval_fastLmPure) %>%
  unnest(model_lm) %>%
  rename(pval_lm = p_value) %>%
  select(n_var, pval_fastLmPure, pval_lm) %>%
  mutate(pval_diff = pval_fastLmPure - pval_lm) %>%
  ggplot(aes(x = n_var, y = pval_diff)) +
  geom_point() +
  geom_smooth() +
  labs(
    x = "number of explanatory variables",
    y = "difference in p-values"
  ) +
  theme_minimal()

# make it look nice and interactive
ggplotly(random_model_sim_plot) %>%
  layout(
    title = list(text = paste("Difference in p-values from fastLmPure vs. lm<br><sub>100 random models per number of explanatory variables</sub>"))
  )








library(foreach)
library(doParallel)
library(furrr)
library(future)

tic()

# Register parallel backend to use multiple cores
numCores <- detectCores()
registerDoParallel(cores = numCores)

# iterate ofer n_var in parallel, estimate models for each n_var
random_model_sim <-
  foreach(
    i = 10:66,
    .combine = rbind,
    .packages = c("tidyverse"),
    .init = tibble(
        n_var = numeric(),
        expl_var = list(),
        model_call = character(),
        coef_fastLmPure = numeric(),
        pval_fastLmPure = numeric(),
        stderr_fastLmPure = numeric()
      )
    ) %dopar% {
  tmp_res <- est_random_models(
    coef_th = 100,
    pval_th = -1,
    n_var = c(i:i),
    n_mod = 100,
    timeout = 100
  ) %>%
    unnest(results)
  
  tibble(
    n_var = i,
    expl_var = tmp_res$expl_var,
    model_call = tmp_res$model_call,
    coef_fastLmPure = tmp_res$coefficient_estimate,
    pval_fastLmPure = tmp_res$p_value,
    stderr_fastLmPure = tmp_res$stderr
  )
}

# Stop the parallel backend
stopImplicitCluster()

toc()



tic()

plan(multisession, workers = detectCores())

# estimate the same models with the lm function
random_model_sim$model_lm <-
  future_map(random_model_sim$expl_var,
      is_variable_significant,
      response = "GR6096",
      variable_to_assess = "SOCIALIST",
      data = bacedata
  )

plan(sequential)

toc()

