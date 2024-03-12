library(tidyverse)
library(assertthat)



is_variable_significant <- function(covariates, response, variable_to_assess, data = bacedata) {
  
  # create model as string
  model_call <- paste0(
    response, " ~ ",
    paste0(covariates, collapse = " + "), " + ",
    variable_to_assess
  )
  
  # estimate model
  model_est <- lm(model_call, data = data)
  
  # extract coefficients and p value of response from model
  coefficient_estimate <- summary(model_est)$coefficients[variable_to_assess, "Estimate"]
  coefficient_pvalue <- summary(model_est)$coefficients[variable_to_assess, "Pr(>|t|)"]

  # return data frame of results
  tibble(
    model_call = model_call,
    assessed_variable = variable_to_assess,
    coefficient_estimate = coefficient_estimate,
    p_value = coefficient_pvalue
  )
  
}
   