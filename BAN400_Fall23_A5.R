library(tidyverse)
library(tictoc)
library(RcppArmadillo)
library(furrr)


# base R version ----

# it is important to use base R to reshape the data for the fastLmPure
# input parameters. This is otherwise overhead that we do not have in the
# function from Assignment 3, so it would be difficult to compare

is_variable_significant_cpp <- function(covariates, response, variable_to_assess, data = bacedata) {
  
  # create model as string
  model_call <- paste0(
    response, " ~ ",
    paste0(covariates, collapse = " + "), " + ",
    variable_to_assess
  )
  
  # all explantory variables, including variable of interest
  # must be filtered to != "" because one model contains only var of interes
  explanatory_var <- c(covariates, variable_to_assess)
  explanatory_var <- explanatory_var[explanatory_var != ""]
  
  # all variables, including response variable
  all_var <- c(response, explanatory_var)
  
  # data must be filtered: fastLmPure cannot handle NAs
  data_filtered <- na.omit(data[all_var])
  
  # define input parameters for fastLmPure
  # matrix of all explanatory values and vector of response values
  X <- as.matrix(cbind(1, data_filtered[explanatory_var]))
  y <- as.vector(data_filtered[[response]])
  
  # Fit the model using fastLmPure and input parameters defined above
  model_est <- RcppArmadillo::fastLmPure(y = y, X = X)
  
  # Calculate index for the coefficient
  # +2, because one intercept, and the last one is the var of interest
  index <- (ifelse(any(covariates == ""), 0, length(covariates)) + 2)
  
  # Extract coefficient and standard error needed to compute t statistic
  coefficient_estimate <- model_est$coefficients[index, ]
  coefficient_stderr <- model_est$stderr[index, ]
  
  # calculate degrees of freedom (n - k - 1) with k exlanatories and n observations
  degrees_of_freedom <- length(y) - length(explanatory_var) - 1
  
  # Calculate p-value of two-sided t-test
  coefficient_pvalue <- 2 * pt(abs(coefficient_estimate / coefficient_stderr),
                               degrees_of_freedom, lower.tail = FALSE)
  
  # return data frame of results
  tibble(
    model_call = model_call,
    assessed_variable = variable_to_assess,
    coefficient_estimate = coefficient_estimate,
    p_value = coefficient_pvalue,
    stderr = coefficient_stderr
  )
}

# start timer
tic()

# add a column with estimated models using function from A3 to explanatory_combs df
explanatory_combs$model_A5 <- map(explanatory_combs$explanatory_comb,
                                  is_variable_significant_cpp,
                                  response = "GR6096",
                                  variable_to_assess = "SOCIALIST",
                                  data = bacedata
)

# report elapsed time
toc()


# create data frame with coefficients and pvalues from model A4 and A5
compare_models <- explanatory_combs %>% 
  unnest(model_A4) %>% 
  rename(coef_model_A4 = coefficient_estimate,
         pval_model_A4 = p_value) %>% 
  select(explanatory_comb, coef_model_A4, pval_model_A4, model_A5) %>% 
  unnest(model_A5) %>% 
  rename(coef_model_A5 = coefficient_estimate,
         pval_model_A5 = p_value) %>% 
  select(explanatory_comb, coef_model_A4, pval_model_A4, coef_model_A5, pval_model_A5) %>% 
  mutate(coef_diff = coef_model_A5 - coef_model_A4,
         pval_diff = pval_model_A5 - pval_model_A4)

# deviations in coefficients between model A5 and A4
compare_models %>% 
  select(coef_diff) %>% 
  arrange(desc(coef_diff)) %>% 
  head(5)

# deviations in pvalues between model A5 and A4
compare_models %>% 
  select(pval_diff) %>% 
  arrange(desc(pval_diff)) %>% 
  head(5)



# dplyr version -----

# using dplyr instead of base R to reshape the data to fit the
# input-parameters needed for fastLmPure funtion
is_variable_significant_cpp_dplyr <- function(covariates, response, variable_to_assess, data = bacedata) {
  # create model as string
  model_call <- paste0(
    response, " ~ ",
    paste0(covariates, collapse = " + "), " + ",
    variable_to_assess
  )
  
  # for fastLmPure to estimate using c++, we need to reshape the input
  # fastLmPure cannot handle NAs
  data_filtered <- data %>%
    select(all_of(response), which(names(data) %in% covariates), all_of(variable_to_assess)) %>%
    filter(complete.cases(.))
  
  # fastLmPure needs matrix input for explanatories
  X <- cbind(1, as.matrix(data_filtered %>% select(-all_of(response))))
  
  # y must be single atomic vector containing all values for response variable
  y <- data_filtered %>% pull(response)
  
  # now, using X and y, we can estimate model with fastLmPure
  model_est <- RcppArmadillo::fastLmPure(y = y, X = X)
  
  # index to extract coefficient etc.
  index <- ifelse(any(covariates == ""), 0, length(covariates)) + 2
  
  # extract the coefficient estimate from the model
  coefficient_estimate <- model_est$coefficients[index, ]
  
  # calculate the p-value from the variable to assess
  coefficient_pvalue <- 2 * pt(
    abs(model_est$coefficients[index, ] / model_est$stderr[index, ]),
    model_est$df.residual,
    lower.tail = FALSE
  )
  
  # return data frame of results
  tibble(
    model_call = model_call,
    assessed_variable = variable_to_assess,
    coefficient_estimate = coefficient_estimate,
    p_value = coefficient_pvalue
  )
  
}


# start timer
tic()

# add a column with estimated models using function from A3 to explanatory_combs df
explanatory_combs$model_A5 <- map(explanatory_combs$explanatory_comb,
                                  is_variable_significant_cpp_dplyr,
                                  response = "GR6096",
                                  variable_to_assess = "SOCIALIST",
                                  data = bacedata
)

# report elapsed time
toc()
