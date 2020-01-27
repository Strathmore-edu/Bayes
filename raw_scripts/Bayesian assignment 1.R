# Load required packages --------------------------------------------------

library(tidyverse)

# simlate date given the funtcion inputs ----------------------------------

set.seed(20200127) # for reproducibility

Age <- rnorm(n = 150, mean = 25, sd = sqrt(5)) %>% 
  tibble::enframe(name = NULL) %>% rename("age" = "value") # simulate age


sex <- rbinom(n = 150, size = 1, prob = 0.52) %>% 
  tibble::enframe(name = NULL) %>% rename("sex" = "value") # simulate sex variable


error_term <- rnorm(n = 150, mean = 0, sd = 1.732)%>% 
  tibble::enframe(name = NULL) %>% rename("error" = "value") # error term


# Formulate Y_i -----------------------------------------------------------

dframe <- bind_cols(sex, Age, error_term) %>% 
  mutate(
    alpha = rep(-2, 150) # the y intercept
  )

# to formulate y_i = -2 + 0.6*sex + 0.3*Age + error_term

dframe <- dframe %>% 
  mutate(
    y_hat = (-2 + 0.6*sex + 0.3*age) # expected outcome based on alpha, age, sex 
  )

# (i) Fitting linear model using "lm" -------------------------------------

lm_model <- lm(y_hat ~ age + sex, data = dframe)

summary(lm_model)

broom::tidy(lm_model)


# (ii) Fitting linear model using quasi newton ----------------------------

# function to get residual sums of squares for linear model

RSS_min <- function(par, df){
  with(df,
       sum(sum((par[1] + par[2] * sex + par[3] * age - y_hat)^2)) # minimization of sums of squares
       )
}

# pass the function to be minimized to as an optim arguments

output <- optim(par = c(0, 0, 0), # initialize the intercept, B1 and B2
      fn = RSS_min, # function to be minimized
      df = dframe) # dataframe to use

sprintf(
    "Intercept= %0.4f; B1 = %0.4f and B2 = %0.4f",
    output$par[1], 
    output$par[2], 
    output$par[3]
  )


# (iii) Fitting linear model using Newton Raphson; first principle --------


