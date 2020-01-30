# Load required packages --------------------------------------------------

library(tidyverse)

# simlate data given the following inputs ----------------------------------

set.seed(20200127) # for reproducibility

age <- rnorm(n = 150, mean = 25, sd = sqrt(5)) %>% 
  tibble::enframe(name = NULL) %>% rename("age" = "value") # simulate age


sex <- rbinom(n = 150, size = 1, prob = 0.52) %>% 
  tibble::enframe(name = NULL) %>% rename("sex" = "value") # simulate sex variable


error_term <- rnorm(n = 150, mean = 0, sd = 1.732)%>% 
  tibble::enframe(name = NULL) %>% rename("error_term" = "value") # error term


# Construct Y_i(bmi) -----------------------------------------------------------

data_frame <- bind_cols(sex, age, error_term) %>% mutate(alpha = rep(-2, 150)) # alpha is the bmi intercept

# To construct bmi = -2 + 0.6*sex + 0.3*Age + error_term

data_frame <- data_frame %>% 
  mutate(
    bmi = (-2 + 0.6*sex + 0.3*age ) # expected outcome(bmi) based on alpha, age, sex and error term 
  )

# (i) Fitting linear model using "lm" -------------------------------------

lm_model <- lm(bmi ~ age + sex, data = data_frame) #fit using "lm" method

summary.lm(lm_model)

broom::tidy(lm_model)


# (ii) Fitting linear model using quasi newton ----------------------------

# function to get residual sums of squares for linear model

RSS_min <- function(par, df){
  with(df,
       sum(sum((par[1] + par[2] * sex + par[3] * age - bmi)^2)) # minimization of sums of squares
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

linear_model_using_newton_raphson = function(  y_hat, toler = .0001) {
  y_hat = (-2 + 0.6*sex + 0.3*age) #y_hat is the linear regression model with 3 parameters
  startvalue = median(y_hat)
  #n = length(bmi);
  thetahatcurr = startvalue
  
  
  
  # Compute first deriviative of log likelihood
  #
  #firstderivll = 2 * sum((bmi - thetahatcurr) / (1 + (bmi - thetahatcurr) ^ 2))
  
  # Continue Newton’s method until the first derivative
  # of the likelihood is within toler of 0.0001
  
  while (abs(thetahatnew-thetahatcurr) < toler)
  {
    # Compute second derivative of log likelihood
    secondderivll = 2 * sum(((y_hat - thetahatcurr) ^ 2 - 1) / (1 + (y_hat - thetahatcurr) ^2) ^ 2)
    
    # Compute first derivative of log likelihood
    
    firstderivll = 2 * sum((y_hat - thetahatcurr) / (1 + (y_hat - thetahatcurr) ^ 2))
    
    # Newton Raphson method update of estimate of theta
    
    thetahatnew = thetahatcurr - firstderivll / secondderivll
    
    thetahatcurr = thetahatnew
    
    
    
  }
  list(thetahat = thetahatcurr)
  
}
list(thetahat = thetahatcurr)

#(linear_model_using_newton_raphson(y_hat))


