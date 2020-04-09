
# Final Project Winter Quarter 2020 -------------------------------------------------------------------
# Holland Blumer
# 3/17/2020 

# Load Packages ---------------------------------------------------------------

library(MASS)
library(tidyverse)
library(janitor)
library(skimr)
library(modelr)
library(tidyr)
library(dplyr)
library(base)
library(ggplot2)
library(readr)
library(GGally)
library(grid)
library(gridExtra)
library(corrplot)
library(corrr)
library(class)
library(broom)
library(naniar)
library(rsample)
library(leaps) # find the best subset selection
library(glmnet) # ridge & lasso
library(glmnetUtils) # improves working with glmnet
library(pls) # pcr and pls

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("setdiff", "dplyr")
conflicted::conflict_prefer("corrplot", "corrplot")

set.seed(26535) # Set the seed to make it reproducible


# Load Data ------------------------------------------------------------

# Load Coronavirus data from past 90 days
# https://trends.google.com/trends/explore?date=today%203-m&geo=US&q=coronavirus

coronavirus_90_days_dat <- read_csv("data/coronavirus_US_90_days.csv") %>% 
  clean_names() %>% 
  rename(date = category_all_categories) %>% 
  rename(coronavirus_count = x2) %>% 
  mutate_if(is_character, factor) %>% 
  clean_names()

# Bernie Sanders data from the past 90 days
# https://trends.google.com/trends/explore?date=today%203-m&geo=US&q=bernie%20sanders

sanders_90_days_dat <- read_csv("data/bernie_sanders_90_days.csv") %>% 
  clean_names() %>% 
  rename(date = category_all_categories) %>%
  rename(sanders_count = x2) %>% 
  mutate_if(is_character, factor) %>%
  clean_names()

# Joe Biden data from the past 90 days
# https://trends.google.com/trends/explore?date=today%201-m&geo=US&q=joe%20biden

biden_90_days_dat <- read_csv("data/joe_biden_90_days.csv") %>% 
  clean_names() %>% 
  rename(date = category_all_categories) %>%
  rename(biden_count = x2) %>% 
  mutate_if(is_character, factor) %>% 
  clean_names()

# Donald Trump data from the past 90 days
# https://trends.google.com/trends/explore?date=2019-12-15%202020-03-13&geo=US&q=donald%20trump

trump_90_days_dat <- read_csv("data/donald_trump_90_days.csv") %>% 
  clean_names() %>% 
  rename(date = category_all_categories) %>%
  rename(trump_count = x2) %>% 
  mutate_if(is_character, factor) %>% 
  clean_names()

# Load Yahoo Finance Data on American National Insurance Company (ANAT)
# https://finance.yahoo.com/quote/AAL/history?period1=1581206400&period2=1583452800&interval=1d&filter=history&frequency=1d

anat_90_days_dat <- read_csv("data/ANAT_yahoo_90_days.csv") %>% 
  clean_names() %>% 
  rename(close_anat = close) %>% 
  mutate_if(is_character, factor)

# Load Yahoo Finance Data on American Airlines (AAL)
# https://finance.yahoo.com/quote/AAL/history?period1=1581206400&period2=1583452800&interval=1d&filter=history&frequency=1d

aal_90_days_dat <- read_csv("data/AAL_yahoo_90_days.csv") %>% 
  clean_names() %>% 
  rename(close_aal = close) %>% 
  mutate_if(is_character, factor)


# Tidy Data ---------------------------------------------------------------

# Skim
skim(coronavirus_90_days_dat)

skim(sanders_90_days_dat)

skim(biden_90_days_dat)

skim(trump_90_days_dat)

skim(anat_90_days_dat)

skim(aal_90_days_dat)

# omit any NA values

coronavirus_90_days_dat %>% 
  na.omit()

sanders_90_days_dat %>% 
  na.omit()

trump_90_days_dat %>%
  na.omit()

biden_90_days_dat %>% 
  na.omit()

anat_90_days_dat %>%
  na.omit()

aal_90_days_dat  %>% 
  na.omit()

# coronavirus dataset remove title
coronavirus_90_days_dat_tidy <- coronavirus_90_days_dat[c(2:91), ]

# bernie sanders dataset remove title
sanders_90_days_dat_tidy <- sanders_90_days_dat[c(2:91), ]

# joe biden dataset remove title
biden_90_days_dat_tidy <- biden_90_days_dat[c(2:91), ]

# donald trump dataset remove title
trump_90_days_dat_tidy <- trump_90_days_dat[c(2:91), ]

# anat yahoo dataset.. I selected close price and date only 
anat_90_days_dat_tidy <- anat_90_days_dat %>%
  select(-high, -low, -adj_close, -volume, -open)

# aal yahoo dataset.. I selected close price and date only 
aal_90_days_dat_tidy <- aal_90_days_dat %>%
  select(-high, -low, -adj_close, -volume, -open)

# used left join to combine datasets into one dataset

anat_aal_join <- left_join(anat_90_days_dat_tidy, aal_90_days_dat_tidy , by=c("date"))

anat_aal_coronavirus_join <- left_join(anat_aal_join, coronavirus_90_days_dat_tidy , by=c("date"))

anat_aal_coronavirus_sanders_join <- left_join(anat_aal_coronavirus_join, sanders_90_days_dat_tidy , by=c("date"))

anat_aal_coronavirus_sanders_biden_join <- left_join(anat_aal_coronavirus_sanders_join, biden_90_days_dat_tidy , by=c("date"))

all_data_combined <- left_join(anat_aal_coronavirus_sanders_biden_join, trump_90_days_dat_tidy , by=c("date"))

# For the coronavirus dataset, we need to change the less than 1 value to 1 to run appropriate regression methods 

all_data_combined_tidy <- as.data.frame(sapply(all_data_combined, gsub, pattern = "<|>", replacement = ""))  

all_data_combined_tidy <- data.frame(lapply(all_data_combined_tidy, function(x) as.numeric(as.factor(x))))

# Use this dataset for when you need to leave out the dates
all_data_combined_no_date <- all_data_combined_tidy  %>%
  select(-date) 

all_data_combined_no_date_numeric <- data.frame(lapply(all_data_combined_no_date, function(x) as.numeric(as.factor(x))))

# need to remove the 1 in coronavirus data because it is affecting the correlation
all_data_combined_no_date_numeric_remove_one <- all_data_combined_no_date_numeric[c(23:60), ]

all_data_combined_no_date_numeric_remove_one

# EDA ---------------------------------------------------------------------

ggscatmat(all_data_combined_no_date_numeric_remove_one)

# the variables must be numeric for corrplot to run 
all_data_combined_no_date_numeric_remove_one %>%
  cor(use="pairwise.complete.obs") %>%
  corrplot()

all_data_combined_no_date_numeric_remove_one %>%
  skim_without_charts()

skim(all_data_combined_no_date_numeric_remove_one )

coronavirus_aal_plot <- all_data_combined_tidy %>%
  ggplot(aes(all_data_combined_tidy$date, group = 1)) +
  geom_line(aes(y=coronavirus_count, group = 1), color="red") +  
  geom_line(aes(y=close_aal, group = 1), color="green") + 
  xlab("Past 90 days") +
  ylab("Numeric Values") +
  ggtitle("Coronavirus v Stock Prices in 2020") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 4), legend.title = element_blank()) 
coronavirus_aal_plot

biden_aal_plot <- all_data_combined_tidy %>%
  ggplot(aes(all_data_combined_tidy$date, group = 1)) +
  geom_line(aes(y=biden_count, group = 1), color="red") +  
  geom_line(aes(y=close_aal, group = 1), color="green") + 
  xlab("Past 90 days") +
  ylab("Numeric Values") +
  ggtitle("Biden v Stock Prices in 2020") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 4), legend.title = element_blank()) 
biden_aal_plot

sanders_aal_plot <- all_data_combined_tidy %>%
  ggplot(aes(all_data_combined_tidy$date, group = 1)) +
  geom_line(aes(y=sanders_count, group = 1), color="red") +  
  geom_line(aes(y=close_aal, group = 1), color="green") + 
  xlab("Past 90 days") +
  ylab("Numeric Values") +
  ggtitle("Sanders v Stock Prices in 2020") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 4), legend.title = element_blank()) 
sanders_aal_plot

trump_aal_plot <- all_data_combined_tidy %>%
  ggplot(aes(all_data_combined_tidy$date, group = 1)) +
  geom_line(aes(y=trump_count, group = 1), color="red") +  
  geom_line(aes(y=close_aal, group = 1), color="green") + 
  xlab("Past 90 days") +
  ylab("Numeric Values") +
  ggtitle("Trump v Stock Prices in 2020") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 4), legend.title = element_blank()) 
trump_aal_plot

anat_aal_plot <- all_data_combined_tidy %>%
  ggplot(aes(all_data_combined_tidy$date, group = 1)) +
  geom_line(aes(y=close_anat, group = 1), color="red") +  
  geom_line(aes(y=close_aal, group = 1), color="green") + 
  xlab("Past 90 days") +
  ylab("Numeric Values") +
  ggtitle("ANAT Stock Prices v AAL Stock Prices in 2020") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 4), legend.title = element_blank()) 
anat_aal_plot

# Simple Linear Regression with coronavirus_count and close_price ---------

lm_fit <- all_data_combined_no_date_numeric_remove_one %>% lm(formula = close_aal ~ coronavirus_count)

lm_fit %>% 
  summary()

lm_fit %>% 
  glance() %>% 
  clean_names()

lm_fit %>% 
  tidy() %>% 
  bind_cols(lm_fit %>% confint_tidy()) %>% 
  clean_names()

# Confidence and Prediction Intervals

coronavirus_sample_data <- tibble(coronavirus_count = c(12, 19, 31))

coronavirus_sample_data %>% 
  predict(lm_fit, newdata = ., interval = "confidence") %>% 
  as_tibble()

coronavirus_sample_data %>% 
  predict(lm_fit, newdata = ., interval = "prediction") %>% 
  as_tibble()

# Assessing many models

coronavirus_augmented <- all_data_combined_no_date_numeric_remove_one %>% 
  augment(lm_fit, data = .) %>% 
  clean_names()

coronavirus_augmented %>% 
  ggplot(aes(x = coronavirus_count, y = close_aal)) +
  geom_point() +
  geom_line(aes(y = fitted), color = "pink", size = 1)

coronavirus_augmented %>% 
  ggplot(aes(x = coronavirus_count, y = close_aal)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(se = FALSE, color = "red", linetype = "dashed")

coronavirus_augmented %>% 
  ggplot(aes(x = fitted, y = sqrt(abs(std_resid)))) +
  geom_point() +
  geom_smooth(color = "red", se = FALSE)

# Multiple Linear Regression

tibble(data = list(all_data_combined_no_date_numeric_remove_one))

all_models <- tibble(data = list(all_data_combined_no_date_numeric_remove_one)) %>% 
  mutate(mod_01 = map(data, lm, formula = close_aal ~ coronavirus_count),
         mod_02 = map(data, lm, formula = close_aal ~ poly(coronavirus_count, 5)),
         mod_03 = map(data, lm, formula = close_aal ~ coronavirus_count + biden_count),
         mod_04 = map(data, lm, formula = close_aal ~ coronavirus_count + trump_count + biden_count),
         mod_05 = map(data, lm, formula = close_aal ~ coronavirus_count*biden_count),
         mod_06 = map(data, lm, formula = close_aal ~ .),
         mod_07 = map(data, lm, formula = close_aal ~ .*.))

# Make this more tidy
all_models <- all_models %>% 
  pivot_longer(cols = -data, names_to = "model_name", values_to = "model_fit")

# Assessing Models

all_models %>% 
  mutate(mod_glance = map(model_fit, glance)) %>%
  unnest(mod_glance) %>% 
  arrange(AIC) %>% 
  select(model_name, AIC, everything())

all_models %>% 
  mutate(
    mod_glance = map(model_fit, glance),
    mae  = map2_dbl(model_fit, data, mae),
    rmse = map2_dbl(model_fit, data, rmse),
    mape = map2_dbl(model_fit, data, mape)
  ) %>% 
  unnest(mod_glance) %>% 
  select(
    model_name, r.squared, adj.r.squared, 
    AIC, BIC, deviance, sigma, rmse, mae, mape
  ) %>% 
  pivot_longer(cols = -model_name, names_to = "measure", values_to = "value") %>% 
  ggplot(aes(value, model_name)) +
  geom_point() +
  facet_wrap(. ~ measure, scales = "free_x")

# Store the information within model database

all_models <- all_models %>%
  mutate(
    mod_glance  = map(model_fit, glance),
    mod_tidy    = map(model_fit, tidy),
    add_tidy    = map(model_fit, confint_tidy),
    mod_tidy    = map2(mod_tidy, add_tidy, bind_cols),
    mod_augment = map2(model_fit, data, augment)
  ) %>%
  select(-add_tidy)

# Plot 95 CI for each model

all_models %>% 
  unnest(mod_tidy) %>% 
  filter(term %in% c("coronavirus_count", "biden_count", "sanders_count", "trump_count", "close_anat")) %>% 
  ggplot(aes(model_name, estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  facet_wrap(. ~ term, scales = "free_x") +
  coord_flip()

# Compare polynomial fit (Model 2) to Model 1

all_models %>% 
  filter(model_name %in% c("mod_01", "mod_02")) %>% 
  unnest(mod_augment) %>% 
  ggplot(aes(x = coronavirus_count, y = close_aal)) +
  geom_point() +
  geom_line(aes(y = .fitted, color = model_name), size = 1)

# Cross Validation --------------------------------------------------------

all_data_validation <- tibble(train = all_data_combined_no_date_numeric_remove_one %>% sample_frac(0.8) %>% list(),
                              test  = all_data_combined_no_date_numeric_remove_one %>% setdiff(train) %>% list())

# New tibble with names and corresponding formulas

model_def <- tibble(degree = 1:5,
                    fmla = str_c("close_aal ~ poly(coronavirus_count, ", degree, ")"))

all_data_validation <- all_data_validation %>% 
  crossing(model_def)

all_data_validation <- all_data_validation %>% 
  mutate(model_fit = map2(fmla, train, lm),
         test_mse = map2_dbl(model_fit, test, mse))

all_data_validation %>% 
  select(degree, test_mse) %>% 
  arrange(test_mse)

all_data_validation %>% 
  select(degree, test_mse) %>% 
  ggplot(aes(x = degree, y = test_mse)) +
  geom_line()

# LOOCV

all_loocv <- all_data_combined_no_date_numeric_remove_one %>% 
  crossv_loo(id = "fold") %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )

all_loocv <- all_loocv %>% 
  crossing(model_def) %>% 
  mutate(
    model_fit = map2(fmla, train, lm),
    fold_mse = map2_dbl(model_fit, test, mse)
  )

all_loocv %>% 
  group_by(degree) %>% 
  summarise(test_mse = mean(fold_mse)) %>% 
  arrange(test_mse)

all_loocv %>% 
  group_by(degree) %>% 
  summarize(test_mse = mean(fold_mse)) %>% 
  ggplot(aes(x = degree, y = test_mse)) +
  geom_line()


#K-fold

all_5fold <- all_data_combined_no_date_numeric_remove_one %>% 
  crossv_kfold(5, id = "fold") %>%
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )

all_5fold <- all_5fold %>% 
  crossing(model_def) %>% 
  mutate(model_fit = map2(fmla, train, lm),
         fold_mse = map2_dbl(model_fit, test, mse))

all_5fold %>% 
  ggplot(aes(x = degree, y = fold_mse, color = fold)) +
  geom_line() 

all_5fold %>% 
  group_by(degree) %>% 
  summarize(test_mse = mean(fold_mse)) %>%
  ggplot(aes(x = degree, y = test_mse)) +
  geom_line() +
  geom_point()


# Subset Selection Methods ------------------------------------------------

# Test set for comparing

mod_comp_dat <- all_data_combined_no_date_numeric_remove_one  %>% sample_frac(0.2)

# Train set for comparing

mod_bldg_dat <- all_data_combined_no_date_numeric_remove_one %>% setdiff(mod_comp_dat)

# Helper functions (used from 6.2 in the Data Science Manual)

predict_regsubset <- function(object, fmla , new_data, model_id)
{
  if(!is.data.frame(new_data)){
    new_data <- as_tibble(new_data)
  }
  
  obj_formula <- as.formula(fmla)
  
  coef_vector <- coef(object, model_id)
  
  x_vars <- names(coef_vector)
  mod_mat_new <- model.matrix(obj_formula, new_data)[ , x_vars]
  
  pred <- as.numeric(mod_mat_new %*% coef_vector)
  
  return(pred)
}

# test MSE
test_mse_regsubset <- function(object, fmla , test_data){
  
  num_models <- object %>% summary() %>% pluck("which") %>% dim() %>% .[1]
  
  # storage
  test_mse <- rep(NA, num_models)
  
  # observed targets
  obs_target <- test_data %>% 
    as_tibble() %>% 
    pull(!!as.formula(fmla)[[2]])
  
  for(i in 1:num_models){
    pred <- predict_regsubset(object, fmla, test_data, model_id = i)
    test_mse[i] <- mean((obs_target - pred)^2)
  }
  
  # test errors for every class 
  tibble(model_index = 1:num_models,
         test_mse    = test_mse)
}

# Best subset: 5-fold CV

data_bestsubset_cv <- mod_bldg_dat %>% 
  crossv_kfold(5, id = "folds") %>% 
  mutate(
    fmla = "close_aal ~ . ",
    model_fits = map2(fmla, train, 
                      ~ regsubsets(as.formula(.x), data = .y, nvmax = 19)),
    model_fold_mse = pmap(list(model_fits, fmla ,test), test_mse_regsubset)
  )

# Forward selection: 5-fold CV
data_fwd_cv <- mod_bldg_dat %>% 
  crossv_kfold(5, id = "folds") %>% 
  mutate(
    fmla = "close_aal ~ . ",
    model_fits = map2(fmla, train, 
                      ~ regsubsets(as.formula(.x), data = .y, nvmax = 19, method = "forward")),
    model_fold_mse = pmap(list(model_fits, fmla ,test), test_mse_regsubset)
  )

# Backward selection: 5-fold CV
data_back_cv <- mod_bldg_dat %>% 
  crossv_kfold(5, id = "folds") %>% 
  mutate(
    fmla = "close_aal ~ . ",
    model_fits = map2(fmla, 
                      train, 
                      ~ regsubsets(as.formula(.x), data = .y, nvmax = 19, method = "backward")),
    model_fold_mse = pmap(list(model_fits, fmla ,test), test_mse_regsubset)
  )

# Plot best subset results

data_bestsubset_cv %>% 
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>% 
  summarize(test_mse = mean(test_mse)) %>% 
  ggplot(aes(model_index, test_mse)) + 
  geom_line()

data_bestsubset_cv %>% 
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>%  
  summarize(test_mse = mean(test_mse)) %>% 
  arrange(test_mse)

# Plot forward selection test MSE

data_fwd_cv %>% 
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>% 
  summarize(test_mse = mean(test_mse)) %>% 
  ggplot(aes(model_index, test_mse)) + 
  geom_line()

data_fwd_cv %>% 
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>%  
  summarize(test_mse = mean(test_mse)) %>% 
  arrange(test_mse) 


# Plot back selection test MSE

data_back_cv %>% 
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>% 
  summarize(test_mse = mean(test_mse)) %>% 
  ggplot(aes(model_index, test_mse)) + 
  geom_line()

data_back_cv %>% 
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>%  
  summarize(test_mse = mean(test_mse)) %>% 
  arrange(test_mse)

# Create a data table

data_regsubsets <- tibble(
  train = mod_bldg_dat %>% list(),
  test  = mod_comp_dat %>% list()
) %>%
  mutate(
    best_subset = map(train, ~ regsubsets(close_aal ~ . , 
                                          data = .x , nvmax = 5)),
    fwd_selection = map(train, ~ regsubsets(close_aal ~ . , 
                                            data = .x, nvmax = 5, 
                                            method = "forward")),         
    back_selection = map(train, ~ regsubsets(close_aal~ . , 
                                             data = .x, nvmax = 5, 
                                             method = "backward"))
  ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

data_regsubsets

data_regsubsets %>% 
  pluck("fit") %>% 
  map2(c(5, 5, 5), ~ coef(.x, id = .y)) %>% 
  map2(c("best", "fwd", "back"), ~ enframe(.x, value = .y)) %>% 
  reduce(full_join) %>% 
  knitr::kable(digits = 3)


# Ridge and Lasso ---------------------------------------------------------

# lambda grid to search -- use for ridge regression (30 values)
lambda_grid <- 10^seq(-2, 10, length = 30)

# ridge regression: 5-fold cv
ridge_cv <- mod_bldg_dat %>% 
  glmnetUtils::cv.glmnet(
    formula = close_aal ~ . , 
    data = ., 
    alpha = 0, 
    nfolds = 5,
    lambda = lambda_grid
  )

plot(ridge_cv)

ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

# Lasso using 5-folds
lasso_cv <- mod_bldg_dat %>% 
  glmnetUtils::cv.glmnet(
    formula = close_aal ~ . - date, 
    data = ., 
    alpha = 1, 
    nfolds = 5
  )

plot(lasso_cv)

lasso_lambda_1se <- lasso_cv$lambda.1se
lasso_lambda_min <- lasso_cv$lambda.min

data_glmnet <- tibble(
  train = mod_bldg_dat %>% list(),
  test  = mod_comp_dat %>% list()
) %>%
  mutate(
    ridge_min = map(train, ~ glmnetUtils::glmnet(close_aal ~ . , data = .x,
                                                 alpha = 0, lambda = ridge_lambda_min)),
    ridge_1se = map(train, ~ glmnetUtils::glmnet(close_aal ~ . , data = .x,
                                                 alpha = 0, lambda = ridge_lambda_1se)),
    lasso_min = map(train, ~ glmnetUtils::glmnet(close_aal ~ . , data = .x,
                                                 alpha = 1, lambda = lasso_lambda_min)),
    lasso_1se = map(train, ~ glmnetUtils::glmnet(close_aal ~ . , data = .x,
                                                 alpha = 1, lambda = lasso_lambda_1se))
  ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

data_glmnet %>% 
  pluck("fit") %>% 
  map( ~ coef(.x) %>% 
         as.matrix() %>% 
         as.data.frame() %>% 
         rownames_to_column("name")) %>%
  reduce(full_join, by = "name") %>% 
  mutate_if(is.double, ~ if_else(. == 0, NA_real_, .)) %>% 
  rename(ridge_min = s0.x,
         ridge_1se = s0.y,
         lasso_min = s0.x.x,
         lasso_1se = s0.y.y) %>% 
  knitr::kable(digits = 100) #selecting 100 digits at the end

regsubset_error <- data_regsubsets %>% 
  mutate(test_mse = map2(fit, test, ~ test_mse_regsubset(.x, close_aal ~ . , .y))) %>% 
  unnest(test_mse) %>% 
  group_by(method) %>% 
  filter(model_index == max(model_index)) %>% 
  select(method, test_mse) %>% 
  ungroup()

# Test error for ridge and lasso fits
glmnet_error <- data_glmnet %>% 
  mutate(pred = map2(fit, test, predict),
         test_mse = map2_dbl(test, pred, ~ mean((.x$close_aal - .y)^2))) %>% 
  unnest(test_mse) %>% 
  select(method, test_mse)

# Test errors ccombined and organzied
regsubset_error %>% 
  bind_rows(glmnet_error) %>% 
  arrange(test_mse) %>%
  knitr::kable(digits = 1000)


# Polynomial Regression ---------------------------------------------------

close_price_split_info <- all_data_combined_no_date_numeric_remove_one %>% 
  initial_split(prop = 0.80)

close_price_split <- tibble(
  train = close_price_split_info %>% training() %>% list(),
  test = close_price_split_info %>% testing() %>% list()
  )

poly_models <- tibble(
  fmla = str_c("close_aal ~ poly(coronavirus_count + trump_count + biden_count + sanders_count + close_anat, ", 1:5, ")"),
  model_name = str_c("degree ", 1:5)
) %>% 
  mutate(fmla = map(fmla, as.formula)
  )

close_cv <- close_price_split %>% 
  pluck("train", 1) %>% 
  modelr::crossv_kfold(k = 5, id = "fold") %>% # kfold w/ 5 folds
  crossing(poly_models) %>% 
  mutate(
    model_fit = map2(fmla, train, lm),
    fold_mse = map2_dbl(model_fit, test, modelr::mse)
  )

close_cv %>% 
  group_by(model_name) %>% 
  summarize(
    test_mse = mean(fold_mse)
  ) %>% 
  mutate(
    pct_diff = 100 * (test_mse - min(test_mse))/min(test_mse)
  ) %>% 
  arrange(test_mse)

close_poly_fits <- poly_models %>% 
  filter(model_name %in% c("degree 1", "degree 2")) %>% 
  crossing(close_price_split) %>% 
  mutate(
    model_fit = map2(fmla, train, lm),
    test_mse = map2_dbl(model_fit, test, modelr::mse)
  )

target_var <- close_price_split_info %>% 
  testing() %>% 
  pull(close_aal) %>% 
  var()

close_poly_fits %>% 
  select(model_name, test_mse) %>% 
  arrange(test_mse)

close_price_split_info %>% 
  testing() %>% 
  ggplot(aes(x = coronavirus_count + trump_count + biden_count + sanders_count + close_anat, y = close_aal)) + 
  xlab("All Predictors") +
  ylab("AAL Close Prices") +
  geom_point(alpha = 0.4) +
  geom_smooth(data = close_price_split_info %>% training(),
              method = "lm",
              formula = "y ~ poly(x, 5)",
              se = FALSE
  ) 

