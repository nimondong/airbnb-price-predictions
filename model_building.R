# MODEL BUILDING

# Loading Packages --------------------------------------------------------
library(tidyverse)
library(modelr)
library(ranger)
library(vip)
library(pdp)
library(xgboost)
library(rsample)
library(janitor)
library(keras)
library(tensorflow)

# Set Seed ----------------------------------------------------------------
set.seed(10221998)

# Load Dataset ------------------------------------------------------------
model_data <- read_csv("data/processed/model_data.csv") %>%
  select(-latitude,
         -longitude,
         -review_scores_rating) %>%
  clean_names()

model_data_split <- model_data %>%
  crossv_kfold(5, id = "fold") %>%
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble))

# Helper Functions --------------------------------------------------------
mse_ranger <- function(model, test, outcome) {
  if(!is_tibble(test)){
    test <- test %>% as_tibble()
  }
  preds <- predict(model, test)$predictions
  mse <- mean((test[[outcome]] - preds)^2)
  return(mse)
}

xgb_matrix <- function(dat, outcome, exclude_vars){
  dat_types <- dat %>% map_chr(class)
  outcome_type <- class(dat[[outcome]])
  
  if("chr" %in% dat_types){
    print("You must encode characters as factors.")
    return(NULL)
    
  } else {
    if(outcome_type == "factor" & nlevels(dat[[outcome]]) == 2){
      tmp <- dat %>% select(outcome) %>% onehot::onehot() %>% predict(dat)  
      lab <- tmp[,1]
    } else {
      lab <- dat[[outcome]]
    }
    
    mat <- dat %>% dplyr::select(-outcome, -all_of(exclude_vars)) %>%
      onehot::onehot() %>%
      predict(dat)
    
    return(xgb.DMatrix(data = mat, 
                       label = lab))
  }
  
}

xg_error <- function(model, test_mat, metric = "mse"){
  preds = predict(model, test_mat)
  vals = getinfo(test_mat, "label")
  
  if(metric == "mse"){
    err <- mean((preds - vals)^2)
    
  } else if(metric == "misclass") {
    err <- mean(preds != vals)
  }
  
  return(err)
}

# Bagging and Random Forests ----------------------------------------------
model_rf_reg <- model_data_split %>% 
  crossing(mtry = 1:(ncol(model_data) - 2)) %>%
  mutate(model = map2(.x = train, .y = mtry, 
                      .f = function(x, y) ranger(price ~ . - id,
                                                 mtry = y, 
                                                 data = x, 
                                                 splitrule = "variance",
                                                 importance = "impurity")),
         train_err = map2(model, train, mse_ranger, outcome = "price"),
         test_err = map2(model, test, mse_ranger, outcome = "price"), 
         oob_err = map(.x = model, 
                       .f = function(x) x[["prediction.error"]]))

model_rf_best <- model_rf_reg %>%
  unnest(oob_err) %>%
  unnest(train_err) %>%
  unnest(test_err) %>%
  group_by(mtry) %>%
  summarize(oob_error = mean(oob_err),
            train_error = mean(train_err),
            test_error = mean(test_err)) %>%
  arrange(test_error) 

model_rf_best

model_rf_best %>%  
  ggplot() +
  geom_line(aes(mtry, oob_error, color = "OOB Error")) +
  geom_line(aes(mtry, train_error, color = "Training Error")) +
  geom_line(aes(mtry, test_error, color = "Test Error")) + 
  labs(x = "mtry", y = "MSE") + 
  scale_color_manual("", values = c("purple", "red", "blue")) + 
  theme_bw()

model_rf_variable <- ranger(price ~ . - id,
                            data = model_data,
                            mtry = 8,
                            importance = "impurity",
                            splitrule = "variance")
vip(model_rf_variable)


# XGBoost -----------------------------------------------------------------
model_xg_reg <- model_data_split %>%
  crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>%
  mutate(train_mat = map(train, xgb_matrix, outcome = "price", exclude_vars = "id"),
         test_mat = map(test, xgb_matrix, outcome = "price", exclude_vars = "id"),
         xg_model = map2(.x = train_mat, .y = learn_rate, 
                         .f = function(x, y) xgb.train(params = list(eta = y,
                                                                     depth = 10,
                                                                     objective = "reg:squarederror"), 
                                                       data = x, 
                                                       nrounds = 500,
                                                       silent = TRUE)), 
         xg_train_mse = map2(xg_model, train_mat, xg_error, metric = "mse"),
         xg_test_mse = map2(xg_model, test_mat, xg_error, metric = "mse"))

model_xg_best <- model_xg_reg %>%
  unnest(xg_train_mse) %>%
  unnest(xg_test_mse) %>%
  group_by(learn_rate) %>%
  summarize(train_error = mean(xg_train_mse),
            test_error = mean(xg_test_mse)) %>%
  arrange(test_error) 

model_xg_best

model_xg_best %>%  
  ggplot() +
  geom_line(aes(learn_rate, train_error, color = "Training Error")) +
  geom_line(aes(learn_rate, test_error, color = "Test Error")) + 
  labs(x = "mtry", y = "MSE") + 
  scale_color_manual("", values = c("red", "blue")) + 
  theme_bw()

xg_reg_mod <- model_xg_reg %>% 
  arrange(unlist(xg_test_mse)) %>%
  pluck("xg_model", 1)

vip(xg_reg_mod)


# Neural Net --------------------------------------------------------------

set.seed(10221998)

model_train <- model_data %>% select(-id) %>% sample_frac(0.08) 

model_test <- model_data %>% select(-id) %>% setdiff(model_train)


model_train_ohe <- model_train %>%
  select(-price)

model_test_ohe <- model_test %>%
  select(-price)

# standardizing data
means_train_dat <- apply(model_train_ohe, 2, mean)
sd_train_dat <- apply(model_test_ohe, 2, sd)

train_data <- scale(model_train_ohe, 
                    center = means_train_dat, 
                    scale = sd_train_dat)

test_data <- scale(model_test_ohe, 
                   center = means_train_dat, 
                   scale = sd_train_dat)

# Modeling

# creating targets
train_targets <- model_train %>%
  pull(price)

test_targets <- model_test %>%
  pull(price)

build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 128, 
                activation = "relu",
                kernel_regularizer = regularizer_l1(0.01),
                input_shape = dim(train_data)[[2]]) %>% 
    layer_dense(units = 256, 
                activation = "relu",
                kernel_regularizer = regularizer_l1(0.01)) %>% 
    layer_dense(units = 256, 
                activation = "relu",
                kernel_regularizer = regularizer_l1(0.01)) %>% 
    layer_dense(units = 512, 
                activation = "relu",
                kernel_regularizer = regularizer_l1(0.01)) %>% 
    layer_dense(units = 1) 
  
  model %>% compile(
    optimizer = "rmsprop", 
    loss = "mse", 
    metrics = c("mse")
  )
}

k <- 5
indices <- sample(1:nrow(train_data))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) 
num_epochs <- 100
mae_score <- c()
mse_score <- c()

for (i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  model <- build_model()
  
  model %>% fit(partial_train_data, partial_train_targets,
                epochs = num_epochs, batch_size = 1, verbose = 0)
  
  results <- model %>% evaluate(val_data, val_targets, verbose = 0)
  mse_score <- c(mse_score, results$mse)
}  

model <- build_model()
summary(model)

mean(mse_score)


# L1 regulaization made it better
# Doubling epochs: barely increased accuracy
# Adding 4th layer -> didn't help much













