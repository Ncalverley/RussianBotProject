####################################################################################
##                          CONSTRUCT PREDICTIVE MODEL
##
## This file will import the final cleaned data set, and initialize a model to 
## predict group membership.
####################################################################################

load(file = "data/analysisData.RData")

####################################################################################
## Define the fields to be used in training
####################################################################################

trainFields <- c(
  "followers",
  "total_tweets",
  "sentiment_republicans",
  "affinity_republicans",
  "emotion_republicans",
  "sentiment_democrats",
  "affinity_democrats",
  "emotion_democrats",
  "hashtag.1",
  "hashtag.2",
  "hashtag.3",
  "hashtag.4",
  "hashtag.5"
)
outcomeVar <- "troll"

####################################################################################
## Clean the data
####################################################################################
analysisData <- analysisData[,c(match(trainFields, names(analysisData)),
                                match(outcomeVar, names(analysisData)))]
analysisData <- na.omit(analysisData)

####################################################################################
## Control observations vastly outnumber treatment observations. Let's downsample.
####################################################################################

# temp_troll <- analysisData[analysisData$troll == 1, ]
# temp_legit <- analysisData[analysisData$troll == 0, ]
# 
# idx <- sample(row.names(temp_legit), size = nrow(temp_troll)*3, replace = FALSE)
# temp_legit <- temp_legit[idx, ]; rm(idx)
# 
# analysisData <- rbind(temp_troll, temp_legit); rm(temp_troll, temp_legit)

####################################################################################
## Prepare the data for a Neural Network model
####################################################################################

## Select the training split
train_test_split <- initial_split(analysisData, prop = 0.8)

# Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split) 

# Create recipe
rec_obj <- recipe(troll ~ ., data = train_tbl) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = train_tbl)

# Predictors
x_train_tbl <- bake(rec_obj, newdata = train_tbl) %>% select(-troll)
x_test_tbl  <- bake(rec_obj, newdata = test_tbl) %>% select(-troll)

# Response variables for training and testing sets
y_train_vec <- train_tbl$troll
y_test_vec  <- test_tbl$troll

####################################################################################
## Define the model for LIME
####################################################################################

# Building our Artificial Neural Network
model_keras <- keras_model_sequential()

model_keras %>% 
  
  # First hidden layer
  layer_dense(
    units              = 16, 
    activation         = "relu", 
    input_shape        = ncol(x_train_tbl)) %>% 
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Output layer
  layer_dense(
    units              = 1, 
    activation         = "sigmoid") %>% 
  
  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )

# Fit the keras model to the training data
history <- fit(
  object           = model_keras, 
  x                = as.matrix(x_train_tbl), 
  y                = y_train_vec,
  batch_size       = 32, 
  epochs           = 35,
  validation_split = 0.30
)

# Print a summary of the training history
print(history)

####################################################################################
## HIGHEST OBSERVED ACCURACY USING PROTOTYPE DATA: 0.9235
## HIGHEST OBSERVED ACCURACY USING FULL DATA: 
####################################################################################


####################################################################################
## RUN MODEL PERFORMANCE AND FEATURE ANALYSIS
####################################################################################

# Predicted Class
yhat_keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()

# Predicted Class Probability
yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()

# Format test data and predictions for yardstick metrics
estimates_keras_tbl <- tibble(
  truth      = as.factor(y_test_vec) %>% fct_recode(yes = "1", no = "0"),
  estimate   = as.factor(yhat_keras_class_vec) %>% fct_recode(yes = "1", no = "0"),
  class_prob = yhat_keras_prob_vec
)

options(yardstick.event_first = FALSE)

# Confusion Table
estimates_keras_tbl %>% conf_mat(truth, estimate)

# Accuracy
estimates_keras_tbl %>% metrics(truth, estimate)

# Setup lime::model_type() function for keras
model_type.keras.models.Sequential <- function(x, ...) {
  "classification"
}

# Setup lime::predict_model() function for keras
predict_model.keras.models.Sequential <- function(x, newdata, type, ...) {
  pred <- predict_proba(object = x, x = as.matrix(newdata))
  return(data.frame(Yes = pred, No = 1 - pred))
}

# Test our predict_model() function
tibbleData <- predict_model(x = model_keras, newdata = x_test_tbl, type = 'raw')
tibbleData <- tibble::as_tibble(tibbleData)

# Run lime() on training set
explainer <- lime::lime(
  x              = x_train_tbl, 
  model          = model_keras, 
  bin_continuous = FALSE
)

# Run explain() on explainer
explanation <- lime::explain(
  x_test_tbl[1:10, ], 
  explainer    = explainer, 
  n_labels     = 1, 
  n_features   = 4,
  kernel_width = 0.5
)

## Plot the feature explanations
# plot_features(explanation) +
#   labs(title = "LIME Feature Importance Visualization",
#        subtitle = "Hold Out (Test) Set, First 10 Cases Shown")
# 
# plot_explanations(explanation) +
#   labs(title = "LIME Feature Importance Heatmap",
#        subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

# Feature correlations to troll
corrr_analysis <- x_train_tbl %>%
  mutate("troll" = y_train_vec) %>%
  correlate() %>%
  focus(troll) %>%
  rename(feature = rowname) %>%
  arrange(abs(troll)) %>%
  mutate(feature = as_factor(feature)) 
corrr_analysis

# Correlation visualization
corrr_analysis %>%
  ggplot(aes(x = troll, y = fct_reorder(feature, desc(troll)))) +
  geom_point() +
  # Positive Correlations - Contribute to churn
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[2]], 
               data = corrr_analysis %>% filter(troll > 0)) +
  geom_point(color = palette_light()[[2]], 
             data = corrr_analysis %>% filter(troll > 0)) +
  # Negative Correlations - Prevent troll
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[1]], 
               data = corrr_analysis %>% filter(troll < 0)) +
  geom_point(color = palette_light()[[1]], 
             data = corrr_analysis %>% filter(troll < 0)) +
  # Vertical lines
  geom_vline(xintercept = 0, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = -0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = 0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  # Aesthetics
  theme_tq() +
  labs(title = "Troll Correlation Analysis",
       subtitle = "Positive Correlations (associated with Trolls), Negative Correlations (associated with Legit users)",
       y = "Feature Importance")


