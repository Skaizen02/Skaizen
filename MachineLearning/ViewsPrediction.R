library(keras)
library(lime)

data_raw = df
data_tbl <- select(data_raw, -c("text", "time"))

set.seed(100)
train_test_split <- initial_split(data_tbl, prop = 0.8)
train_test_split

train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split) 

x_train <- select(train_tbl, -c("views")) %>% as.matrix()
x_test <- select(test_tbl, -c("views")) %>% as.matrix()
# 
# x_train <- to_categorical(x_train$wday, 7) %>%
#             data.frame() %>%
#             cbind(select(x_train, -c("wday"))) %>%
#             as.matrix()
#   
# x_test <- to_categorical(x_test$wday, 7) %>%
#   data.frame() %>%
#   cbind(select(x_test, -c("wday"))) %>%
#   as.matrix()

y_train <- train_tbl$views
y_test <- test_tbl$views

# y_train <- y_train/max(y_train)
# y_test <- y_test/max(y_train)

model <- keras_model_sequential()
model %>%
    layer_dense(units = round(ncol(x_train)*1.2), activation = "relu", input_shape = c(ncol(x_train))) %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 1, activation = "relu")

summary(model)

model %>% compile(
  loss = "mean_absolute_error",
  optimize = "sgd",
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test)


# Setup lime::model_type() function for keras
model_type.keras.models.Sequential <- function(x, ...) {
  return("classification")
}

# Setup lime::predict_model() function for keras
predict_model.keras.models.Sequential <- function(x, newdata, type, ...) {
  pred <- predict_proba(object = x, x = as.matrix(newdata))
  return(data.frame(Yes = pred, No = 1 - pred))
}

# Test our predict_model() function
predict_model(x = model_keras, newdata = x_test_tbl, type = 'raw') %>%
  tibble::as_tibble()

# Run lime() on training set
explainer <- lime::lime(
  x              = x_train_tbl, 
  model          = model_keras, 
  bin_continuous = FALSE)

# Run explain() on explainer
explanation <- lime::explain(
  x_test_tbl[1:10,], 
  explainer    = explainer, 
  n_labels     = 1, 
  n_features   = 4,
  kernel_width = 0.5)

plot_features(explanation) +
  labs(title = "LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

plot_explanations(explanation) +
  labs(title = "LIME Feature Importance Heatmap",
       subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

# Feature correlations to Churn
corrr_analysis <- data.frame(x_train) %>%
  mutate(views = y_train) %>%
  correlate() %>%
  focus(views) %>%
  rename(feature = rowname) %>%
  arrange(abs(views)) %>%
  mutate(feature = as_factor(feature)) 
corrr_analysis

# Correlation visualization
corrr_analysis %>%
  ggplot(aes(x = views, y = fct_reorder(feature, desc(views)))) +
  geom_point() +
  # Positive Correlations - more views
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[2]], 
               data = corrr_analysis %>% filter(views > 0)) +
  geom_point(color = palette_light()[[2]], 
             data = corrr_analysis %>% filter(views > 0)) +
  # Negative Correlations - less views
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[1]], 
               data = corrr_analysis %>% filter(views < 0)) +
  geom_point(color = palette_light()[[1]], 
             data = corrr_analysis %>% filter(views < 0)) +
  # Vertical lines
  geom_vline(xintercept = 0, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = -0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = 0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  # Aesthetics
  theme_tq() +
  labs(title = "Keywords/Views Correlation Analysis",
       subtitle = "Positive Correlations (more views), Negative Correlations (less views)",
       y = "Feature Importance") + 
  theme(text = element_text(size=8))



