#----LIBRARIES----
library(tidyverse)
library(data.table)
library(glmnet)
library(plotmo)
library(rsample)
library(ggplot2)
library(yardstick)
library(rpart)
library(rpart.plot)
library(tidymodels)
library(ranger)

#----DATA PRE-PROCESSING----
dataset <- readRDS("./CICIDS2017_clean.rds")

set.seed(1234)
dataset_sample <- dataset[sample(nrow(dataset), 1000),]

# Remove obs. belonging to under-represented classes (<50)
dataset_sample <- dataset_sample %>%
  group_by(Label) %>%
  filter(n() >= 50) %>%
  ungroup()

dataset_sample$Label <- droplevels(dataset_sample$Label)

split <- initial_split(dataset_sample, strata = Label)
train <- training(split)
test  <- testing(split)

#----LASSO----
lasso <- glmnet(x = train %>% select(-Label) %>% as.matrix(), 
                y = train$Label, 
                alpha = 1, 
                nlambda = 200, 
                family = "multinomial")

cv.lasso <- cv.glmnet(x = train %>% select(-Label) %>% as.matrix(), 
                      y = train$Label, 
                      alpha = 1, 
                      nlambda = 200, 
                      family = "multinomial")

lambda.min <- cv.lasso$lambda.min
lambda.1se <- cv.lasso$lambda.1se

lasso_colors <- hue_pal(h = c(500,1000), l = 65, c = 100)(10)

par(mfrow = c(2, 2), mar = c(5, 5, 2, 2))
plot(lasso, xvar = "lambda", label = T,
     xlab = expression(log(lambda)), lwd = 2.5, col = lasso_colors)
dev.off()

plot(cv.lasso,
     xlab        = expression(log(lambda)),
     ylab        = "Deviance",
     sign.lambda = 1)
abline(v = log(lambda.min),   col = "red",    lty = 2)
abline(v = log(lambda.1se),   col = "gray50", lty = 3)
legend("bottomright",
       legend = c(expression(lambda[min]), expression(lambda[1*se])),
       col    = c("red", "gray50"),
       lty    = c(2, 3),
       lwd    = 2,
       bty    = "n",
       cex    = 1.5)

#----LASSO: FEATURE SELECTION----
X <- train %>% select(-Label) %>% as.matrix()
sds <- apply(X, 2, sd)

classnames <- lasso$classnames
coefs_list <- lapply(classnames, function(cl) {
  coef(lasso, s = lambda.min)[[cl]]
})

names(coefs_list) <- classnames

standardized_coefs <- purrr::map_dfr(names(coefs_list), function(cl) {
  beta <- as.matrix(coefs_list[[cl]])
  beta <- beta[-1, , drop = FALSE]
  beta_std <- beta * sds[rownames(beta)]
  
  data.frame(
    Feature = rownames(beta_std),
    Coefficient = as.numeric(beta_std),
    Class = cl
  )
})

filtered <- standardized_coefs %>%
  group_by(Feature) %>%
  filter(any(Coefficient != 0)) %>%
  ungroup()

n_classes <- length(unique(filtered$Class))
n_features <- length(unique(filtered$Feature))

ggplot(filtered, aes(x = Feature, y = Class, fill = Coefficient)) +
  geom_tile(color = NA) +
  annotate("rect", 
           xmin = 0.5, xmax = n_features + 0.5,
           ymin = 0.5, ymax = n_classes + 0.5,
           color = "black", fill = NA, size = 0.3) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, name = expression(beta ~ "(standardized)")
  ) +
  coord_fixed() +
  labs(
    x = "Feature",
    y = "Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.title = element_text(size = 12),
  ) +
  coord_flip()

#----LASSO: PREDICTIONS----
lasso_class <- predict(cv.lasso, newx = test %>% select(-Label) %>% as.matrix(), 
                       s = lambda.min, type = "class")
lasso_class <- factor(lasso_class, levels = levels(test$Label))
lasso_probs <- predict(cv.lasso, newx = test %>% select(-Label) %>% as.matrix(), 
                       s = lambda.min, type = "response")
lasso_probs_df <- as.data.frame(lasso_probs)

#----CART----
cart_full <- rpart(Label ~ ., data    = train, method  = "class", cp=0)  
plotcp(cart_full)
best.cp <- cart_full$cptable[which.min(cart_full$cptable[,"xerror"]), "CP"]
model_cart <- prune.rpart(cart_full, cp  = best.cp) #Pruning

#Pruned tree at cp=0.01
model_cart <- rpart(Label ~ ., data = train, method = "class", cp = 0.01)
rpart.plot(model_cart, type = 5, extra = 100, fallen.leaves = T)

cart_class <- predict(model_cart, newdata = test, type = "class")
cart_probs_df <- predict(model_cart, newdata = test, type = "prob") %>% as.data.frame()

#----RF----
model_rf <- ranger(
  Label ~ ., 
  data = train,
  num.trees = 200,
  importance = "impurity",
  probability = TRUE
)

preds_rf <- predict(model_rf, data = test)

rf_class <- preds_rf$predictions %>% 
  as.data.frame() %>%
  mutate(prediction = colnames(.)[max.col(.)]) %>%
  pull(prediction) %>%
  factor(levels = levels(test$Label))

rf_probs_df <- as.data.frame(preds_rf$predictions)

#----RF: FEATURE SELECTION----
var_imp <- data.frame(
  Variable   = names(model_rf$variable.importance),
  Importance = as.numeric(model_rf$variable.importance)
)

df_imp <- var_imp %>%
  arrange(desc(Importance)) %>%
  mutate(Variable = factor(Variable, levels = rev(Variable)))

ggplot(df_imp, aes(x = Variable, y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    x = NULL,
    y = "Mean Decrease in Gini"
  ) +
  theme(
    axis.text.y = element_text(size = 12)
  )

#----MODEL EVAL----
test_results <- tibble(
  Label = test$Label,
  LASSO = lasso_class,
  CART = cart_class,
  RF = rf_class
) %>%
  bind_cols(
    lasso_probs_df %>% rename_with(~paste0("LASSO_prob_", .)),
    cart_probs_df %>% rename_with(~paste0("CART_prob_", .)),
    rf_probs_df %>% rename_with(~paste0("RF_prob_", .))
  )


test_results <- test_results %>%
  mutate(
    LASSO = factor(LASSO, levels = levels(Label)),
    CART  = factor(CART,  levels = levels(Label)),
    RF    = factor(RF,    levels = levels(Label))
  )

# Confusion matrices
conf_lasso <- table(Predetto = test_results$LASSO, Reale = test_results$Label)
conf_cart  <- table(Predetto = test_results$CART,  Reale = test_results$Label)
conf_rf    <- table(Predetto = test_results$RF,    Reale = test_results$Label)

plot_cm <- function(cm, title) {
  as.data.frame(cm) %>%
    ggplot(aes(x = Reale, y = Predetto, fill = Freq)) +
    geom_raster() + 
    geom_text(aes(label = Freq), color = "white", size = 6) +
    labs(title = title, x = "Actual", y = "Predicted") +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
    )
}

plot_cm(conf_lasso, title = "")
plot_cm(conf_cart, title = "")
plot_cm(conf_rf, title = "")

# Metrics
metrics_df <- function(truth, prediction, model_name) {
  tibble(
    Modello = model_name,
    Accuracy = accuracy_vec(truth, prediction),
    Precision = precision_vec(truth, prediction),
    Recall = recall_vec(truth, prediction),
    F1 = f_meas_vec(truth, prediction)
  )
}

metrics <- bind_rows(
  metrics_df(test_results$Label, test_results$LASSO, "LASSO"),
  metrics_df(test_results$Label, test_results$CART, "CART"),
  metrics_df(test_results$Label, test_results$RF,   "Random Forest")
)

print(metrics)

#----PER CLASS PER MODEL METRICS----
get_per_class_metrics <- function(truth, estimate, model_name = "") {
  labels <- levels(truth)
  
  map_dfr(labels, function(lbl) {
    binary_df <- tibble(
      truth_bin = factor(ifelse(truth == lbl, "yes", "no"), levels = c("yes", "no")),
      estimate_bin = factor(ifelse(estimate == lbl, "yes", "no"), levels = c("yes", "no"))
    )
    
    tibble(
      Modello = model_name,
      Classe = lbl,
      Accuracy = accuracy(binary_df, truth = truth_bin, estimate = estimate_bin)$.estimate %>% round(3),
      Precision = precision(binary_df, truth = truth_bin, estimate = estimate_bin, estimator = "binary")$.estimate %>% round(3),
      Recall = recall(binary_df, truth = truth_bin, estimate = estimate_bin, estimator = "binary")$.estimate %>% round(3),
      F1 = f_meas(binary_df, truth = truth_bin, estimate = estimate_bin, estimator = "binary")$.estimate %>% round(3)
    )
  })
}

metrics_lasso <- get_per_class_metrics(test_results$Label, test_results$LASSO, "LASSO")
metrics_cart  <- get_per_class_metrics(test_results$Label, test_results$CART, "CART")
metrics_rf    <- get_per_class_metrics(test_results$Label, test_results$RF,   "Random Forest")

print(metrics_lasso)
print(metrics_cart)
print(metrics_rf)