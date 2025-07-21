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
dataset_sample <- dataset[sample(nrow(dataset), 1000000),]

dataset_sample <- dataset_sample %>%
  mutate(Label = ifelse(Label == "BENIGN", "BENIGN", "ATTACK")) %>%
  mutate(Label = as.factor(Label))

split <- initial_split(dataset_sample, strata = Label)
train <- training(split)
test  <- testing(split)

#----LASSO----
lasso <- glmnet(x = train %>% select(-Label) %>% as.matrix(), 
                y = train$Label, 
                alpha = 1, 
                nlambda = 200, 
                family = "binomial")

cv.lasso <- cv.glmnet(x = train %>% select(-Label) %>% as.matrix(), 
                      y = train$Label, 
                      alpha = 1, 
                      nlambda = 200, 
                      family = "binomial")

lambda.min <- cv.lasso$lambda.min
lambda.1se <- cv.lasso$lambda.1se

lasso_colors <- hue_pal(h = c(500,1000), l = 65, c = 100)(10)
plot_glmnet(lasso,
            xvar="lambda",
            label = 8,
            col = lasso_colors,
            lwd = 2.2,
            xlab = expression(log(lambda))
            )

par(mar = c(5, 4, 4, 2) + 0.1)
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
coefs <- coef(cv.lasso, s = lambda.min)

coefs_df <- data.frame(
  Variabile = rownames(coefs),
  Coefficiente = as.vector(coefs)
) %>%
  filter(Variabile != "(Intercept)" & Coefficiente != 0) %>%
  mutate(Importanza = abs(Coefficiente)) %>%
  arrange(desc(Importanza))

ggplot(coefs_df[1:10, ], aes(x = reorder(Variabile, Importanza), y = Importanza)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(x="", y = "|Coefficient|") +
  theme_minimal(base_size = 20)

#----LASSO: PREDICTIONS----
lasso_class <- predict(cv.lasso, newx = test %>% select(-Label) %>% as.matrix(), 
                       s = lambda.min, type = "class")
lasso_class <- factor(lasso_class, levels = levels(test$Label))
lasso_probs <- predict(cv.lasso, newx = test %>% select(-Label) %>% as.matrix(), 
                       s = lambda.min, type = "response") %>% as.vector()

#----CART----
cart_full <- rpart(Label ~ ., data    = train, method  = "class", cp=0)  
plotcp(cart_full)
best.cp <- cart_full$cptable[which.min(cart_full$cptable[,"xerror"]), "CP"]
model_cart <- prune.rpart(cart_full, cp  = best.cp) #Pruning

#Pruned tree at cp=0.01
model_cart <- rpart(Label ~ ., data = train, method = "class", cp = 0.01)
rpart.plot(model_cart, type = 5, extra = 100, fallen.leaves = T)

cart_class <- predict(model_cart, newdata = test, type = "class")
cart_probs <- predict(model_cart, newdata = test, type = "prob")[, "BENIGN"]

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
  as_tibble() %>%
  mutate(prediction = if_else(ATTACK > BENIGN, "ATTACK", "BENIGN")) %>%
  pull(prediction) %>%
  factor(levels = c("BENIGN", "ATTACK"))

rf_probs <- preds_rf$predictions[, "BENIGN"]

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
  Label = factor(test$Label, levels = c("BENIGN", "ATTACK")),
  LASSO = lasso_class,
  CART = cart_class,
  RF = rf_class,
  LASSO_prob = lasso_probs,
  CART_prob = cart_probs,
  RF_prob = rf_probs
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
    geom_tile() +
    geom_text(aes(label = Freq), color = "white", size = 9) +
    labs(title = title, x = "Actual", y = "Predicted") +
    theme_minimal(base_size = 20)
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

# ROC curve
roc_df_lasso <- roc_curve(test_results, truth = Label, LASSO_prob) %>%
  mutate(Model = "LASSO")
roc_df_cart <- roc_curve(test_results, truth = Label, CART_prob) %>%
  mutate(Model = "CART")
roc_df_rf <- roc_curve(test_results, truth = Label, RF_prob) %>%
  mutate(Model = "Random Forest")

roc_combined <- bind_rows(roc_df_lasso, roc_df_cart, roc_df_rf)

ggplot(roc_combined, aes(x = 1 - specificity, y = sensitivity, color = Model)) +
  geom_line(linewidth = 1.5) +
  geom_abline(linetype = "dashed") +
  labs( 
       x = "Specifity (FPR)", 
       y = "Sensitivity (TPR)") +
  theme_minimal(base_size = 20)

# AUC
auc_lasso <- roc_auc(test_results, truth = Label, LASSO_prob) %>%
  mutate(Model = "LASSO")
auc_cart  <- roc_auc(test_results, truth = Label, CART_prob) %>%
  mutate(Model = "CART")
auc_rf    <- roc_auc(test_results, truth = Label, RF_prob) %>%
  mutate(Model = "Random Forest")

bind_rows(auc_lasso, auc_cart, auc_rf)
