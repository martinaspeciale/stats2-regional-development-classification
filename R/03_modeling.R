# 03_modeling.R
# Tre metodi visti a lezione: KNN, LDA e QDA.
# Il test set resta da parte fino alla valutazione finale.

library(dplyr)
library(class)   # KNN
library(MASS)    # LDA e QDA
library(caret)   # matrice di confusione e indicatori

set.seed(2026)

train <- read.csv("data/regions_train.csv", stringsAsFactors = FALSE)
test  <- read.csv("data/regions_test.csv",  stringsAsFactors = FALSE)

lvls <- c("meno_sviluppata","transizione","piu_sviluppata")
train$dev_class <- factor(train$dev_class, levels = lvls)
test$dev_class  <- factor(test$dev_class,  levels = lvls)

predictors <- c("unemp_rate","educ_tertiary","rnd_gdp_pct","hitech_emp_pct","broadband_pct")

X_train <- scale(train[, predictors])
X_test  <- scale(test[, predictors],
                 center = attr(X_train, "scaled:center"),
                 scale  = attr(X_train, "scaled:scale"))
y_train <- train$dev_class
y_test  <- test$dev_class

dir.create("output", showWarnings = FALSE)

# K di KNN scelto con cross-validation sul training set.
K_cv   <- 10
folds  <- sample(rep(1:K_cv, length.out = nrow(train)))
K_grid <- 1:25

cv_errors <- sapply(K_grid, function(k) {
  errs <- sapply(1:K_cv, function(f) {
    tr_x <- X_train[folds != f, , drop = FALSE]
    va_x <- X_train[folds == f, , drop = FALSE]
    tr_y <- y_train[folds != f]
    va_y <- y_train[folds == f]
    pred <- knn(tr_x, va_x, tr_y, k = k)
    mean(pred != va_y)
  })
  mean(errs)
})

cv_results <- data.frame(K = K_grid, cv_error = cv_errors)
write.csv(cv_results, "output/knn_cv_results.csv", row.names = FALSE)

best_K <- K_grid[which.min(cv_errors)]
cat(sprintf("KNN — best K = %d  (CV error = %.3f)\n", best_K, min(cv_errors)))

# Previsione KNN finale sul test set.
knn_pred <- knn(X_train, X_test, y_train, k = best_K)

# LDA: separazione lineare tra le classi.
lda_fit  <- lda(dev_class ~ ., data = data.frame(dev_class = y_train, X_train))
lda_pred <- predict(lda_fit, newdata = data.frame(X_test))$class

# QDA: simile a LDA, ma con frontiere più flessibili.
qda_fit  <- qda(dev_class ~ ., data = data.frame(dev_class = y_train, X_train))
qda_pred <- predict(qda_fit, newdata = data.frame(X_test))$class

# Prestazioni ricavate dalla matrice di confusione.
metrics <- function(pred, actual, name) {
  cm   <- confusionMatrix(pred, actual)
  acc  <- cm$overall["Accuracy"]
  # Media sulle tre classi: non solo accuratezza totale.
  sens <- mean(cm$byClass[,"Sensitivity"], na.rm = TRUE)
  spec <- mean(cm$byClass[,"Specificity"], na.rm = TRUE)
  list(name = name, cm = cm, accuracy = acc, sensitivity = sens, specificity = spec)
}

res_knn <- metrics(knn_pred, y_test, "KNN")
res_lda <- metrics(lda_pred, y_test, "LDA")
res_qda <- metrics(qda_pred, y_test, "QDA")

results <- list(knn = res_knn, lda = res_lda, qda = res_qda)
saveRDS(results,  "output/classification_results.rds")
saveRDS(lda_fit,  "output/lda_fit.rds")
saveRDS(qda_fit,  "output/qda_fit.rds")

comparison <- bind_rows(lapply(results, function(r)
  data.frame(Modello    = r$name,
             Accuratezza = round(r$accuracy, 3),
             Sensitivita = round(r$sensitivity, 3),
             Specificita = round(r$specificity, 3))
))
write.csv(comparison, "output/model_comparison.csv", row.names = FALSE)

cat("\n── Confronto modelli (test set) ──\n")
print(comparison)

cat("\n── Matrice di confusione KNN ──\n");  print(res_knn$cm$table)
cat("\n── Matrice di confusione LDA ──\n");  print(res_lda$cm$table)
cat("\n── Matrice di confusione QDA ──\n");  print(res_qda$cm$table)
