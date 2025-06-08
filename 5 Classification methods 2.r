# Завантажити набір даних
data_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/arrhythmia/arrhythmia.data"
arrhythmia_data <- read.csv(data_url, header = FALSE, na.strings = "?")

# Видалити рядки з пропущеними значеннями
arrhythmia_data <- na.omit(arrhythmia_data)

# Розділити дані на ознаки (X) та мітки класів (y)
X <- arrhythmia_data[, -ncol(arrhythmia_data)]  # усі стовпці, крім останнього
y <- arrhythmia_data[, ncol(arrhythmia_data)]   # останній стовпець — це класи

# Об'єднати назад у один фрейм даних, щоб зручно розбивати
arrhythmia_data_clean <- data.frame(X, Class = y)

# Розбити на тренувальну і тестову вибірки (наприклад, 70% на тренування, 30% на тест)
set.seed(123)  # для відтворюваності результату
train_indices <- sample(1:nrow(arrhythmia_data_clean), 0.7 * nrow(arrhythmia_data_clean))
train_data <- arrhythmia_data_clean[train_indices, ]
test_data <- arrhythmia_data_clean[-train_indices, ]

# Перевіримо розміри
cat("Тренувальна вибірка:", nrow(train_data), "рядків\n")
cat("Тестова вибірка:", nrow(test_data), "рядків\n")

# Бінаризуємо цільову змінну: 0 - нормальний клас (1), 1 - патологія (2-16)
train_data$ClassBinary <- ifelse(train_data$Class == 1, 0, 1)

# Побудуємо логістичну регресію, щоб оцінити важливість змінних
train_for_model <- subset(train_data, select = -Class)  # виключаємо старий клас
log_model <- glm(ClassBinary ~ ., data = train_for_model, family = binomial)

# Отримаємо значення коефіцієнтів (без перехоплення) та знайдемо топ-2 найвпливовіших змінні
coefs <- summary(log_model)$coefficients[-1, ]  # -1, щоб прибрати Intercept
top2_vars <- names(sort(abs(coefs[, "Estimate"]), decreasing = TRUE))[1:2]
print(paste("Найвпливовіші змінні:", paste(top2_vars, collapse = ", ")))

# Підготовка даних для K-NN (залишаємо лише V50, V39 і ClassBinary)
library(class)
library(caret)

# Тестова вибірка: бінаризуємо так само
test_data$ClassBinary <- ifelse(test_data$Class == 1, 0, 1)

# Витягуємо тільки необхідні змінні
train_knn <- train_data[, c("V50", "V39", "ClassBinary")]
test_knn <- test_data[, c("V50", "V39", "ClassBinary")]

# Нормалізація (KNN чутливий до масштабу)
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
train_knn_norm <- as.data.frame(lapply(train_knn[, 1:2], normalize))
test_knn_norm <- as.data.frame(lapply(test_knn[, 1:2], normalize))

# Покращена функція нормалізації (щоб уникнути ділення на 0)
normalize_safe <- function(x) {
  rng <- max(x) - min(x)
  if (rng == 0) return(rep(0, length(x)))
  return((x - min(x)) / rng)
}

# Застосуємо безпечну нормалізацію
train_knn_norm <- as.data.frame(lapply(train_knn[, 1:2], normalize_safe))
test_knn_norm <- as.data.frame(lapply(test_knn[, 1:2], normalize_safe))

# Додаємо цільову змінну назад
train_knn_norm$ClassBinary <- train_knn$ClassBinary
test_knn_norm$ClassBinary <- test_knn$ClassBinary

# Побудова моделі KNN
set.seed(123)
knn_pred <- knn(train = train_knn_norm[, 1:2],
                test = test_knn_norm[, 1:2],
                cl = train_knn_norm$ClassBinary,
                k = 3)

# Оцінка якості класифікації
conf_matrix <- confusionMatrix(knn_pred, factor(test_knn_norm$ClassBinary))
print(conf_matrix)

# Встановлення та завантаження бібліотеки
install.packages("e1071")
library(e1071)

# Підготовка даних для Naive Bayes моделі (залишаємо лише V50, V39 та ClassBinary)
train_nb <- train_data[, c("V50", "V39", "ClassBinary")]
test_nb <- test_data[, c("V50", "V39", "ClassBinary")]

# Нормалізація (Naive Bayes менш чутливий до масштабу, але краще зробити)
train_nb_norm <- as.data.frame(lapply(train_nb[, 1:2], normalize_safe))
test_nb_norm <- as.data.frame(lapply(test_nb[, 1:2], normalize_safe))

# Додаємо цільову змінну назад
train_nb_norm$ClassBinary <- train_nb$ClassBinary
test_nb_norm$ClassBinary <- test_nb$ClassBinary

# Побудова Naive Bayes моделі
nb_model <- naiveBayes(ClassBinary ~ V50 + V39, data = train_nb_norm)

# Прогнозування на тестовій вибірці
nb_pred <- predict(nb_model, test_nb_norm[, c("V50", "V39")])

# Оцінка якості класифікації
conf_matrix_nb <- confusionMatrix(nb_pred, factor(test_nb_norm$ClassBinary))
print(conf_matrix_nb)

# Підготовка даних для Classification Tree моделі (залишаємо лише V50, V39 та ClassBinary)
library(rpart)

train_tree <- train_data[, c("V50", "V39", "ClassBinary")]
test_tree <- test_data[, c("V50", "V39", "ClassBinary")]

# Нормалізація (хоча дерева рішень менш чутливі до масштабу, можна провести нормалізацію)
train_tree_norm <- as.data.frame(lapply(train_tree[, 1:2], normalize_safe))
test_tree_norm <- as.data.frame(lapply(test_tree[, 1:2], normalize_safe))

# Додаємо цільову змінну назад
train_tree_norm$ClassBinary <- train_tree$ClassBinary
test_tree_norm$ClassBinary <- test_tree$ClassBinary

# Побудова дерева рішень (Classification Tree)
tree_model <- rpart(ClassBinary ~ V50 + V39, data = train_tree_norm, method = "class")

# Прогнозування на тестовій вибірці
tree_pred <- predict(tree_model, test_tree_norm[, c("V50", "V39")], type = "class")

# Оцінка якості класифікації
conf_matrix_tree <- confusionMatrix(tree_pred, factor(test_tree_norm$ClassBinary))
print(conf_matrix_tree)

# Завантаження необхідних бібліотек
library(randomForest)

# Підготовка даних для Random Forest (залишаємо лише найвпливовіші змінні V50, V39 та цільову змінну ClassBinary)
train_rf <- train_data[, c("V50", "V39", "ClassBinary")]
test_rf <- test_data[, c("V50", "V39", "ClassBinary")]

# Нормалізація ознак (щоб уникнути впливу масштабу, хоча Random Forest менш чутливий до цього)
train_rf_norm <- as.data.frame(lapply(train_rf[, 1:2], normalize_safe))
test_rf_norm <- as.data.frame(lapply(test_rf[, 1:2], normalize_safe))

# Додаємо цільову змінну назад до нормалізованого набору даних
train_rf_norm$ClassBinary <- train_rf$ClassBinary
test_rf_norm$ClassBinary <- test_rf$ClassBinary

# Перетворюємо цільову змінну на фактор для класифікаційної моделі
train_rf_norm$ClassBinary <- factor(train_rf_norm$ClassBinary)
test_rf_norm$ClassBinary <- factor(test_rf_norm$ClassBinary)

# Побудова моделі Random Forest на тренувальній вибірці
rf_model <- randomForest(ClassBinary ~ V50 + V39, data = train_rf_norm)

# Прогнозування на тестовій вибірці
rf_pred <- predict(rf_model, test_rf_norm[, c("V50", "V39")])

# Обчислення матриці сплутаності для оцінки точності моделі
conf_matrix_rf <- confusionMatrix(rf_pred, factor(test_rf_norm$ClassBinary))
print(conf_matrix_rf)

# Завантаження необхідних бібліотек
install.packages("xgboost")
library(xgboost)

# Підготовка даних (використовуємо вже нормалізовані дані)
# Створення матриць ознак для тренувальної та тестової вибірок
train_matrix <- as.matrix(train_rf_norm[, c("V50", "V39")])
test_matrix <- as.matrix(test_rf_norm[, c("V50", "V39")])

# Цільові змінні у форматі чисел
train_label <- as.numeric(as.character(train_rf_norm$ClassBinary))
test_label <- as.numeric(as.character(test_rf_norm$ClassBinary))

# Побудова моделі XGBoost
xgb_model <- xgboost(data = train_matrix,
                     label = train_label,
                     nrounds = 50,
                     objective = "binary:logistic",
                     verbose = 0)

# Прогнозування (отримуємо ймовірності)
xgb_prob <- predict(xgb_model, test_matrix)

# Перетворення ймовірностей у класи (поріг 0.5)
xgb_pred <- ifelse(xgb_prob > 0.5, 1, 0)

# Оцінка якості класифікації
conf_matrix_xgb <- confusionMatrix(factor(xgb_pred), factor(test_label))
print(conf_matrix_xgb)

# Обчислимо основні метрики вручну
conf <- table(Predicted = knn_pred, Actual = factor(test_knn_norm$ClassBinary))

# Значення
TP <- conf["1", "1"]
TN <- conf["0", "0"]
FP <- conf["1", "0"]
FN <- conf["0", "1"]

# Метрики
accuracy <- (TP + TN) / sum(conf)
error_rate <- 1 - accuracy
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)

cat("Accuracy:", round(accuracy, 4), "\n")
cat("Error rate:", round(error_rate, 4), "\n")
cat("Sensitivity (Recall):", round(sensitivity, 4), "\n")
cat("Specificity:", round(specificity, 4), "\n")

# Таблиця спряженості
conf <- table(Predicted = knn_pred, Actual = factor(test_knn_norm$ClassBinary))
print(conf)

library(pROC)

# Навчання моделі KNN з ймовірностями
knn_model <- knn3(ClassBinary ~ ., data = train_knn_norm, k = 5)

# Отримання ймовірностей для тестових даних
knn_probs <- predict(knn_model, test_knn_norm, type = "prob")

# Побудова ROC-кривої
roc_knn <- roc(response = test_knn_norm$ClassBinary, predictor = knn_probs[,2])

# Побудова графіка
plot(roc_knn, col = "blue", main = "ROC-крива для K-NN")
auc(roc_knn)  # Обчислення площі під кривою

# Підключення бібліотеки для Naive Bayes
library(e1071)

# Навчання моделі Naive Bayes
nb_model <- naiveBayes(ClassBinary ~ ., data = train_knn_norm)

# Передбачення для тестових даних
nb_pred <- predict(nb_model, newdata = test_knn_norm)

# Таблиця спряженості
conf_nb <- table(Predicted = nb_pred, Actual = factor(test_knn_norm$ClassBinary))
print(conf_nb)

# Значення
TP_nb <- conf_nb["1", "1"]
TN_nb <- conf_nb["0", "0"]
FP_nb <- conf_nb["1", "0"]
FN_nb <- conf_nb["0", "1"]

# Метрики
accuracy_nb <- (TP_nb + TN_nb) / sum(conf_nb)
error_rate_nb <- 1 - accuracy_nb
sensitivity_nb <- TP_nb / (TP_nb + FN_nb)
specificity_nb <- TN_nb / (TN_nb + FP_nb)

cat("Accuracy (Naive Bayes):", round(accuracy_nb, 4), "\n")
cat("Error rate (Naive Bayes):", round(error_rate_nb, 4), "\n")
cat("Sensitivity (Recall) (Naive Bayes):", round(sensitivity_nb, 4), "\n")
cat("Specificity (Naive Bayes):", round(specificity_nb, 4), "\n")

# Таблиця спряженості для Naive Bayes
conf_nb <- table(Predicted = nb_pred, Actual = factor(test_knn_norm$ClassBinary))
print(conf_nb)

# Отримання ймовірностей для тестових даних
nb_probs <- predict(nb_model, newdata = test_knn_norm, type = "raw")

# Побудова ROC-кривої
roc_nb <- roc(response = test_knn_norm$ClassBinary, predictor = nb_probs[,2])

# Побудова графіка
plot(roc_nb, col = "red", main = "ROC-крива для Naive Bayes")
auc(roc_nb)  # Обчислення площі під кривою

# Навчання моделі Classification Tree
tree_model <- rpart(ClassBinary ~ ., data = train_knn_norm, method = "class")

# Передбачення для тестових даних
tree_pred <- predict(tree_model, newdata = test_knn_norm, type = "class")

# Таблиця спряженості
conf_tree <- table(Predicted = tree_pred, Actual = factor(test_knn_norm$ClassBinary))
print(conf_tree)

# Значення
TP_tree <- conf_tree["1", "1"]
TN_tree <- conf_tree["0", "0"]
FP_tree <- conf_tree["1", "0"]
FN_tree <- conf_tree["0", "1"]

# Метрики
accuracy_tree <- (TP_tree + TN_tree) / sum(conf_tree)
error_rate_tree <- 1 - accuracy_tree
sensitivity_tree <- TP_tree / (TP_tree + FN_tree)
specificity_tree <- TN_tree / (TN_tree + FP_tree)

cat("Accuracy (Classification Tree):", round(accuracy_tree, 4), "\n")
cat("Error rate (Classification Tree):", round(error_rate_tree, 4), "\n")
cat("Sensitivity (Recall) (Classification Tree):", round(sensitivity_tree, 4), "\n")
cat("Specificity (Classification Tree):", round(specificity_tree, 4), "\n")

# Отримання ймовірностей для тестових даних
tree_probs <- predict(tree_model, newdata = test_knn_norm, type = "prob")

# Побудова ROC-кривої
roc_tree <- roc(response = test_knn_norm$ClassBinary, predictor = tree_probs[,2])

# Побудова графіка
plot(roc_tree, col = "green", main = "ROC-крива для Classification Tree")
auc(roc_tree)  # Обчислення площі під кривою

# Навчання моделі Random Forest
rf_model <- randomForest(ClassBinary ~ ., data = train_knn_norm)

# Передбачення категорій (0 або 1)
rf_pred <- predict(rf_model, newdata = test_knn_norm, type = "response")

# Таблиця спряженості
conf_rf <- table(Predicted = rf_pred, Actual = factor(test_knn_norm$ClassBinary))
print(conf_rf)

# Обчислення метрик
TP_rf <- conf_rf["1", "1"]
TN_rf <- conf_rf["0", "0"]
FP_rf <- conf_rf["1", "0"]
FN_rf <- conf_rf["0", "1"]

accuracy_rf <- (TP_rf + TN_rf) / sum(conf_rf)
sensitivity_rf <- TP_rf / (TP_rf + FN_rf)
specificity_rf <- TN_rf / (TN_rf + FP_rf)
precision_rf <- TP_rf / (TP_rf + FP_rf)
F1_rf <- 2 * (precision_rf * sensitivity_rf) / (precision_rf + sensitivity_rf)

# Виведення результатів
cat("Accuracy: ", accuracy_rf, "\n")
cat("Sensitivity: ", sensitivity_rf, "\n")
cat("Specificity: ", specificity_rf, "\n")
cat("Precision: ", precision_rf, "\n")
cat("F1 Score: ", F1_rf, "\n")

# Отримуємо ймовірності для класу "1" (щоб побудувати ROC)
rf_probs <- predict(rf_model, newdata = test_knn_norm, type = "prob")

# Створення ROC кривої для класу "1"
roc_rf <- roc(test_knn_norm$ClassBinary, rf_probs[,2])  # rf_probs[,2] - ймовірність для класу "1"

# Побудова ROC кривої
plot(roc_rf, main = "ROC Curve for Random Forest", col = "orange", lwd = 2)

# Додавання AUC (площі під кривою)
cat("AUC: ", auc(roc_rf), "\n")

# Переведемо мітки в значення 0 та 1, якщо вони ще не у цьому форматі
train_labels <- as.numeric(train_labels)  # Перетворення на числові значення
train_labels <- ifelse(train_labels == -1, 0, train_labels)  # Перетворення на 0, якщо значення -1

# Підготовка матриці для XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(train_knn_norm[, -ncol(train_knn_norm)]), label = train_labels)

# Навчання моделі XGBoost
xgb_model <- xgboost(data = train_matrix, label = train_labels, nrounds = 100, objective = "binary:logistic")

# Передбачення для тестових даних
test_labels <- as.numeric(test_knn_norm$ClassBinary)  # Переведемо мітки тестових даних
test_labels <- ifelse(test_labels == -1, 0, test_labels)  # Приведемо до 0-1

test_matrix <- xgb.DMatrix(data = as.matrix(test_knn_norm[, -ncol(test_knn_norm)]))

xgb_pred <- predict(xgb_model, test_matrix)

# Визначимо класи, використовуючи поріг 0.5
xgb_pred_class <- ifelse(xgb_pred > 0.5, 1, 0)

# Таблиця спряженості
conf_xgb <- table(Predicted = factor(xgb_pred_class, levels = c(0, 1)), Actual = factor(test_labels, levels = c(0, 1)))
print(conf_xgb)

# Значення
TP_xgb <- conf_xgb["1", "1"]
TN_xgb <- conf_xgb["0", "0"]
FP_xgb <- conf_xgb["1", "0"]
FN_xgb <- conf_xgb["0", "1"]

# Метрики
accuracy_xgb <- (TP_xgb + TN_xgb) / sum(conf_xgb)
error_rate_xgb <- 1 - accuracy_xgb
sensitivity_xgb <- TP_xgb / (TP_xgb + FN_xgb)
specificity_xgb <- TN_xgb / (TN_xgb + FP_xgb)

cat("Accuracy (XGBoost):", round(accuracy_xgb, 4), "\n")
cat("Error rate (XGBoost):", round(error_rate_xgb, 4), "\n")
cat("Sensitivity (Recall) (XGBoost):", round(sensitivity_xgb, 4), "\n")
cat("Specificity (XGBoost):", round(specificity_xgb, 4), "\n")

# Побудова ROC-кривої
library(pROC)
roc_xgb <- roc(response = test_labels, predictor = xgb_pred)

# Побудова графіка
plot(roc_xgb, col = "purple", main = "ROC-крива для XGBoost")
auc(roc_xgb)  # Обчислення площі під кривою

# Створення таблиці результатів
results <- data.frame(
  Model = c("K-NN", "Naive Bayes", "Classification Tree", "Random Forest", "XGBoost"),
  Accuracy = c(0.5714, 0.4286, 0.5714, 0.5714, 0.0000),
  Error_Rate = c(0.4286, 0.5714, 0.4286, 0.4286, 1.0000),
  Sensitivity = c(1.0000, 0.0000, 1.0000, 1.0000, 0.0000),
  Specificity = c(0.0000, 1.0000, 0.0000, 0.0000, 0.0000),
  AUC = c(0.5, 0.5, 0.5, 0.5, 0.5)
)

# Виведення таблиці результатів
print(results)

# Збереження таблиці в CSV файл
write.csv(results, "model_comparison_results.csv", row.names = FALSE)
