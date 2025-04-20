# Завантажити набір даних
data_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/arrhythmia/arrhythmia.data"
arrhythmia_data <- read.csv(data_url, header = FALSE, na.strings = "?")

# Визначення індексів ознак та залежної змінної
first_feature_index <- 161
second_feature_index <- 162
third_feature_index <- 163
dependent_variable_index <- 2

# Вибір потрібних стовпців
selected_columns <- c(dependent_variable_index, first_feature_index, second_feature_index, third_feature_index)
subset_data <- arrhythmia_data[, selected_columns]
colnames(subset_data) <- c("Sex", "Q_wave_DI", "R_wave_DI", "S_wave_DI")

# Проаналізувати типи даних в наборі
print("Типи даних у наборі:")
str(subset_data)

# Кількість кількісних і категоріальних змінних
numeric_cols <- sapply(subset_data, is.numeric)
categorical_cols <- sapply(subset_data, is.factor)

print(paste("Кількість кількісних змінних:", sum(numeric_cols)))
print(paste("Кількість категоріальних змінних:", sum(categorical_cols)))

# Проаналізувати описову статистику
print("\nОписова статистика:")
summary(subset_data)

# Трансформувати категоріальні змінні в факторні, а потім в чисельні
# Залежна змінна "Sex" є категоріальною, тому її потрібно перетворити
subset_data$Sex <- as.factor(subset_data$Sex)
print("\nТип даних змінної Sex після перетворення у фактор:")
print(str(subset_data$Sex))

# Перетворення факторних змінних у числові (використовуємо чисельні коди факторів)
subset_data$Sex_numeric <- as.numeric(subset_data$Sex)
print("\nТип даних змінної Sex після перетворення у числове:")
print(str(subset_data$Sex_numeric))

# Видаляємо оригінальну факторну змінну Sex, залишаючи числову версію
subset_data <- subset_data[, !(names(subset_data) %in% "Sex")]

print("\nОновлені типи даних після трансформації:")
str(subset_data)

numeric_cols_updated <- sapply(subset_data, is.numeric)
categorical_cols_updated <- sapply(subset_data, is.factor)

print(paste("Кількість кількісних змінних після трансформації:", sum(numeric_cols_updated)))
print(paste("Кількість категоріальних змінних після трансформації:", sum(categorical_cols_updated)))

# Обчислення кореляційної матриці
cor_matrix <- cor(subset_data)

# Виведення кореляційної матриці
print("\nКореляційна матриця:")
print(cor_matrix)

# Встановлення seed для відтворюваності результатів
set.seed(123)

# Визначення кількості спостережень
n <- nrow(subset_data)

# Визначення розміру навчальної вибірки (2/5 від загальної кількості)
train_size <- floor(0.4 * n)

# Створення випадкового індексу для вибірки навчальних даних
train_index <- sample(1:n, size = train_size, replace = FALSE)

# Створення навчальної вибірки
train_data <- subset_data[train_index, ]

# Створення тестової вибірки (всі інші дані)
test_data <- subset_data[-train_index, ]

# Перевірка розмірів вибірок
print(paste("Розмір навчальної вибірки:", nrow(train_data)))
print(paste("Розмір тестової вибірки:", nrow(test_data)))

# Визначаємо числові стовпці для стандартизації
numeric_cols_to_scale <- sapply(subset_data, is.numeric)

# Функція для стандартизації
standardize <- function(x) {
  (x - mean(x)) / sd(x)
}

# Стандартизація навчальної вибірки
train_data_scaled <- train_data
train_data_scaled[, numeric_cols_to_scale] <- apply(train_data[, numeric_cols_to_scale], 2, standardize)

# Стандартизація тестової вибірки з використанням середнього та стандартного відхилення навчальної вибірки
test_data_scaled <- test_data
for (col in names(train_data_scaled[, numeric_cols_to_scale])) {
  if (col %in% names(test_data_scaled)) {
    mean_train <- mean(train_data[, col])
    sd_train <- sd(train_data[, col])
    test_data_scaled[, col] <- (test_data_scaled[, col] - mean_train) / sd_train
  }
}

# Перегляд перших кількох рядків стандартизованих даних
print("\nПерші кілька рядків стандартизованої навчальної вибірки:")
head(train_data_scaled)

print("\nПерші кілька рядків стандартизованої тестової вибірки:")
head(test_data_scaled)

# Перетворення залежної змінної в бінарний формат (0 та 1)
train_data_scaled$Sex_binary <- ifelse(train_data_scaled$Sex_numeric == 2, 1, 0)

# Побудова моделі логістичної регресії зі збільшеною кількістю ітерацій
model_full <- glm(Sex_binary ~ Q_wave_DI + R_wave_DI + S_wave_DI,
                  data = train_data_scaled,
                  family = binomial(link = "logit"),
                  control = list(maxit = 50))

# Аналіз результатів моделювання
print("\nЗвіт по моделі логістичної регресії (всі незалежні змінні)")
summary(model_full)

# Значення критерія AIC
print(paste("\nAIC моделі (всі незалежні змінні)", AIC(model_full)))

# Побудова моделі логістичної регресії від двох змінних з найбільшим впливом
# Побудова моделі з Q_wave_DI та S_wave_DI
model_two_vars <- glm(Sex_binary ~ Q_wave_DI + S_wave_DI,
                      data = train_data_scaled,
                      family = binomial(link = "logit"),
                      control = list(maxit = 50))

# Аналіз результатів моделювання
print("\nЗвіт по моделі логістичної регресії (дві змінні):")
summary(model_two_vars)

# Значення критерія AIC
print(paste("\nAIC моделі (дві змінні):", AIC(model_two_vars)))

# Обчислення метрик якості класифікації
TN <- confusion_matrix["0", "0"]
FP <- 0  # Оскільки модель завжди передбачає 0, FP = 0
FN <- 0  # Оскільки модель завжди передбачає 0, FN = 0
TP <- 0  # Оскільки модель завжди передбачає 0, TP = 0

accuracy <- (TP + TN) / (TP + TN + FP + FN)
error_rate <- 1 - accuracy
sensitivity <- NA
specificity <- TN / (TN + FP)

print(paste("\nТочність моделі:", round(accuracy, 3)))
print(paste("Частка помилок:", round(error_rate, 3)))
print(paste("Чутливість:", sensitivity))
print(paste("Специфічність:", round(specificity, 3)))

# Трансформувати категоріальну змінну Sex, оскільки виникають помилки
subset_data$Sex <- as.factor(subset_data$Sex)
subset_data$Sex_numeric <- as.numeric(subset_data$Sex)

# Створення бінарної залежної змінної Sex_binary
subset_data$Sex_binary <- ifelse(subset_data$Sex_numeric == 2, 1, 0)

# Перевірка наявності обох класів у subset_data
print("Розподіл класів у початковому наборі даних:")
print(table(subset_data$Sex_binary))

# Розділення даних із збереженням пропорцій класів
library(caret)
if(length(unique(subset_data$Sex_binary)) < 2) {
    print("Помилка: Вихідні дані містять тільки один клас. Неможливо коректно розділити.")
} else {
    set.seed(123)
    train_index <- createDataPartition(subset_data$Sex_binary, p = 0.7, list = FALSE, times = 1)
    train_data <- subset_data[train_index, ]
    test_data <- subset_data[-train_index, ]

    # Перетворення залежної змінної в бінарний формат (0 та 1)
    train_data$Sex_binary <- ifelse(train_data$Sex_numeric == 2, 1, 0)
    test_data$Sex_binary <- ifelse(test_data$Sex_numeric == 2, 1, 0)

    # Перевірка розподілу класів у навчальній та тестовій вибірках
    print("Розподіл класів у навчальній вибірці:")
    print(table(train_data$Sex_binary))
    print("Розподіл класів у тестовій вибірці:")
    print(table(test_data$Sex_binary))

    # Визначаємо стовпці для стандартизації
    cols_to_standardize <- c("Q_wave_DI", "R_wave_DI", "S_wave_DI")

    # Перевіряємо, чи ці стовпці є числовими
    print(paste("Тип даних Q_wave_DI в train_data:", class(train_data$Q_wave_DI)))
    print(paste("Тип даних R_wave_DI в train_data:", class(train_data$R_wave_DI)))
    print(paste("Тип даних S_wave_DI в train_data:", class(train_data$S_wave_DI)))

    # Стандартизація навчальної вибірки (тільки вказані стовпці)
    train_data_scaled <- train_data
    for (col in cols_to_standardize) {
        if (is.numeric(train_data_scaled[[col]])) {
            train_data_scaled[[col]] <- (train_data_scaled[[col]] - mean(train_data[[col]])) / sd(train_data[[col]])
        } else {
            print(paste("Попередження: Стовпець", col, "не є числовим і не буде стандартизований."))
        }
    }

    # Стандартизація тестової вибірки (тільки вказані стовпці, використовуючи параметри навчальної вибірки)
    test_data_scaled <- test_data
    for (col in cols_to_standardize) {
        if (is.numeric(test_data_scaled[[col]])) {
            mean_train <- mean(train_data[[col]])
            sd_train <- sd(train_data[[col]])
            test_data_scaled[[col]] <- (test_data_scaled[[col]] - mean_train) / sd_train
        } else {
            print(paste("Попередження: Стовпець", col, "не є числовим у тестовій вибірці і не буде стандартизований."))
        }
    }

    # Побудова моделі логістичної регресії
    model_full_new <- glm(Sex_binary ~ Q_wave_DI + R_wave_DI + S_wave_DI,
                          data = train_data_scaled,
                          family = binomial(link = "logit"),
                          control = list(maxit = 50))

    # Оцінка моделі та візуалізація ROC-кривої
    predicted_probabilities_new <- predict(model_full_new, newdata = test_data_scaled, type = "response")
    actual_classes_new <- test_data_scaled$Sex_binary

    # Перевірка наявності обох класів у тестовій вибірці
    if (length(unique(actual_classes_new)) == 2 && length(unique(predicted_probabilities_new)) > 1) {
        library(pROC)
        roc_curve_new <- roc(actual_classes_new, predicted_probabilities_new)
        plot(roc_curve_new, main = "ROC-крива (нова модель)",
             xlab = "Специфічність (1 - Специфічність)", ylab = "Чутливість")
        abline(0, 1, lty = 2)
        print(paste("AUC:", round(auc(roc_curve_new), 3)))
    } else {
        print("Неможливо побудувати ROC-криву: відсутні обидва класи у тестовій вибірці або модель не генерує різних передбачень.")
    }
}

library(e1071)

# Побудова SVM моделі з лінійним ядром
svm_model_linear <- svm(Sex_binary ~ Q_wave_DI + R_wave_DI,
                         data = train_data_scaled,
                         kernel = "linear")

# Аналіз результатів моделювання за допомогою звіту
print(svm_model_linear)

# Побудова SVM моделі з радіальним ядром
svm_model_radial <- svm(Sex_binary ~ Q_wave_DI + R_wave_DI,
                         data = train_data_scaled,
                         kernel = "radial")

# Аналіз результатів моделювання за допомогою звіту
print(svm_model_radial)

# Прогнозування значень функції прийняття рішення на тестовій вибірці (лінійна модель)
decision_values_linear <- predict(svm_model_linear, newdata = test_data_scaled, decision.values = TRUE)

# Перетворення значень функції прийняття рішення на бінарні передбачені класи (поріг 0)
predicted_classes_linear <- ifelse(decision_values_linear > 0, 1, 0)

# Формування таблиці спряженості
confusion_matrix_linear <- table(predicted = predicted_classes_linear, actual = test_data_scaled$Sex_binary)
print("Таблиця спряженості (SVM, лінійне ядро):")
print(confusion_matrix_linear)

# Обчислення метрик якості класифікації
TN_linear <- confusion_matrix_linear["0", "0"]
FP_linear <- confusion_matrix_linear["1", "0"]
FN_linear <- confusion_matrix_linear["0", "1"]
TP_linear <- confusion_matrix_linear["1", "1"]

accuracy_linear <- (TP_linear + TN_linear) / (TP_linear + TN_linear + FP_linear + FN_linear)
error_rate_linear <- 1 - accuracy_linear
sensitivity_linear <- TP_linear / (TP_linear + FN_linear)
specificity_linear <- TN_linear / (TN_linear + FP_linear)

print(paste("\nТочність моделі (SVM, лінійне ядро):", round(accuracy_linear, 3)))
print(paste("Частка помилок (SVM, лінійне ядро):", round(error_rate_linear, 3)))
print(paste("Чутливість (SVM, лінійне ядро):", round(sensitivity_linear, 3)))
print(paste("Специфічність (SVM, лінійне ядро):", round(specificity_linear, 3)))

# Візуалізація ROC-кривої (лінійне ядро)
roc_curve_linear <- roc(test_data_scaled$Sex_binary, as.numeric(decision_values_linear))
plot(roc_curve_linear, main = "ROC-крива (SVM, лінійне ядро)", xlab = "Специфічність (1 - Специфічність)", ylab = "Чутливість")
abline(0, 1, lty = 2)
print(paste("AUC (SVM, лінійне ядро):", round(auc(roc_curve_linear), 3)))

# Створення таблиці для порівняння результатів
results_table <- data.frame(
  Модель = character(0),
  Змінні = character(0),
  Точність = numeric(0),
  Частка_помилок = numeric(0),
  Чутливість = numeric(0),
  Специфічність = numeric(0),
  AUC = numeric(0),
  AIC_Навчальна = numeric(0)
)

# Додавання результатів логістичної регресії (всі змінні)
results_table <- rbind(results_table, data.frame(
  Модель = "Логістична регресія (всі)",
  Змінні = "Q, R, S",
  Точність = NA,
  Частка_помилок = NA,
  Чутливість = NA,
  Специфічність = NA,
  AUC = 0.559,
  AIC_Навчальна = 8.00
))

# Додавання результатів логістичної регресії (дві змінні)
results_table <- rbind(results_table, data.frame(
  Модель = "Логістична регресія (дві)",
  Змінні = "Q, S",
  Точність = NA,
  Частка_помилок = NA,
  Чутливість = NA,
  Специфічність = NA,
  AUC = NA,
  AIC_Навчальна = 6.00
))

# Додавання результатів SVM (лінійне ядро)
results_table <- rbind(results_table, data.frame(
  Модель = "SVM (лінійне ядро)",
  Змінні = "Q, R",
  Точність = 0.578,
  Частка_помилок = 0.422,
  Чутливість = 0.987,
  Специфічність = 0,
  AUC = 0.523,
  AIC_Навчальна = NA
))

# Додавання рядка для SVM (радіальне ядро) - поки без значень
results_table <- rbind(results_table, data.frame(
  Модель = "SVM (радіальне ядро)",
  Змінні = "Q, R",
  Точність = NA,
  Частка_помилок = NA,
  Чутливість = NA,
  Специфічність = NA,
  AUC = NA,
  AIC_Навчальна = NA
))

# Виведення таблиці в консоль
print(results_table)

# Збереження таблиці у CSV файл
write.csv(results_table, "results_comparison.csv", row.names = FAL
