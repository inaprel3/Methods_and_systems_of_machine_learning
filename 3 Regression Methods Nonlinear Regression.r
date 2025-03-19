# Завантажуємо необхідні бібліотеки
library(readr)  
library(caret)  

# Завантажуємо дані
insurance_data <- read_csv("E:/2) STUDY/Методи та системи машинного навчання/Інше/insurance.csv")

# Переглядаємо перші рядки
head(insurance_data)

# Встановлюємо seed для відтворюваності
set.seed(123)

# Ділимо вибірку: 80% - навчальна, 20% - тестова
train_index <- createDataPartition(insurance_data$charges, p = 0.8, list = FALSE)
train_data <- insurance_data[train_index, ]
test_data <- insurance_data[-train_index, ]

# Перевіряємо розміри вибірок
dim(train_data)
dim(test_data)

# Завантажуємо бібліотеку для візуалізації
library(ggcorrplot)

# Обчислюємо кореляційну матрицю для числових змінних
numeric_vars <- insurance_data[, sapply(insurance_data, is.numeric)]
cor_matrix <- cor(numeric_vars)

# Візуалізуємо кореляцію
ggcorrplot(cor_matrix, lab = TRUE, method = "circle", type = "lower", colors = c("blue", "white", "red"))

# Дивимося кореляцію змінних з "charges"
correlations <- data.frame(Variable = rownames(cor_matrix), Correlation = cor_matrix[, "charges"])
correlations <- correlations[order(-abs(correlations$Correlation)), ]
print(correlations)

# Завантажуємо бібліотеку для візуалізації
library(ggplot2)
library(dplyr)

# Аналіз середніх значень charges за категоріями
insurance_data %>%
  group_by(sex) %>%
  summarise(Mean_Charges = mean(charges)) %>%
  print()

insurance_data %>%
  group_by(smoker) %>%
  summarise(Mean_Charges = mean(charges)) %>%
  print()

insurance_data %>%
  group_by(region) %>%
  summarise(Mean_Charges = mean(charges)) %>%
  print()

# Візуалізація впливу категоріальних змінних на charges
ggplot(insurance_data, aes(x = sex, y = charges, fill = sex)) +
  geom_boxplot() + ggtitle("Вплив статі на вартість страховки")

ggplot(insurance_data, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot() + ggtitle("Вплив куріння на вартість страховки")

ggplot(insurance_data, aes(x = region, y = charges, fill = region)) +
  geom_boxplot() + ggtitle("Вплив регіону на вартість страховки")

# Виконуємо дисперсійний аналіз (ANOVA)
anova_sex <- aov(charges ~ sex, data = insurance_data)
anova_smoker <- aov(charges ~ smoker, data = insurance_data)
anova_region <- aov(charges ~ region, data = insurance_data)

summary(anova_sex)
summary(anova_smoker)
summary(anova_region)

# Завантажуємо необхідні бібліотеки
library(rpart)
library(rpart.plot)
library(Metrics)

# Побудова моделі регресійного дерева на основі 'age'
model_tree <- rpart(charges ~ age, data = insurance_data, method = "anova")

# Візуалізація дерева рішень
rpart.plot(model_tree, main = "Регресійне дерево для змінної 'age'")

# Прогнозування на навчальній вибірці
train_pred <- predict(model_tree, train_data)

# Прогнозування на тестовій вибірці
test_pred <- predict(model_tree, test_data)

# Обчислення MSE для навчальної вибірки
train_mse <- mse(train_data$charges, train_pred)

# Обчислення MSE для тестової вибірки
test_mse <- mse(test_data$charges, test_pred)

# Виведення MSE для обох вибірок
cat("MSE для навчальної вибірки:", train_mse, "\n")
cat("MSE для тестової вибірки:", test_mse, "\n")

# Створюємо дані для графіка
plot_data <- data.frame(
  Real_Values = test_data$charges,
  Predicted_Values = test_pred,
  Set = "Test"
)

train_data_plot <- data.frame(
  Real_Values = train_data$charges,
  Predicted_Values = train_pred,
  Set = "Train"
)

# Об'єднуємо обидві вибірки для візуалізації
combined_data <- rbind(plot_data, train_data_plot)

# Створюємо графік
ggplot(combined_data, aes(x = Real_Values, y = Predicted_Values, color = Set)) +
  geom_point(alpha = 0.6) +  # Точки для навчальної та тестової вибірок
  scale_color_manual(values = c("red", "darkgreen")) +  # Червоний для навчальної, темно-зелений для тестової вибірки
  geom_abline(slope = 1, intercept = 0, color = "blue", size = 1.2) +  # Лінія рівності синя
  labs(title = "Реальні vs Прогнозовані значення",
       x = "Реальні значення",
       y = "Прогнозовані значення") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Побудова моделі регресійного дерева на основі всіх факторів
model_tree_all <- rpart(charges ~ ., data = insurance_data, method = "anova")

# Виведення результатів моделі
summary(model_tree_all)

# Візуалізація дерева рішень
rpart.plot(model_tree_all, main = "Регресійне дерево для всіх факторів")

# Прогнозування на навчальній вибірці
train_pred_all <- predict(model_tree_all, train_data)

# Прогнозування на тестовій вибірці
test_pred_all <- predict(model_tree_all, test_data)

# Обчислення MSE для навчальної вибірки
train_mse_all <- mse(train_data$charges, train_pred_all)

# Обчислення MSE для тестової вибірки
test_mse_all <- mse(test_data$charges, test_pred_all)

# Виведення MSE для обох вибірок
cat("MSE для навчальної вибірки:", train_mse_all, "\n")
cat("MSE для тестової вибірки:", test_mse_all, "\n")

# Створюємо дані для графіка
plot_data_all <- data.frame(
  Real_Values = test_data$charges,
  Predicted_Values = test_pred_all,
  Set = "Test"
)

train_data_plot_all <- data.frame(
  Real_Values = train_data$charges,
  Predicted_Values = train_pred_all,
  Set = "Train"
)

# Об'єднуємо обидві вибірки для візуалізації
combined_data_all <- rbind(plot_data_all, train_data_plot_all)

# Створюємо графік
ggplot(combined_data_all, aes(x = Real_Values, y = Predicted_Values, color = Set)) +
  geom_point(alpha = 0.6) +  # Точки для навчальної та тестової вибірок
  scale_color_manual(values = c("red", "darkgreen")) +  # Червоний для навчальної, темно-зелений для тестової вибірки
  geom_abline(slope = 1, intercept = 0, color = "blue", size = 1.2) +  # Лінія рівності синя
  labs(title = "Реальні vs Прогнозовані значення",
       x = "Реальні значення",
       y = "Прогнозовані значення") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Завантаження необхідних бібліотек
library(randomForest)

# Побудова моделі випадкового лісу на основі змінної "smoker"
model_rf_smoker <- randomForest(charges ~ smoker, data = insurance_data)

# Виведення результатів моделі
print(model_rf_smoker)

# Важливість змінних
importance(model_rf_smoker)

# Оцінка якості моделі (MSE для навчальної вибірки)
train_pred_rf_smoker <- predict(model_rf_smoker, train_data)
train_mse_rf_smoker <- mean((train_data$charges - train_pred_rf_smoker)^2)
cat("MSE для навчальної вибірки (Random Forest - smoker):", train_mse_rf_smoker, "\n")

# Оцінка якості моделі (MSE для тестової вибірки)
test_pred_rf_smoker <- predict(model_rf_smoker, test_data)
test_mse_rf_smoker <- mean((test_data$charges - test_pred_rf_smoker)^2)
cat("MSE для тестової вибірки (Random Forest - smoker):", test_mse_rf_smoker, "\n")

# Побудова поліноміальної моделі лінійної регресії від 3 змінних
model_poly <- lm(charges ~ poly(age, 2) + poly(bmi, 2) + poly(children, 2), data = insurance_data)

# Виведення результатів моделі
summary(model_poly)

# Оцінка якості моделі (MSE для навчальної вибірки)
train_pred_poly <- predict(model_poly, train_data)
train_mse_poly <- mean((train_data$charges - train_pred_poly)^2)
cat("MSE для навчальної вибірки (Polynomial Regression):", train_mse_poly, "\n")

# Оцінка якості моделі (MSE для тестової вибірки)
test_pred_poly <- predict(model_poly, test_data)
test_mse_poly <- mean((test_data$charges - test_pred_poly)^2)
cat("MSE для тестової вибірки (Polynomial Regression):", test_mse_poly, "\n")

# Збереження критеріїв якості моделі випадкового лісу для змінної "smoker"
cat("MSE для навчальної вибірки (Random Forest - smoker):", train_mse_rf_smoker, "\n")
cat("MSE для тестової вибірки (Random Forest - smoker):", test_mse_rf_smoker, "\n")

# Збереження критеріїв якості поліноміальної регресії
cat("MSE для навчальної вибірки (Polynomial Regression):", train_mse_poly, "\n")
cat("MSE для тестової вибірки (Polynomial Regression):", test_mse_poly, "\n")

# Створюємо дані для графіка випадкового лісу
plot_data_rf_smoker <- data.frame(
  Real_Values = test_data$charges,
  Predicted_Values = test_pred_rf_smoker,
  Set = "Test"
)

train_data_rf_smoker_plot <- data.frame(
  Real_Values = train_data$charges,
  Predicted_Values = train_pred_rf_smoker,
  Set = "Train"
)

# Об'єднуємо обидві вибірки для візуалізації
combined_data_rf_smoker <- rbind(plot_data_rf_smoker, train_data_rf_smoker_plot)

# Створюємо графік для випадкового лісу
ggplot(combined_data_rf_smoker, aes(x = Real_Values, y = Predicted_Values, color = Set)) +
  geom_point(alpha = 0.6) +  # Точки для навчальної та тестової вибірок
  scale_color_manual(values = c("red", "darkgreen")) +  # Червоний для навчальної, темно-зелений для тестової вибірки
  geom_abline(slope = 1, intercept = 0, color = "blue", size = 1.2) +  # Лінія рівності синя
  labs(title = "Реальні vs Прогнозовані значення (Random Forest - smoker)",
       x = "Реальні значення",
       y = "Прогнозовані значення") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Створюємо дані для графіка поліноміальної регресії
plot_data_poly <- data.frame(
  Real_Values = test_data$charges,
  Predicted_Values = test_pred_poly,
  Set = "Test"
)

train_data_poly_plot <- data.frame(
  Real_Values = train_data$charges,
  Predicted_Values = train_pred_poly,
  Set = "Train"
)

# Об'єднуємо обидві вибірки для візуалізації
combined_data_poly <- rbind(plot_data_poly, train_data_poly_plot)

# Створюємо графік для поліноміальної регресії
ggplot(combined_data_poly, aes(x = Real_Values, y = Predicted_Values, color = Set)) +
  geom_point(alpha = 0.6) +  # Точки для навчальної та тестової вибірок
  scale_color_manual(values = c("red", "darkgreen")) +  # Червоний для навчальної, темно-зелений для тестової вибірки
  geom_abline(slope = 1, intercept = 0, color = "blue", size = 1.2) +  # Лінія рівності синя
  labs(title = "Реальні vs Прогнозовані значення (Polynomial Regression)",
       x = "Реальні значення",
       y = "Прогнозовані значення") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Створюємо таблицю з результатами моделювання
results <- data.frame(
  Model = c("Regression Tree (age)", "Regression Tree (all variables)", "Random Forest (smoker)", "Polynomial Regression"),
  Train_MSE = c(train_mse, train_mse_all, train_mse_rf_smoker, train_mse_poly),
  Test_MSE = c(test_mse, test_mse_all, test_mse_rf_smoker, test_mse_poly)
)

# Виводимо таблицю результатів
print(results)

# Збереження таблиці результатів у файл
write.csv(results, "model_comparison_results.csv", row.names = FALSE)

# Прогнозування для кожної моделі
test_pred_tree_age <- predict(model_tree, test_data)
test_pred_tree_all <- predict(model_tree_all, test_data)
test_pred_rf_smoker <- predict(model_rf_smoker, test_data)
test_pred_poly <- predict(model_poly, test_data)

# Створюємо дані для графіка порівняння
plot_data_comparison <- data.frame(
  Real_Values = test_data$charges,
  Tree_Age = test_pred_tree_age,
  Tree_All = test_pred_tree_all,
  RF_Smoker = test_pred_rf_smoker,
  Poly = test_pred_poly
)

# Створюємо графік для порівняння
library(ggplot2)
plot_data_comparison_long <- reshape2::melt(plot_data_comparison, id.vars = "Real_Values")

ggplot(plot_data_comparison_long, aes(x = Real_Values, y = value, color = variable)) +
  geom_point(alpha = 0.6) +  # Точки для кожної моделі
  geom_abline(slope = 1, intercept = 0, color = "blue", size = 1.2) +  # Лінія рівності синя
  labs(title = "Порівняння реальних та прогнозованих значень для моделей",
       x = "Реальні значення",
       y = "Прогнозовані значення") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("red", "green", "purple", "orange", "blue"))  # Різні кольори для кожної моделі

# Створення таблиці з метриками
results <- data.frame(
  Model = c("Regression Tree (age)", "Regression Tree (all variables)", 
            "Random Forest (smoker)", "Polynomial Regression"),
  Test_MSE = c(120862425, 29074630, 63532077, 113869493)
)

# Побудова гістограми
ggplot(results, aes(x = Model, y = Test_MSE, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Порівняння MSE моделей на тестовій вибірці",
       x = "Модель",
       y = "Mean Squared Error (MSE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
