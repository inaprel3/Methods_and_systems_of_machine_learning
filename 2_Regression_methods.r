# Завантажуємо необхідні бібліотеки
install.packages("tidyverse")
install.packages("caTools")
install.packages("psych")
library(tidyverse)
library(caTools)
library(psych)

# Завантажуємо дані
insurance_data <- read_csv("E:/2) STUDY/Методи та системи машинного навчання/Інше/insurance.csv")

# Поділ на навчальну та тестову вибірки (70%/30%)
set.seed(123)  # Фіксуємо генератор випадкових чисел для відтворюваності
split <- sample.split(insurance_data$charges, SplitRatio = 0.7)
train_data <- subset(insurance_data, split == TRUE)
test_data <- subset(insurance_data, split == FALSE)

# Перевіряємо розміри вибірок
dim(train_data)
dim(test_data)

# Вибираємо лише числові змінні
numeric_data <- select(train_data, where(is.numeric))

# Розраховуємо кореляційну матрицю
cor_matrix <- cor(numeric_data)

# Виводимо кореляційну матрицю
print(cor_matrix)

# Побудова парних графіків з кореляціями
pairs.panels(train_data[, c("age", "bmi", "children", "charges")], 
             method = "pearson",   # Метод кореляції (Пірсона)
             hist.col = "#00AFBB", # Колір гістограм
             lm = TRUE)            # Додавання ліній регресії

# Створюємо модель лінійної регресії
simple_lm <- lm(charges ~ age, data = train_data)

# Виводимо зведену статистику моделі
summary(simple_lm)

# Витягуємо R² (коефіцієнт детермінації)
r_squared <- summary(simple_lm)$r.squared
r_squared

# Прогноз для тренувальної вибірки
train_predictions <- predict(simple_lm, newdata = train_data)
train_mse <- mean((train_data$charges - train_predictions)^2)

# Прогноз для тестової вибірки
test_predictions <- predict(simple_lm, newdata = test_data)
test_mse <- mean((test_data$charges - test_predictions)^2)

# Виводимо MSE
train_mse
test_mse

# Отримуємо передбачені значення для тестової вибірки
test_predictions <- predict(simple_lm, newdata = test_data)

# Створюємо графік
ggplot() +
  geom_point(aes(train_data$age, train_data$charges), colour = 'red', alpha = 0.5) +  # Тренувальні дані (червоні точки)
  geom_point(aes(test_data$age, test_data$charges), colour = 'dark green', alpha = 0.5) +  # Тестові дані (зелені точки)
  geom_line(aes(test_data$age, test_predictions), colour = 'blue') +  # Лінія регресії (синя)
  ggtitle('Витрати на страхування vs Вік') +
  xlab('Вік (age)') +
  ylab('Страхові витрати (charges)') +
  theme_minimal()

# Побудова моделі множинної лінійної регресії з усіма предикторами
full_model <- lm(charges ~ ., data = train_data)

# Вивід зведеної статистики моделі
summary(full_model

# Витягуємо коефіцієнт детермінації
r_squared_full <- summary(full_model)$r.squared
r_squared_full

# Прогноз для тренувальної вибірки
train_pred_full <- predict(full_model, newdata = train_data)
train_mse_full <- mean((train_data$charges - train_pred_full)^2)

# Прогноз для тестової вибірки
test_pred_full <- predict(full_model, newdata = test_data)
test_mse_full <- mean((test_data$charges - test_pred_full)^2)

# Вивід MSE
train_mse_full
test_mse_full

# Використовуємо пошук найкращої моделі (метод backward elimination)
optimized_model <- step(full_model, direction = "backward")

# Вивід зведеної статистики
summary(optimized_model)

# Коефіцієнт детермінації
r_squared_opt <- summary(optimized_model)$r.squared
r_squared_opt

# Прогноз для тренувальної вибірки
train_pred_opt <- predict(optimized_model, newdata = train_data)
train_mse_opt <- mean((train_data$charges - train_pred_opt)^2)

# Прогноз для тестової вибірки
test_pred_opt <- predict(optimized_model, newdata = test_data)
test_mse_opt <- mean((test_data$charges - test_pred_opt)^2)

# Вивід MSE
train_mse_opt
test_mse_opt

ggplot() +
  geom_point(aes(x = test_data$charges, y = test_pred_opt), color = "dark green", alpha = 0.5) + 
  geom_point(aes(x = test_data$charges, y = test_data$charges), color = "red", alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") + 
  ggtitle("Реальні vs Прогнозовані значення") +
  xlab("Реальні значення (charges)") +
  ylab("Прогнозовані значення (charges)") +
  theme_minimal()

# Побудова поліноміальної моделі 2-го ступеня для віку
poly_model_age <- lm(charges ~ poly(age, 2), data = train_data)

# Звіт моделі
summary(poly_model_age)

# Прогноз для тренувальної та тестової вибірок
train_pred_poly_age <- predict(poly_model_age, newdata = train_data)
test_pred_poly_age <- predict(poly_model_age, newdata = test_data)

# Розрахунок MSE
train_mse_poly_age <- mean((train_data$charges - train_pred_poly_age)^2)
test_mse_poly_age <- mean((test_data$charges - test_pred_poly_age)^2)

# Візуалізація результатів
ggplot() + 
  geom_point(aes(train_data$age, train_data$charges), colour = 'red', alpha = 0.5) + 
  geom_point(aes(test_data$age, test_data$charges), colour = 'dark green', alpha = 0.5) + 
  geom_line(aes(test_data$age, test_pred_poly_age), colour = 'blue') + 
  ggtitle('Поліноміальна регресія: charges ~ age') + 
  xlab('Вік') + 
  ylab('Страхові витрати') + 
  theme_minimal()

# Побудова поліноміальної моделі 2-го ступеня для age, bmi та children
poly_model_3vars <- lm(charges ~ poly(age, 2) + poly(bmi, 2) + poly(children, 2), data = train_data)

# Звіт моделі
summary(poly_model_3vars)

# Прогноз для тренувальної та тестової вибірок
train_pred_poly_3vars <- predict(poly_model_3vars, newdata = train_data)
test_pred_poly_3vars <- predict(poly_model_3vars, newdata = test_data)

# Розрахунок MSE
train_mse_poly_3vars <- mean((train_data$charges - train_pred_poly_3vars)^2)
test_mse_poly_3vars <- mean((test_data$charges - test_pred_poly_3vars)^2)

# Візуалізація результатів для віку та страхових витрат
ggplot() + 
  geom_point(aes(train_data$age, train_data$charges), colour = 'red', alpha = 0.5) + 
  geom_point(aes(test_data$age, test_data$charges), colour = 'dark green', alpha = 0.5) + 
  geom_line(aes(test_data$age, test_pred_poly_3vars), colour = 'blue') + 
  ggtitle('Поліноміальна регресія: charges ~ age + bmi + children') + 
  xlab('Вік') + 
  ylab('Страхові витрати') + 
  theme_minimal()

# Створюємо текстовий файл і записуємо дані
sink("model_comparison_results.txt")

# Виводимо заголовок
cat("Порівняльна таблиця моделей:\n\n")

# Виводимо кожну модель окремо
cat("1. Лінійна регресія (1 змінна)\n")
cat("   Змінні: age\n")
cat("   MSE:", test_mse, "\n")
cat("   R²:", r_squared, "\n\n")

cat("2. Поліноміальна регресія (1 змінна, ступінь 2)\n")
cat("   Змінні: age\n")
cat("   MSE:", test_mse_poly_age, "\n")
cat("   R²:", r_squared_full, "\n\n")

cat("3. Поліноміальна регресія (1 змінна, ступінь 3)\n")
cat("   Змінні: age\n")
cat("   MSE: немає даних\n")
cat("   R²: немає даних\n\n")

cat("4. Множинна лінійна регресія (3 змінні)\n")
cat("   Змінні: age, bmi, children\n")
cat("   MSE:", test_mse_full, "\n")
cat("   R²:", r_squared_opt, "\n\n")

cat("5. Множинна поліноміальна регресія (3 змінні, ступінь 2)\n")
cat("   Змінні: age, bmi, children\n")
cat("   MSE: немає даних\n")
cat("   R²: немає даних\n\n")

# Закриваємо запис у файл
sink()
