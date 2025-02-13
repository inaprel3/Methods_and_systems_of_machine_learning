# Встановлення пакету для роботи з CSV файлами
install.packages("readr")

# Завантаження бібліотеки readr для роботи з CSV файлами
library(readr)

# Читання даних з CSV файлу
insurance_data <- read_csv("E:/2) STUDY/Методи та системи машинного навчання/Інше/insurance.csv")

# Перегляд перших 6 рядків даних для загального огляду
head(insurance_data)

# Відкриття таблиці для редагування вручну (якщо потрібно)
fix(insurance_data)

# Загальний опис статистичних характеристик даних
summary(insurance_data)

# Перевірка розмірів таблиці (кількість рядків і стовпців)
dim(insurance_data)

# Перевірка наявності пропущених значень у кожному стовпці
colSums(is.na(insurance_data))

# Визначення діапазону значень для кожної змінної
range(insurance_data$age)       # Діапазон віку
range(insurance_data$bmi)       # Діапазон BMI
range(insurance_data$children)  # Діапазон кількості дітей
range(insurance_data$charges)   # Діапазон медичних витрат

# Обчислення середнього значення та стандартного відхилення для віку
mean(insurance_data$age)
sd(insurance_data$age)

# Обчислення середнього значення та стандартного відхилення для BMI
mean(insurance_data$bmi)
sd(insurance_data$bmi)

# Обчислення середнього значення та стандартного відхилення для кількості дітей
mean(insurance_data$children)
sd(insurance_data$children)

# Обчислення середнього значення та стандартного відхилення для медичних витрат
mean(insurance_data$charges)
sd(insurance_data$charges)

# Підрахунок кількості значень для кожної категорії статі
table(insurance_data$sex)

# Підрахунок кількості значень для кожної категорії курців
table(insurance_data$smoker)

# Підрахунок кількості значень для кожної категорії регіону
table(insurance_data$region)

# Функція для знаходження моди (найбільш поширеного значення)
Mode <- function(x) {
  ux <- unique(x)  # Отримання унікальних значень
  ux[which.max(tabulate(match(x, ux)))]  # Повертає найбільш часто зустрічається значення
}

# Застосування функції Mode для статі
Mode(insurance_data$sex)

# Застосування функції Mode для курців
Mode(insurance_data$smoker)

# Застосування функції Mode для регіону
Mode(insurance_data$region)

# Побудова матриці діаграм розсіювання для кількісних змінних
pairs(insurance_data[, c("age", "bmi", "children", "charges")], 
      main = "Матриця діаграм розсіювання")

# Налаштування області малюнка для 4 графіків
par(mfrow = c(2, 2))

# Побудова гістограми для віку
hist(insurance_data$age, main = "Гістограма для віку", xlab = "Вік", col = "lightblue", breaks = 20)

# Побудова гістограми для BMI
hist(insurance_data$bmi, main = "Гістограма для BMI", xlab = "Індекс маси тіла", col = "lightgreen", breaks = 15)

# Побудова гістограми для кількості дітей
hist(insurance_data$children, main = "Гістограма для кількості дітей", xlab = "Кількість дітей", col = "lightcoral", breaks = 6)

# Побудова гістограми для медичних витрат
hist(insurance_data$charges, main = "Гістограма для медичних витрат", xlab = "Медичні витрати (USD)", col = "lightyellow", breaks = 20)
