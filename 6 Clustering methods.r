# Завантажити набір даних з вказаної URL-адреси
data_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/arrhythmia/arrhythmia.data"
arrhythmia_data <- read.csv(data_url, header = FALSE, na.strings = "?")  # замінюємо ? на NA

# Вибір потрібних стовпців: Sex (2), Q wave (161), R wave (162), S wave (163)
df <- arrhythmia_data[, c(2, 161, 162, 163)]
colnames(df) <- c("Sex", "Q", "R", "S")  # перейменовуємо стовпці для зручності

# Видалення всіх рядків із пропущеними значеннями
df_clean <- na.omit(df)

# Видаляємо мітку класу (Sex), залишаємо тільки ознаки для кластеризації
df_cluster <- df_clean[, c("Q", "R", "S")]

# (За потреби) встановлюємо пакет для перевірки кластеризаційної тенденції
install.packages("clustertend")  # пакет застарілий — краще використовувати "hopkins"
library(clustertend)

# Обчислюємо статистику Хопкінса, щоб оцінити наявність кластерної структури
set.seed(123)
hopkins(df_cluster, n = nrow(df_cluster) - 1)  # значення H < 0.5 — хороша тенденція до кластеризації

# Масштабуємо дані (стандартизація) перед обчисленням відстаней
df_scaled <- scale(df_cluster)

# Розрахунок матриці відстаней за евклідовою метрикою
dist_matrix <- dist(df_scaled)

# Побудова ієрархічної кластеризації з методом середньої відстані (average linkage)
hc <- hclust(dist_matrix, method = "average")

# Візуалізація дендрограми, щоб побачити, як об'єднуються об'єкти у кластери
plot(hc, main = "Дендрограма ієрархічної кластеризації", xlab = "", sub = "", cex = 0.6)

# Виділення певної кількості кластерів, наприклад, 2
clusters_hc <- cutree(hc, k = 2)  # присвоює кожному об'єкту номер кластера

# Визначення оптимальної кількості кластерів (правило ліктя)
wss <- numeric(10)
for (k in 1:10) {
  set.seed(123)
  wss[k] <- sum(kmeans(df_scaled, centers = k, nstart = 10)$withinss)
}

plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Кількість кластерів K", ylab = "WCSS",
     main = "Правило ліктя")

# K-means з оптимальною кількістю кластерів, напр. 2
set.seed(123)
kmeans_model <- kmeans(df_scaled, centers = 2, nstart = 10)
clusters_kmeans <- kmeans_model$cluster

# Таблиця перехрещення кластерів
table(clusters_hc, clusters_kmeans)

# Таблиця перехрещення кластерів, отриманих двома методами
table(clusters_hc, clusters_kmeans)

# Встановлення пакета factoextra
install.packages("factoextra")

# Підключення необхідних бібліотек для візуалізації
library(factoextra)
library(ggplot2)
library(ggrepel)  # для відображення підписів без накладань

set.seed(123)  # фіксація генератора випадкових чисел для відтворюваності

# Обираємо 10% випадкових індексів для підпису точок (щоб уникнути нагромадження підписів)
n_labels <- round(0.1 * nrow(df_scaled))  
label_indices <- sample(1:nrow(df_scaled), n_labels)

# Отримуємо дані для візуалізації кластерів методом k-середніх
cluster_data <- fviz_cluster(list(data = df_scaled, cluster = clusters_kmeans),
                            geom = "point",
                            stand = FALSE,
                            show.clust.cent = TRUE,
                            main = "Кластери методом k-середніх",
                            palette = "jco",
                            ggtheme = theme_minimal())$data

# Додаємо колонку з номерами рядків для підписів
cluster_data$label <- as.character(1:nrow(cluster_data))  

# Побудова базового графіка кластерів методом k-середніх
p <- fviz_cluster(list(data = df_scaled, cluster = clusters_kmeans),
                  geom = "point",
                  stand = FALSE,
                  show.clust.cent = TRUE,
                  main = "Кластери методом k-середніх",
                  palette = "jco",
                  ggtheme = theme_minimal())

# Додаємо вибіркові підписи точок (чорного кольору, розмір 4), уникаючи накладань
p + geom_text_repel(data = cluster_data[label_indices, ],
                    aes(x = x, y = y, label = label),
                    color = "black",
                    size = 4,
                    max.overlaps = 10)

# Аналогічно для ієрархічної кластеризації
cluster_data_hc <- fviz_cluster(list(data = df_scaled, cluster = clusters_hc),
                               geom = "point",
                               stand = FALSE,
                               show.clust.cent = TRUE,
                               main = "Кластери методом ієрархічної кластеризації",
                               palette = "jco",
                               ggtheme = theme_minimal())$data

# Додаємо мітки (номера рядків)
cluster_data_hc$label <- as.character(1:nrow(cluster_data_hc))

# Побудова базового графіка ієрархічної кластеризації
p_hc <- fviz_cluster(list(data = df_scaled, cluster = clusters_hc),
                     geom = "point",
                     stand = FALSE,
                     show.clust.cent = TRUE,
                     main = "Кластери методом ієрархічної кластеризації",
                     palette = "jco",
                     ggtheme = theme_minimal())

# Додаємо вибіркові підписи для кластерів ієрархічної кластеризації
p_hc + geom_text_repel(data = cluster_data_hc[label_indices, ],
                       aes(x = x, y = y, label = label),
                       color = "black",
                       size = 4,
                       max.overlaps = 10)

library(cluster)

# Силует для k-means
sil_kmeans <- silhouette(clusters_kmeans, dist_matrix)
fviz_silhouette(sil_kmeans)

# Силует для ієрархічної кластеризації
sil_hc <- silhouette(clusters_hc, dist_matrix)
fviz_silhouette(sil_hc)

# Ініціалізація вектора для збереження середньої ширини силуету для кожного значення K
sil_width <- numeric(10)

# Перебір кількостей кластерів від 2 до 10
for (k in 2:10) {
  # Побудова моделі k-середніх з K кластерами
  km <- kmeans(df_scaled, centers = k, nstart = 10)
  
  # Обчислення силуетних коефіцієнтів для кожної точки
  ss <- silhouette(km$cluster, dist_matrix)
  
  # Збереження середньої ширини силуету для поточного K
  sil_width[k] <- mean(ss[, 3])
}

# Побудова графіка залежності середньої ширини силуету від кількості кластерів
plot(2:10, sil_width[2:10], type = "b", pch = 19,
     xlab = "Кількість кластерів K",
     ylab = "Середня ширина силуету",
     main = "Визначення оптимальної кількості кластерів")

# Створюємо фінальний датафрейм, додаючи до очищених даних кластери
df_final <- df_clean

# Додаємо колонку з належністю до кластерів за методом k-середніх
df_final$Cluster_kmeans <- clusters_kmeans

# Додаємо колонку з належністю до кластерів за методом ієрархічної кластеризації
df_final$Cluster_hc <- clusters_hc

# Виводимо перші рядки результату для попереднього перегляду
head(df_final)

# Побудова таблиці перехрещення статі (Sex) та кластерів, отриманих методом k-середніх
table(df_final$Sex, df_final$Cluster_kmeans)

# Побудова таблиці перехрещення статі (Sex) та кластерів, отриманих ієрархічною кластеризацією
table(df_final$Sex, df_final$Cluster_hc)

# Встановлення пакету dbscan
install.packages("dbscan")

# Підключення бібліотеки для DBSCAN кластеризації
library(dbscan)

# Встановлення фіксованого значення генератора випадкових чисел для відтворюваності результатів
set.seed(123)

# Застосування алгоритму DBSCAN до масштабованих даних
# eps = 0.5 — максимальна відстань між точками в одному кластері
# minPts = 5 — мінімальна кількість точок, щоб сформувати кластер
dbscan_res <- dbscan(df_scaled, eps = 0.5, minPts = 5)

# Виведення таблиці з кількістю об'єктів у кожному кластері
# Кластер 0 означає "шум" — об'єкти, які не належать до жодного кластера
table(dbscan_res$cluster)

# Візуалізація результатів кластеризації методом DBSCAN
# Шумові точки автоматично будуть позначені окремим кольором
fviz_cluster(dbscan_res, data = df_scaled, main = "DBSCAN кластеризація")

# Об'єднуємо всі результати кластеризації в один датафрейм
df_results <- df_clean
df_results$Cluster_kmeans <- as.factor(clusters_kmeans)
df_results$Cluster_hc <- as.factor(clusters_hc)
df_results$Cluster_dbscan <- as.factor(dbscan_res$cluster)  # 0 — це "шум"

# Функція для створення зведеної таблиці частот
get_cluster_table <- function(method_name, cluster_column) {
  tab <- table(Sex = df_results$Sex, Cluster = df_results[[cluster_column]])
  cat(paste0("\n=== Метод: ", method_name, " ===\n"))
  print(tab)
}

# Вивід таблиць для кожного методу
get_cluster_table("K-means", "Cluster_kmeans")
get_cluster_table("Ієрархічна кластеризація", "Cluster_hc")
get_cluster_table("DBSCAN", "Cluster_dbscan")
