#Paketlerin Yüklenmesi
install.packages("readxl")
install.packages("caret")
install.packages("rpart")
install.packages("e1071")
install.packages("rpart.plot")

#Kütüphanelerin yüklenmesi
library(rpart)
library(readxl)
library(caret)
library(e1071)
library(rpart.plot)

#Veri setinin okunması ve Kolesterol=0 değerlerin silinmesi
veri <- read_excel("heart1.xlsx")
veri_duzeltilmis <- subset(veri, Kolestorel != 0)

#Veri setindeki Dinlenme Kan Basıcı mm/HG değişkeninden mm/HG çıkarılması
colnames(veri_duzeltilmis)[colnames(veri_duzeltilmis) == "Dinlenme_Kan_Basinci mm/HG"] <- "Dinlenme_Kan_Basinci"

#Veriyi Bölme - train ve test 
set.seed(123) 
train_index <- createDataPartition(veri_duzeltilmis$Kalp_Hastaligi, p = 0.7, list = FALSE)
train_data <- veri_duzeltilmis[train_index, ]
test_data <- veri_duzeltilmis[-train_index, ]

#GİNİ min. kuralına göre karar ağacı modeli oluşturma
model_gini <- rpart(Kalp_Hastaligi ~ Yas + Cinsiyet + Agri_Tipi + Dinlenme_Kan_Basinci + Kolestorel + Aclik_Kan_Sekeri + Dinlenme_EKG + Maks_Kalp_Atis + Egzersiz_Angina + Oldpeak + ST_Egimi, data = train_data, method = "class", parms = list(split = "gini"))

#Modelin performans değerlendirmesi için fonksiyon yazma
evaluate_performance <- function(predictions, actual) {
  confusion_matrix <- table(predictions, actual)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision <- confusion_matrix[2,2] / sum(confusion_matrix[,2])
  recall <- confusion_matrix[2,2] / sum(confusion_matrix[2,])
  f1_score <- 2 * precision * recall / (precision + recall)
  performance <- c(Accuracy = accuracy, Precision = precision, Recall = recall, F1_Score = f1_score)
  return(performance)
}

#Performans değerlerini yazdır
cat("Model Performansı:\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

#Karar ağacını görselleştirme
rpart.plot(model_gini, main="Kalp Hastalığı Tahminlemek İçin Karar Agacı Modeli", extra=101, under=TRUE, tweak=0.8)


#Test seti üzerinden performans değerleme
test_predictions <- predict(model_gini, test_data, type = "class")
performance <- evaluate_performance(test_predictions, test_data$Kalp_Hastaligi)

#performans değerlerini görselleştirme
performance_table <- as.data.frame(performance)
print(performance_table, row.names = TRUE)

