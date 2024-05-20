install.packages("caret")
install.packages("e1071")
install.packages("forecast")
install.packages("knitr")
install.packages("kableExtra")
library(knitr)
library(kableExtra)

library(readxl)
library(lattice)
library(caret)
library(e1071)
library(forecast)
data <- read_excel("heart1.xlsx")
colnames(df_total) <- c("Yas", "Cinsiyet", "Agri_Tipi", "Dinlenme_Kan_Basinci", "Kolestorel", "Aclik_Kan_Sekeri", "Dinlenme_EKG", "Maks_Kalp_Atis", "Egzersiz_Angina", "Oldpeak", "ST_Egimi", "Kalp_Hastaligi")

df_total$Cinsiyet <- as.factor(df_total$Cinsiyet)
df_total$Agri_Tipi <- as.factor(df_total$Agri_Tipi)
df_total$Dinlenme_EKG <- as.factor(df_total$Dinlenme_EKG)
df_total$Egzersiz_Angina <- as.factor(df_total$Egzersiz_Angina)
df_total$ST_Egimi <- as.factor(df_total$ST_Egimi)
df_total$Kalp_Hastaligi <- as.factor(df_total$Kalp_Hastaligi)

set.seed(123)
trainIndex <- createDataPartition(df_total$Kalp_Hastaligi, p = .8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- df_total[trainIndex, ]
data_test  <- df_total[-trainIndex, ]

model <- glm(Kalp_Hastaligi ~ ., data = data_train, family = binomial)
print(model)
#########################################################################HATA ANALİZİ###############################
predictions <- predict(model, data_test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

predicted_classes <- as.factor(predicted_classes)
levels(predicted_classes) <- levels(data_test$Kalp_Hastaligi)
confusionMatrix(predicted_classes, data_test$Kalp_Hastaligi)
cm <- confusionMatrix(predicted_classes, data_test$Kalp_Hastaligi)


data_test$Kalp_Hastaligi <- as.numeric(as.character(data_test$Kalp_Hastaligi))
residuals <- data_test$Kalp_Hastaligi - predictions
# ACF grafiğini oluşturun ve lag 0'ı dahil etmeyin
acf_result <- acf(residuals, plot = FALSE)

str(acf_result)
# ACF grafiğini çizdirin
plot(acf_result, main = "Hataların otokolerosyonu", xlab = "Lag", ylab = "ACF")





# Gerçek değerlerle karşılaştırma
confusionMatrix(as.factor(predicted_classes), data_test$Kalp_Hastaligi)
