test_predictions <- predict(model_gini, test_data, type = "class")
performance <- evaluate_performance(test_predictions, test_data$Kalp_Hastaligi)

#performans değerlerini görselleştirme
performance_table <- as.data.frame(performance)
print(performance_table, row.names = TRUE)

#Modeli kullanarak yeni veri ile tahmin yapma
predict_heart_disease <- function(model, yeni_veri) {
  prediction <- predict(model, yeni_veri, type = "class")
  ifelse(prediction == 1, "Hasta", "Hasta Degil")
}

predict_heart_disease <- function(model) {
  yeni_veri <- data.frame(
    Yas = as.numeric(readline("Yaşınızı girin: ")),
    Cinsiyet = readline("Cinsiyetinizi girin (Erkek/Kadın): "),
    Agri_Tipi = readline("Ağrı Tipinizi girin (ATA/NAP/ASY/TA): "),
    Dinlenme_Kan_Basinci = as.numeric(readline("Dinlenme Kan Basıncınızı girin: ")),
    Kolestorel = as.numeric(readline("Kolesterol Değerinizi girin: ")),
    Aclik_Kan_Sekeri = as.numeric(readline("Açlık Kan Şekerinizi girin: ")),
    Dinlenme_EKG = readline("Dinlenme EKG Değerinizi girin (Normal/ST/LVH): "),
    Maks_Kalp_Atis = as.numeric(readline("Maksimum Kalp Atış Sayınızı girin: ")),
    Egzersiz_Angina = readline("Egzersiz Angina var mı? (Evet/Hayır): "),
    Oldpeak = as.numeric(readline("Oldpeak değerinizi girin: ")),
    ST_Egimi = readline("ST Egimi Değerinizi girin (Up/Down/Flat): ")
  )
  
  prediction <- predict(model, yeni_veri, type = "class")
  ifelse(prediction == 1, "Hasta", "Hasta Değil")
}

# Modeli kullanarak tahmin yapma
tahmin <- predict_heart_disease(model_gini)
print(tahmin)
