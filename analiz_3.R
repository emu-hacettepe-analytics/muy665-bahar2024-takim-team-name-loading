install.packages("tidyverse")
install.packages("readxl")
install.packages("ggplot2")

library(ggplot2)
library(tidyverse)
library(readxl)
data <- read_excel("heart1.xlsx")

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0), mar = c(4, 4, 2, 1), cex.lab = 0.8, cex.main = 1, cex.axis = 0.8)

# Kolesterol ve Kalp Hastalığı
boxplot(data$Kolestorel ~ data$Kalp_Hastaligi, 
        main = "Kolesterol - Hastalık", 
        xlab = "Hastalık", 
        ylab = "Kolesterol", 
        col = c("turquoise", "coral"), 
        names = c("Hayır", "Evet"))

# Dinlenme Kan Basıncı ve Kalp Hastalığı
boxplot(data$`Dinlenme_Kan_Basinci mm/HG` ~ data$Kalp_Hastaligi, 
        main = "DinlenmeKanBasıncı-Hastalık", 
        xlab = "Hastalık", 
        ylab = "Dinlenme Kan Basıncı (mm/HG)", 
        col = c("turquoise", "coral"), 
        names = c("Hayır", "Evet"))

# Maksimum Kalp Atışı ve Kalp Hastalığı
boxplot(data$Maks_Kalp_Atis ~ data$Kalp_Hastaligi, 
        main = "MaksimumKalpAtışı-Hastalık", 
        xlab = "Hastalık", 
        ylab = "Maksimum Kalp Atışı", 
        col = c("turquoise", "coral"), 
        names = c("Hayır", "Evet"))

# Yaş ve Kalp Hastalığı
boxplot(data$Yas ~ data$Kalp_Hastaligi, 
        main = "Yaş - Hastalık", 
        xlab = "HHastalık", 
        ylab = "Yaş", 
        col = c("turquoise", "coral"), 
        names = c("Hayır", "Evet"))

#Dış çerçeve başlığı
title("Niteliklerin Hastalığa Etkisi(Boxplots)", outer = TRUE, cex.main = 1)
