install.packages("tidyverse")
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("scales")
install.packages("plotly")
install.packages("ggrepel")
install.packages("ggthemes")
install.packages("kableExtra")





library(kableExtra)
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(plotly)
library(ggrepel)
library(ggthemes)
df_total <- read_excel("heart1.xlsx")
head(df_total)
# Erkek ve Kadın sayısını hesapla
erkek_sayisi <- sum(df_total$Cinsiyet == "Erkek")
kadin_sayisi <- sum(df_total$Cinsiyet == "Kadin")
# Sonuçları yazdır
cat("Erkek sayısı:", erkek_sayisi, "\n")
cat("Kadın sayısı:", kadin_sayisi, "\n")
#Cinsiyet dağılımını bar grafik olarak çiz grafik_1.2
barplot(table(df_total$Cinsiyet), main = "Cinsiyet Dağılımı", xlab = "Cinsiyet", ylab = "Kişi Sayısı", col = c("blue", "pink"), legend = c("Erkek", "Kadın"))

#kadın sayısı ve erkek sayısı arasındaki farkı gösteren histogram
# cinsiyet_sayisi <- table(df_total$Cinsiyet)
# ggplot(data = NULL, aes(x = names(cinsiyet_sayisi), y = cinsiyet_sayisi)) +
#   geom_bar(stat = "identity", fill = c("blue", "red")) +
#   labs(x = "Cinsiyet", y = "Kişi Sayısı") +
#   ggtitle("Erkek ve Kadın Sayısı") +
#   theme_minimal()

#Cinsiyet_Yas dağılımlarına göre bir grafik_1
ggplot(df_total, aes(x = Yas, fill = Cinsiyet)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 10) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Yaş", y = "Frekans", title = "Erkek ve Kadınların Yaş Dağılımları") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(30, 70, by = 5))+
  scale_y_continuous(breaks = seq(0, 200, by = 10))





















