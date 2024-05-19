

library(gridExtra)
library(ggplot2)
library(tidyverse)
library(readxl)
data <- read_excel("heart1.xlsx")
df_filtered <- df_total[df_total$Kolestorel > 0,]

# Yaş dağılımı
plot_age <- ggplot(df_filtered, aes(x = Yas)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Yaş Dağılımı", x = "Yaş", y = "Frekans")

# Dinlenme Kan Basıncı dağılımı
plot_bp <- ggplot(df_filtered, aes(x = `Dinlenme_Kan_Basinci mm/HG`)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Kan Basıncı", x = "Kan Basıncı", y = "Frekans")

# Kolesterol dağılımı
plot_chol <- ggplot(df_filtered, aes(x = Kolestorel)) +
  geom_histogram(binwidth = 10, fill = "lightcoral", color = "black") +
  labs(title = "Kolesterol", x = "Kolesterol", y = "Frekans")

# Grafiği yan yana gösterme
grid.arrange(plot_age, plot_bp, plot_chol, ncol = 3)

#Yaş ve Kalp Hastalığı İlişkisi:

# Histogram kullanarak farklı yaş gruplarında kalp hastalığı olan ve olmayan kişilerin dağılımını gösterir.
# Cinsiyet ve Kolesterol Seviyeleri:
#   
#   Boxplot kullanarak erkek ve kadınlarda kolesterol seviyelerinin dağılımını karşılaştırır.
# Dinlenme Kan Basıncı ve Kalp Hastalığı İlişkisi:
#   
#   Histogram kullanarak farklı dinlenme kan basıncı seviyelerinde kalp hastalığı olan ve olmayan kişilerin dağılımını gösterir.
# Bu grafikler verinizdeki trendleri ve ilişkileri daha iyi anlamanıza yardımcı olacaktır. Bu analizlerin yanı sıra, başka değişkenler arasında da benzer ilişkileri inceleyebilirsiniz.
