library(ggplot2)
library(tidyverse)

df_total <- read_excel("heart1.xlsx")
par(mar = c(5, 5, 4, 2) + 0.1)  # Alt, sol, üst, sağ kenar boşlukları
# # Kan Basıncı ve Kolesterol İlişkisi Scatter Plot
# plot(df_total$`Dinlenme_Kan_Basinci mm/HG`, df_total$Kolestorel, xlab = "Dinlenme Kan Basıncı (mm/HG)", ylab = "Kolesterol", main = "Kan Basıncı ve Kolesterol İlişkisi", col = ifelse(df_total$Kalp_Hastaligi== 1, "red", "blue"))
# legend("topright", legend = c("Kalp Hastalığı Yok", "Kalp Hastalığı Var"), col = c("blue", "red"), pch = 1)

# Görüldüğü üzere, genel eğilim, dinlenme kan basıncının yüksek olduğu durumlarda kolesterol seviyelerinin arttığı yönündedir. 
#Ayrıca, kalp hastalığı olan bireylerin genellikle daha yüksek kolesterol seviyelerine sahip olduğu gözlemlenmektedir.

#Ancak, bu ilişkinin kesin olmadığını belirtmek önemlidir; çünkü bazı düşük dinlenme kan basıncı değerlerine sahip bireylerin 
#yine de yüksek kolesterol seviyelerine sahip olduğu noktalar görülebilmektedir.


# Kolesterol değeri sıfır olanları filtrele
df_filtered <- df_total[df_total$Kolestorel > 0,]

plot(df_filtered$`Dinlenme_Kan_Basinci mm/HG`, df_filtered$Kolestorel, 
     xlab = "Dinlenme Kan Basıncı (mm/HG)", 
     ylab = "Kolesterol", 
     main = "Kan Basıncı ve Kolesterol İlişkisi", 
     col = ifelse(df_total$Kalp_Hastaligi == 1, "red", "blue"),
     pch = 19,  # Noktaların şekli
     cex = 0.5, # Nokta boyutu
     lwd = 2,# Nokta çizgi kalınlığı
     alpha = 0.5)  # Nokta saydamlgııı 
legend("topright", 
       legend = c("Kalp Hastalığı Yok", "Kalp Hastalığı Var"), 
       col = c("blue", "red"), 
       pch = 19, 
       pt.cex = 1.1, # Nokta boyutu
       cex = 0.5,    # Metin boyutu
       title = "Kalp Hastalığı", 
       bg = "yellow",  # Arkaplan rengi
       box.lwd = 1.5) # Kutu çizgi kalınlığı

