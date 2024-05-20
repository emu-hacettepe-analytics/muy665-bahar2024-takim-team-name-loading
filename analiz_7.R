
# reshape2 paketini yükleyin
install.packages("reshape2")

# reshape2 paketini yükleyin
library(reshape2)


library(ggplot2)
library(gridExtra)

df_total1 <- within(df_total, Agri_Tipi <- as.numeric(factor(Agri_Tipi, levels = unique(Agri_Tipi))))
df_total1 <- within(df_total, Kalp_Hastaligi <- as.numeric(factor(Kalp_Hastaligi, levels = unique(Kalp_Hastaligi))))

# Korelasyon Matrisi
numeric_vars <- df_total1 %>% select(Yas, Kolestorel, Agri_Tipi,Kalp_Hastaligi)
cor_matrix <- round(cor(numeric_vars, use = "complete.obs"), )

# Korelasyon matrisi görselleştirme
print(cor_matrix)



