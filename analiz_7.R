
# reshape2 paketini yükleyin
install.packages("reshape2")

# reshape2 paketini yükleyin
library(reshape2)


library(ggplot2)
library(gridExtra)

df_total1 <- within(df_total, Agri_Tipi <- as.numeric(factor(Agri_Tipi, levels = unique(Agri_Tipi))))
df_total1 <- within(df_total, Kalp_Hastaligi <- as.numeric(factor(Kalp_Hastaligi, levels = unique(Kalp_Hastaligi))))

# Korelasyon Matrisi
numeric_vars <- df_total1 %>% select(Yas, Kolestorel, Kalp_Hastaligi, Agri_Tipi)
cor_matrix <- round(cor(numeric_vars, use = "complete.obs"), )

# Korelasyon matrisi görselleştirme
ggplot(data = melted_cor_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  geom_text(aes(label = value), color = "white", size = 3) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Korelasyon") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  labs(title = "Korelasyon Matrisi")



