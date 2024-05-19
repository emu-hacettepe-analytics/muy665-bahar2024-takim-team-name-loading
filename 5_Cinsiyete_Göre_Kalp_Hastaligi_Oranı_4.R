
library(ggplot2)
library(tidyverse)
library(readxl)
data <- read_excel("heart1.xlsx")



df_summary <- df_total %>%
  group_by(Cinsiyet, Kalp_Hastaligi) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Cinsiyet) %>%
  mutate(total = sum(count), proportion = count / total * 100) %>%
  filter(Kalp_Hastaligi == 1)

# grafik_4 yasa göre hastalığa yakalanma
ggplot(df_summary, aes(x = Cinsiyet, y = proportion, fill = Cinsiyet)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("Erkek" = "#9999FF", "Kadin" = "#FF9999")) +
  labs(title = "Cinsiyet & Kalp Hastalığına Yakalanma Oranı(%)", x = "Cinsiyet", y = "Kalp Hastalığına Yakalanma Oranı(%)") +
  theme_minimal(base_size = 15)+
  theme(
   plot.title = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
    geom_text(aes(label = paste0(round(proportion, 1), "%")), vjust = -0.5, size = 5)
