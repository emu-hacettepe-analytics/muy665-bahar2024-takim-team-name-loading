

library(ggplot2)
library(tidyverse)
library(readxl)
data <- read_excel("heart1.xlsx")


age_heart_disease <- df_total %>%
  group_by(Yas, Kalp_Hastaligi) %>%
  summarise(count = n()) %>%
  spread(Kalp_Hastaligi, count, fill = 0) %>%
  mutate(total = `0` + `1`,
         percent = `1` / total * 100)

# Grafiği oluşturma
ggplot(age_heart_disease, aes(x = Yas, y = percent)) +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_line(color = "blue") +
  labs(title = "Yaşlara Göre Kalp Hastalığı Olma Durumu",
       x = "Yaş",
       y = "Kalp Hastalığı Olma Oranı (%)") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(20, 80, by = 5))