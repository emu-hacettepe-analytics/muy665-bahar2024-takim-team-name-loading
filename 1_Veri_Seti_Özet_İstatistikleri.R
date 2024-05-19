install.packages("pander")
install.packages("knitr")
install.packages("kableExtra")

library(pander)
library(ggplot2)
library(tidyverse)
library(readxl)
library(knitr)
library(kableExtra)
library(dplyr)
data <- read_excel("heart1.xlsx")

head(df_filtered)
sum(is.na(df_total))


library(pander)

# Veri seti özetini pander kullanarak gösterme
pander(summary(df_filtered), caption = "Veri Seti Özet İstatistikleri")

# Veri seti özetini dataframe olarak oluşturma
df_summary <- df_total %>%
  summarise(
    Yas_mean = mean(Yas, na.rm = TRUE),
    Yas_sd = sd(Yas, na.rm = TRUE),
    Dinlenme_Kan_Basinci_mean = mean(`Dinlenme_Kan_Basinci mm/HG`, na.rm = TRUE),
    Dinlenme_Kan_Basinci_sd = sd(`Dinlenme_Kan_Basinci mm/HG`, na.rm = TRUE),
    Kolestorel_mean = mean(Kolestorel, na.rm = TRUE),
    Kolestorel_sd = sd(Kolestorel, na.rm = TRUE),
    Aclik_Kan_Sekeri_mean = mean(Aclik_Kan_Sekeri, na.rm = TRUE),
    Aclik_Kan_Sekeri_sd = sd(Aclik_Kan_Sekeri, na.rm = TRUE),
    Maks_Kalp_Atis_mean = mean(Maks_Kalp_Atis, na.rm = TRUE),
    Maks_Kalp_Atis_sd = sd(Maks_Kalp_Atis, na.rm = TRUE),
    Oldpeak_mean = mean(Oldpeak, na.rm = TRUE),
    Oldpeak_sd = sd(Oldpeak, na.rm = TRUE)
  ) %>%
  gather(key = "Metric", value = "Value")

# Daha güzel bir tablo olarak gösterme
df_summary %>%
  kable("html", caption = "Veri Seti Özet İstatistikleri") %>%
  kable_styling("striped", full_width = F)