library(ggplot2)
library(readxl)
library(writexl)

# Échantillon de base de données
data2 <- read_excel("C:/Users/HP/Downloads/ML_table.xlsx")

# Compte le nombre d'occurrences pour chaque paire Parent1/Parent2 et Recombinant
count <- aggregate(Variant ~ Parent1Parent2, data2, length)

# Crée le graphique
ggplot(count, aes(x = "", y =Variant , fill = Parent1Parent2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "Parent1/Parent2") +
  
  theme_minimal() +
  theme(legend.position = "right") +
  scale_fill_discrete(name = "Parent1/Parent2") 
library(dplyr)

# Échantillon de base de données


# Calcul des fréquences pour chaque paire Parent1/Parent2
freq <- count(data2, Parent1Parent2)

# Calcul des pourcentages
freq <- freq %>%
  mutate(percentage = freq$n / sum(freq$n) * 100)

# Affichage du tableau de pourcentages
print(freq)
View(freq)
write_xlsx(freq, "C:/Users/HP/Downloads/ML.xlsx")
