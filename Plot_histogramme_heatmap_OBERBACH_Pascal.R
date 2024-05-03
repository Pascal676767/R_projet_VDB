library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(reshape2)
library(gplots)


################# BARSPLOT DES CATEGORIES D'AGE PAR EPIDEMIES ##################

#//// Import des données \\\\\\

# Retranscription des données de l'article (tableau 1) en dataframe
table1 <- data.frame(
  Catégories = c("n", "2-19", "20-44", "45-64", "65+", "male", "female"),
  Epidémie_1 = c(516, 57, 133, 259, 67, 203, 313),
  Epidémie_2 = c(558, 24, 95, 359, 80, 228, 330),
  Epidémie_3 = c(619, 32, 124, 330, 133, 242, 377),
  Epidémie_4 = c(585, 35, 106, 298, 146, 229, 356)
)

#//// Pre-processing \\\\\\

# Mise en forme des données
df_long <- tidyr::gather(table1, key = "epidemie", value = "valeur", -Catégories)

# Exclure les catégories "male" et "female"
df_long <- df_long[!(df_long$Catégories%in% c("male", "female", "n")), ]

#//// Bar plot \\\\\

couleurs_personnalisees <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072")

# Bar plot avec couleurs personnalisées et titre de taille cex=2
ggplot(df_long, aes(x = epidemie, y = valeur, fill = Catégories)) +
  geom_bar(stat = "identity") +
  labs(title = "Sérum récolté par épidémies en fonction des tranches d'âge",
       x = "Épidémie", y = "Nombre d'individus") +
  scale_fill_manual(values = couleurs_personnalisees) +
  theme_minimal() +
  theme(plot.title = element_text(size = 30))

################# HEATMAP DU NIVEAU D'INFECTIONS DES EPIDEMIES #################

#//// Import des données \\\\\\

# Retranscription des données de l'article (tableau 3) en dataframe
table3 <- data.frame(
  X = c("2-19y", "20-44y", "45-64y", ">=65y"),
  Epidémie1 = c(0.11, 0.05, 0.10, 0.17),
  Epidémie2 = c(0.21, 0.15, 0.23, 0.20),
  Epidémie3 = c(0.04, 0.06, 0.07, 0.14),
  Epidémie4 = c(0.17, 0.04, 0.04, 0.07)
)


# Réorganiser les niveaux de la colonne X dans le dataframe table3
table3$X <- factor(table3$X, levels = c(">=65y", "45-64y", "20-44y", "2-19y"))

#//// Heatmap \\\\\

heatmap.2(acast(melt(table3, id.vars = "X"), X ~ variable, value.var = "value"),
          col = colorRampPalette(c("white", "blue"))(50), 
          scale = "none",  # Désactiver la normalisation
          margins = c(10, 10),
          main = "Heatmap des taux d'infection par groupe en fonction des épidémies",
          ylab = "Groupes d'âge",
          xlab = "Épidémies",
          Colv = FALSE, 
          symm = TRUE,  
          Rowv = FALSE,
          trace="none",
          hclustfun = NULL,
          key = TRUE,
          density.info=c("none"),
          key.title = NULL)
          

