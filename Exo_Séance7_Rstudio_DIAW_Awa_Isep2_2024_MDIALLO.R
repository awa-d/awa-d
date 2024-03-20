
#====================================================================================#
#     ENSAE Pierre NDIAYE de Dakar ISEP2 2023-2024                                   #
#     COURS DE Traitement statistiques avec le logiciel R        avec M. DIALLO      #
#          Exercice de la sÃ©ance 7 Ã¡ faire avant le cours du 20 mars 2024          #
#====================================================================================#

#============== Exercice Sur la base de données cereales =================


###################################################    I Reprenons les manipulations déjà faites lors de la sÃ©ance 6
################################################# avec des commentaires explicites de chaque code

# Installation et chargement du package haven pour lire les fichiers de données STATA
install.packages("haven")
library(haven)

# Lecture du fichier de données cereales.dta
cereales <- read_dta("C:/Users/hp/Desktop/Awa_DIAW_2023_2024/Awa_DIAW_cours/Second_Term_2023_2024_Isep2_ENSAE/Informatique 4/Traitement_statistique_avec_R/CoursR_Isep2_2024_ENSAE/cereales.dta")

# Affichage des données
View(cereales)

# Extraction des noms des variables
nom <- names(cereales)

# Chargement du package tidyverse pour manipuler les données
library(tidyverse)

# Affichage de la structure des données
glimpse(cereales)

# Suppression de la colonne 't' dans le jeu de données
cereales$t <- NULL
#Renommons les variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "value_achat")

# Liste des produits
labprod <- c("Riz local brisé", "Riz local entier", "Riz importé brisé",
             "Riz importé entier", "Riz importé 3", "Maïs en épi", "Maïs en grain", "Mil", "Sorgho", "Blé", "Fonio",
             "Autres céréales", "Farine de maïs", "semoule de mais", "Farine/semoule de mil", "semoule de mil",
             "Farine de blé local ou importé", "semoule de blé", "Autres farines de céréales", "Autres semoules de céréales",
             "Pâtes alimentaires", "Pain moderne", "Pain moderne type 2", "Pain traditionnel", "Pains traditionnel type 2",
             "Céréales de petit déjeuner", "Croissants", "Biscuits", "Gâteaux", "Beignets, galettes")

# Identification des produits uniques
levprod <- unique(cereales$cereales__id)

# Édition des niveaux des produits
edit(levprod)

# Noms des niveaux
levprodN <- names(attr(cereales$cereales__id, "labels"))

# Labels des niveaux
levprodL <- unname(attr(cereales$cereales__id, "labels"))

# Transformation de la variable 'cereales__id' en facteur
cereales$produit1 <- as.factor(cereales$cereales__id)

# Affichage de la structure des données après transformation
glimpse(cereales)

# Tableau de fréquences de 'produit1'
table(cereales$produit1)

# Transformation de la variable 'cereales__id' en facteur avec des labels personnalisés
cereales$produit <- factor(cereales$cereales__id,
                           levels = levprodL,
                           labels = levprodN)

# Tableau de fréquences de 'produit'
table(cereales$produit)

# Affichage de la structure des données après transformation
glimpse(cereales)

# Édition des niveaux de 'Unite_cons'
edit(cereales$Unite_cons)

# Transformation de la variable 'Unite_cons' en facteur avec des labels personnalisés
cereales$unite_cons <- factor(cereales$Unite_cons,
                              levels = unname(attr(cereales$Unite_cons, "labels")),
                              labels = names(attr(cereales$Unite_cons, "labels")))

# Transformation de la variable 'Taille_cons' en facteur avec des labels personnalisés
cereales$taille_cons <- factor(cereales$Taille_cons,
                               levels = unname(attr(cereales$Taille_cons, "labels")),
                               labels = names(attr(cereales$Taille_cons, "labels")))

# Création de classes pour la variable 'Qtty_cons'
cereales$classCereal <- cut(cereales$Qtty_cons,
                            labels = c("Très faible", "Faible", "Moyen", "Élevé"),
                            breaks = c(0, 50, 70, 110, 168))

# Tableau de fréquences de 'classCereal'
table(cereales$classCereal)

# Création de classes pour la variable 'Qtty_cons' avec des conditions spécifiques pour le Riz en Kg
cereales$classCereal_RizKg <- ifelse(cereales$cereales__id == 1 & cereales$Unite_cons == 100,
                                     cut(cereales$Qtty_cons,
                                         labels = c("Très faible", "Faible", "Moyen", "Élevé"),
                                         breaks = c(0, 50, 70, 110, 168)), NA)

# Tableau de fréquences de 'classCereal_RizKg'
table(cereales$classCereal_RizKg)

# Sélection des niveaux de 'Taille_cons' pour les céréales avec 'Unite_cons' = 100
c0 <- unique(cereales[cereales$Unite_cons == 100, "Taille_cons"])

# Sélection des données pour les céréales dont l'identifiant est inférieur à 5 et avec 'unite_cons' = 100
c1 <- cereales[cereales$cereales__id < 5 & cereales$unite_cons == 100, ]


##########################################################    II  Fusionnons la base cereale avec la table de conversion

# Installer et charger le package openxlsx s'il n'est pas déjà installé
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
library(openxlsx)

# Spécifier le chemin complet vers le fichier Excel
chemin_fichier <- "C:/Users/hp/Desktop/Awa_DIAW_2023_2024/Awa_DIAW_cours/Second_Term_2023_2024_Isep2_ENSAE/Informatique 4/Traitement_statistique_avec_R/CoursR_Isep2_2024_ENSAE/Table de conversion phase 2.xlsx"

# Lire le fichier Excel et stocker son contenu dans une variable
table_conversion <- read.xlsx(chemin_fichier)

# Afficher les premières lignes de la base de données
head(table_conversion)
# Afficher la base de données table_conversion
View(table_conversion)

# Renommer la variable 'produitID' en 'cereales__id'
colnames(table_conversion)[colnames(table_conversion) == "produitID"] <- "cereales__id"

# Fusionner les données
mergedbase <- merge(cereales, table_conversion, by = "cereales__id", all.x = TRUE)

# Afficher la structure des données fusionnées
str(mergedbase)
# Afficher la base de données mergedbase
View(mergedbase)