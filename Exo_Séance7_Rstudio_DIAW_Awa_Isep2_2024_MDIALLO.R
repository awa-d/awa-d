
#====================================================================================#
#     ENSAE Pierre NDIAYE de Dakar ISEP2 2023-2024                                   #
#     COURS DE Traitement statistiques avec le logiciel R        avec M. DIALLO      #
#          Exercice de la séance 7 á faire avant le cours du 20 mars 2024          #
#====================================================================================#

#============== Exercice Sur la base de donn�es cereales =================


###################################################    I Reprenons les manipulations d�j� faites lors de la séance 6
################################################# avec des commentaires explicites de chaque code

# Installation et chargement du package haven pour lire les fichiers de donn�es STATA
install.packages("haven")
library(haven)

# Lecture du fichier de donn�es cereales.dta
cereales <- read_dta("C:/Users/hp/Desktop/Awa_DIAW_2023_2024/Awa_DIAW_cours/Second_Term_2023_2024_Isep2_ENSAE/Informatique 4/Traitement_statistique_avec_R/CoursR_Isep2_2024_ENSAE/cereales.dta")

# Affichage des donn�es
View(cereales)

# Extraction des noms des variables
nom <- names(cereales)

# Chargement du package tidyverse pour manipuler les donn�es
library(tidyverse)

# Affichage de la structure des donn�es
glimpse(cereales)

# Suppression de la colonne 't' dans le jeu de donn�es
cereales$t <- NULL
#Renommons les variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "value_achat")

# Liste des produits
labprod <- c("Riz local bris�", "Riz local entier", "Riz import� bris�",
             "Riz import� entier", "Riz import� 3", "Ma�s en �pi", "Ma�s en grain", "Mil", "Sorgho", "Bl�", "Fonio",
             "Autres c�r�ales", "Farine de ma�s", "semoule de mais", "Farine/semoule de mil", "semoule de mil",
             "Farine de bl� local ou import�", "semoule de bl�", "Autres farines de c�r�ales", "Autres semoules de c�r�ales",
             "P�tes alimentaires", "Pain moderne", "Pain moderne type 2", "Pain traditionnel", "Pains traditionnel type 2",
             "C�r�ales de petit d�jeuner", "Croissants", "Biscuits", "G�teaux", "Beignets, galettes")

# Identification des produits uniques
levprod <- unique(cereales$cereales__id)

# �dition des niveaux des produits
edit(levprod)

# Noms des niveaux
levprodN <- names(attr(cereales$cereales__id, "labels"))

# Labels des niveaux
levprodL <- unname(attr(cereales$cereales__id, "labels"))

# Transformation de la variable 'cereales__id' en facteur
cereales$produit1 <- as.factor(cereales$cereales__id)

# Affichage de la structure des donn�es apr�s transformation
glimpse(cereales)

# Tableau de fr�quences de 'produit1'
table(cereales$produit1)

# Transformation de la variable 'cereales__id' en facteur avec des labels personnalis�s
cereales$produit <- factor(cereales$cereales__id,
                           levels = levprodL,
                           labels = levprodN)

# Tableau de fr�quences de 'produit'
table(cereales$produit)

# Affichage de la structure des donn�es apr�s transformation
glimpse(cereales)

# �dition des niveaux de 'Unite_cons'
edit(cereales$Unite_cons)

# Transformation de la variable 'Unite_cons' en facteur avec des labels personnalis�s
cereales$unite_cons <- factor(cereales$Unite_cons,
                              levels = unname(attr(cereales$Unite_cons, "labels")),
                              labels = names(attr(cereales$Unite_cons, "labels")))

# Transformation de la variable 'Taille_cons' en facteur avec des labels personnalis�s
cereales$taille_cons <- factor(cereales$Taille_cons,
                               levels = unname(attr(cereales$Taille_cons, "labels")),
                               labels = names(attr(cereales$Taille_cons, "labels")))

# Cr�ation de classes pour la variable 'Qtty_cons'
cereales$classCereal <- cut(cereales$Qtty_cons,
                            labels = c("Tr�s faible", "Faible", "Moyen", "�lev�"),
                            breaks = c(0, 50, 70, 110, 168))

# Tableau de fr�quences de 'classCereal'
table(cereales$classCereal)

# Cr�ation de classes pour la variable 'Qtty_cons' avec des conditions sp�cifiques pour le Riz en Kg
cereales$classCereal_RizKg <- ifelse(cereales$cereales__id == 1 & cereales$Unite_cons == 100,
                                     cut(cereales$Qtty_cons,
                                         labels = c("Tr�s faible", "Faible", "Moyen", "�lev�"),
                                         breaks = c(0, 50, 70, 110, 168)), NA)

# Tableau de fr�quences de 'classCereal_RizKg'
table(cereales$classCereal_RizKg)

# S�lection des niveaux de 'Taille_cons' pour les c�r�ales avec 'Unite_cons' = 100
c0 <- unique(cereales[cereales$Unite_cons == 100, "Taille_cons"])

# S�lection des donn�es pour les c�r�ales dont l'identifiant est inf�rieur � 5 et avec 'unite_cons' = 100
c1 <- cereales[cereales$cereales__id < 5 & cereales$unite_cons == 100, ]


##########################################################    II  Fusionnons la base cereale avec la table de conversion

# Installer et charger le package openxlsx s'il n'est pas d�j� install�
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
library(openxlsx)

# Sp�cifier le chemin complet vers le fichier Excel
chemin_fichier <- "C:/Users/hp/Desktop/Awa_DIAW_2023_2024/Awa_DIAW_cours/Second_Term_2023_2024_Isep2_ENSAE/Informatique 4/Traitement_statistique_avec_R/CoursR_Isep2_2024_ENSAE/Table de conversion phase 2.xlsx"

# Lire le fichier Excel et stocker son contenu dans une variable
table_conversion <- read.xlsx(chemin_fichier)

# Afficher les premi�res lignes de la base de donn�es
head(table_conversion)
# Afficher la base de donn�es table_conversion
View(table_conversion)

# Renommer la variable 'produitID' en 'cereales__id'
colnames(table_conversion)[colnames(table_conversion) == "produitID"] <- "cereales__id"

# Fusionner les donn�es
mergedbase <- merge(cereales, table_conversion, by = "cereales__id", all.x = TRUE)

# Afficher la structure des donn�es fusionn�es
str(mergedbase)
# Afficher la base de donn�es mergedbase
View(mergedbase)