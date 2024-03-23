
#====================================================================================#
#     ENSAE Pierre NDIAYE de Dakar ISEP2 2023-2024                                   #
#     COURS DE Traitement statistiques avec le logiciel R avec M. DIALLO             #
#          Exercice de la séance 8 á faire avant le cours du 23 mars 2024            #
#====================================================================================#

#============== Exercice Sur la base de données cereales =================


###################################################    I Reprenons les manipulations déjà faites lors de la séance 6
################################################# 

# Installation et chargement du package haven pour lire les fichiers de donn?es STATA
install.packages("haven")
library(haven)

# Lecture du fichier de donn?es cereales.dta
cereales <- read_dta("C:/Users/hp/Desktop/Awa_DIAW_2023_2024/Awa_DIAW_cours/Second_Term_2023_2024_Isep2_ENSAE/Informatique 4/Traitement_statistique_avec_R/CoursR_Isep2_2024_ENSAE/cereales.dta")

# Affichage des donn?es
View(cereales)

# Extraction des noms des variables
nom <- names(cereales)

# Chargement du package tidyverse pour manipuler les donn?es
library(tidyverse)

# Affichage de la structure des donn?es
glimpse(cereales)

# Suppression de la colonne 't' dans le jeu de donn?es
cereales$t <- NULL
#Renommons les variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "value_achat")

# Liste des produits
labprod <- c("Riz local bris?", "Riz local entier", "Riz import? bris?",
             "Riz import? entier", "Riz import? 3", "Ma?s en ?pi", "Ma?s en grain", "Mil", "Sorgho", "Bl?", "Fonio",
             "Autres c?r?ales", "Farine de ma?s", "semoule de mais", "Farine/semoule de mil", "semoule de mil",
             "Farine de bl? local ou import?", "semoule de bl?", "Autres farines de c?r?ales", "Autres semoules de c?r?ales",
             "P?tes alimentaires", "Pain moderne", "Pain moderne type 2", "Pain traditionnel", "Pains traditionnel type 2",
             "C?r?ales de petit d?jeuner", "Croissants", "Biscuits", "G?teaux", "Beignets, galettes")

# Identification des produits uniques
levprod <- unique(cereales$cereales__id)

# ?dition des niveaux des produits
edit(levprod)

# Noms des niveaux
levprodN <- names(attr(cereales$cereales__id, "labels"))

# Labels des niveaux
levprodL <- unname(attr(cereales$cereales__id, "labels"))

# Transformation de la variable 'cereales__id' en facteur
cereales$produit1 <- as.factor(cereales$cereales__id)

# Affichage de la structure des donn?es apr?s transformation
glimpse(cereales)

# Tableau de fr?quences de 'produit1'
table(cereales$produit1)

# Transformation de la variable 'cereales__id' en facteur avec des labels personnalis?s
cereales$produit <- factor(cereales$cereales__id,
                           levels = levprodL,
                           labels = levprodN)

# Tableau de fr?quences de 'produit'
table(cereales$produit)

# Affichage de la structure des donn?es apr?s transformation
glimpse(cereales)

# ?dition des niveaux de 'Unite_cons'
edit(cereales$Unite_cons)

# Transformation de la variable 'Unite_cons' en facteur avec des labels personnalis?s
cereales$unite_cons <- factor(cereales$Unite_cons,
                              levels = unname(attr(cereales$Unite_cons, "labels")),
                              labels = names(attr(cereales$Unite_cons, "labels")))

# Transformation de la variable 'Taille_cons' en facteur avec des labels personnalis?s
cereales$taille_cons <- factor(cereales$Taille_cons,
                               levels = unname(attr(cereales$Taille_cons, "labels")),
                               labels = names(attr(cereales$Taille_cons, "labels")))

# Cr?ation de classes pour la variable 'Qtty_cons'
cereales$classCereal <- cut(cereales$Qtty_cons,
                            labels = c("Tr?s faible", "Faible", "Moyen", "?lev?"),
                            breaks = c(0, 50, 70, 110, 168))

# Tableau de fr?quences de 'classCereal'
table(cereales$classCereal)

# Cr?ation de classes pour la variable 'Qtty_cons' avec des conditions sp?cifiques pour le Riz en Kg
cereales$classCereal_RizKg <- ifelse(cereales$cereales__id == 1 & cereales$Unite_cons == 100,
                                     cut(cereales$Qtty_cons,
                                         labels = c("Tr?s faible", "Faible", "Moyen", "?lev?"),
                                         breaks = c(0, 50, 70, 110, 168)), NA)

# Tableau de fr?quences de 'classCereal_RizKg'
table(cereales$classCereal_RizKg)

# S?lection des niveaux de 'Taille_cons' pour les c?r?ales avec 'Unite_cons' = 100
c0 <- unique(cereales[cereales$Unite_cons == 100, "Taille_cons"])

# S?lection des donn?es pour les c?r?ales dont l'identifiant est inf?rieur ? 5 et avec 'unite_cons' = 100
c1 <- cereales[cereales$cereales__id < 5 & cereales$unite_cons == 100, ]


###################################################    II Complétons avec les manipulations déjà faites lors de la séance 7
#################################################         avant de faire les travaux supplémentaires demandés

# Installer et charger le package openxlsx s'il n'est pas déjà installé
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
library(openxlsx)

## TABLE DE CONVERSION
library(readxl)
Table_de_conversion_phase_2 <- read_excel("C:/Users/hp/Desktop/Awa_DIAW_2023_2024/Awa_DIAW_cours/Second_Term_2023_2024_Isep2_ENSAE/Informatique 4/Traitement_statistique_avec_R/CoursR_Isep2_2024_ENSAE/Table de conversion phase 2.xlsx")

View(Table_de_conversion_phase_2)
names(Table_de_conversion_phase_2)
##
tableconves<- Table_de_conversion_phase_2 %>% 
  select(-c(8,9)) %>%
  mutate(produit=factor(produitID, 
                        levels=produitID,
                        labels=produitNom
  ), 
  unite_cons=factor(uniteID,levels=uniteID,
                    labels=uniteNom),
  taille_cons=factor(tailleID,levels=tailleID,
                     labels=tailleNom))

glimpse(tableconves)

## Fusion des bases, pour obtenir le poids des alimements 
## consomommes; 

# cereales2 <- merge(cereales,tableconves,
#                    by=c("produit","unite_cons","taille_cons"),
#                    all.x = T) ## Ne marche pas ; 
# glimpse(cereales2)


glimpse(cereales)
glimpse(tableconves)
cereales3 <- merge(cereales,tableconves, 
                   by.x = c("cereales__id","Unite_cons","Taille_cons"),
                   by.y = c("produitID","uniteID","tailleID"),
                   all.x = T)
glimpse(cereales3)
View(cereales3)

## Poids 
library(data.table)
cereales3 <- data.table(cereales3)
setnames(cereales3,"poids","poids_cons")

## NA dans poids 
anyNA(cereales3$poids_cons)
sum(is.na(cereales3$poids_cons))

## Quantity conso en unite standard (kg)
cereales3 [, qtty_cons_kg:= poids_cons*Qtty_cons/1000]

is.numeric(cereales3$Qtty_cons)
is.numeric(cereales3$poids_cons)
# on convertit poids en numeric 
cereales3[,poids_cons:=as.numeric(poids_cons)]
is.numeric(cereales3$poids_cons)

#cereales3 %>% summarise(qtty_cons_kg) :: a regarder 

cereales3 [, qtty_cons_kg:= poids_cons*Qtty_cons/1000]
cereales3[,summary(qtty_cons_kg)]

## valeur anormale 

cereales3_anormal <- cereales3[qtty_cons_kg>1000]
glimpse(cereales3_anormal)
View(cereales3_anormal)

###                                                    ************ HomeWork                                         ##########

###################################################    III Les travaux supplémentaires demandés
#################################################         

                              #' calculer la quantite achetee en kg
# Ramenons toutes les observations de Unité_acheté en Kg 138>50kg et 100>1Kg
# Remplacer toutes les occurrences de 138 par 50 dans la variable variable_a_modifier
cereales3$Unite_achat_uniforme[cereales3$Unite_achat == 138] <- 50
 # Calculer la somme du produit des deux colonnes, en ignorant les valeurs NA
 Total_Qtty_achat <- sum(cereales3$Unite_achat_uniforme * cereales3$Qtty_achat, na.rm = TRUE)
 # Afficher la somme du produit
 print(Total_Qtty_achat)
 print(paste("La quantité totale achetée est", Total_Qtty_achat))
                            #' calculer le prix unitaire 
Prix_total<-sum(cereales3$value_achat, na.rm = TRUE)
print(Prix_total)
Prix_unitaire <- Prix_total/Total_Qtty_achat
print(Prix_unitaire)
 
                            #' Calculer les depenses de consommation >> = Prix_total
depenses_consommation <- Total_Qtty_achat * Prix_unitaire
cat("Les dépenses de consommation sont ", depenses_consommation,"FCFA" ,"\n")
 
                           #' Valeurs aberrantes :: corrections  

# Parcourir chaque colonne du dataframe cereales3
for (colonne in names(cereales3)) {
  # Vérifier si la colonne est numérique
  if (is.numeric(cereales3[[colonne]])) {
    # Identifier les valeurs aberrantes dans la colonne
    ecart_type <- sd(cereales3[[colonne]], na.rm = TRUE)
    seuil_aberrant <- 300 # Arbitraire
    valeurs_aberrantes <- cereales3[[colonne]][abs(cereales3[[colonne]] - mean(cereales3[[colonne]], na.rm = TRUE)) > seuil_aberrant * ecart_type]
    
    # Remplacer les valeurs aberrantes par la médiane de la colonne
    cereales3[[colonne]][which(cereales3[[colonne]] %in% valeurs_aberrantes)] <- median(cereales3[[colonne]], na.rm = TRUE)
  }
}

# Afficher le dataframe après correction
print(cereales3)
View(cereales3)
