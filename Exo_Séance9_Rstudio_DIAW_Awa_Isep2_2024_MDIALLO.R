
#====================================================================================#
#     ENSAE Pierre NDIAYE de Dakar ISEP2 2023-2024                                   #
#     COURS DE Traitement statistiques avec le logiciel R avec M. DIALLO             #
#          Exercice de la séance 9 á faire avant le cours du 27 mars 2024            #
#====================================================================================#

#============== Exercice Sur la base de données cereales et la table de conversion table conversion 2 =================


###################################################   Travail demandé
#################################################  Reprise de l'exercice de la séance précédente

                #Commençons par charger le package nécessaires (readxl) et importer les données à partir de fichiers Excel

library(readxl)
Table_de_conversion<- read_excel( "C:/Users/hp/Desktop/Awa_DIAW_2023_2024/Awa_DIAW_cours/Second_Term_2023_2024_Isep2_ENSAE/Informatique 4/Traitement_statistique_avec_R/CoursR_Isep2_2024_ENSAE/Table_de_conversion_phase_2.xlsx")
Table_de_conversion$...8 <- NULL # Suppression des colonnes 8 et 9
Table_de_conversion$...9 <- NULL

               #Chargeons le package nécessaires  haven et importer les données à partir de fichiers Stata.
               # Fusionnons les données de céréales avec une table de conversion en utilisant les colonnes appropriées comme clés de fusion


library(haven)
cereales <- read_dta(paste0("C:/Users/hp/Desktop/Awa_DIAW_2023_2024/Awa_DIAW_cours/Second_Term_2023_2024_Isep2_ENSAE/Informatique 4/Traitement_statistique_avec_R/CoursR_Isep2_2024_ENSAE/cereales.dta"))

colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")

mergesec <- merge(cereales, Table_de_conversion, 
                by.x=c("cereales__id", "Unite_achat", "Taille_achat"),
                by.y=c("produitID", "uniteID", "tailleID"),
                all.x = TRUE)

              # Verifions s'il y a des valeurs manquantes 
anyNA(mergesec$Qtty_achat)  # Renvoie true donc il y en a et nous allons donc supprimer les valeurs manquantes

              # Supprimons donc ces valeurs manquantes 
mergesec <- mergesec[!(is.na(mergesec$Qtty_achat)),]

              # Calculons la quantité consommée en kg
library(data.table)
cereales4 <- data.table(mergesec)
setnames(cereales4,"poids","poids_achat")   # Pour renommer la colonne poids en poids acheté "poids_achat"

cereales4[,poids_achat:=as.numeric(poids_achat)]  # Pour convertir en numerique 
cereales4[,qtty_achat_kg:= poids_achat*Qtty_achat/1000]  # On divise par 1000 car les quanntités sont en grammes

              # Calculons le prix unitaire 
cereales4[,prix_unit:= Value_achat/qtty_achat_kg] 

              # Calculons les dépenses de consommation 
### RECONDUISONS LA QUANTITE TOTALE CONSOMMEE 
cereales4[,qtty_cons_kg:= poids_achat*Qtty_cons/1000] 
### Calcul des depenses de consommation proprement dites 
cereales4[,depen_cons:= prix_unit*qtty_cons_kg]  

              #Correction des valeurs aberrantes

#1 Calcul de l'intervalle interquartile
# On supprime au préalable les valeurs manquantes 
cereales4 <- cereales4[!(is.na(cereales4$Qtty_achat)),]
Q1 <- quantile(cereales4$qtty_achat_kg, 0.25)
Q3 <- quantile(cereales4$qtty_achat_kg, 0.75)
IQR <- Q3 - Q1

#2 Définition des limites pour les valeurs aberrantes
upper_bound <- Q3 + 1.5 * IQR
lower_bound <- Q1 - 1.5 * IQR

#3 Suppression des valeurs aberrantes
cereales4 <- cereales4[cereales4$qtty_achat_kg >= lower_bound      #Cette ligne de code filtre les observations dans le dataframe cereales4 en ne conservant que celles pour 
                       & cereales4$qtty_achat_kg <= upper_bound, ]   #lesquelles la valeur de la variable qtty_achat_kg se situe entre lower_bound et upper_bound
dim(cereales4) #  Les observations passent de 11114 à 9495 après enlevement des valeurs manquantes et 8031 apres suppression des valeurs aberrantes
