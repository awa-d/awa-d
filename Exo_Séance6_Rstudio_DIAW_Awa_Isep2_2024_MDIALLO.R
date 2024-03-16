

#====================================================================================#
#     ENSAE Pierre NDIAYE de Dakar ISEP2 2023-2024                                   #
#     COURS DE Traitement statistiques avec le logiciel R        avec M. DIALLO      #
#           Exercice de la séance 5 á faire avant le cours du 16 mars 2024           #
#====================================================================================#

#============== Exercice  Sur la base de données cereales =================


################################################################################    I Reprenons les manipulations déjà faites

##
install.packages(haven)
library(haven)
cereales <- read_dta("C:/Users/hp/Desktop/Awa_DIAW_2023_2024/Awa_DIAW_cours/Second_Term_2023_2024_Isep2_ENSAE/Informatique 4/Traitement_statistique_avec_R/CoursR_Isep2_2024_ENSAE/cereales.dta")
View(cereales)
nom <- names(cereales)

colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")
#
library (tidyverse)
##Structure de la base
glimpse(cereales)
view(cereales)



################################################################################       II Exercice

#########################    Renommons, créons et labellisons les variables>> Choix variable value_achat

#Renommons la variable value_achat en Total acheté
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Total_acheté")
#########################    Créons une nouvelle variable "A_acheté" qui en réalité est une fonction indicatrice qui prend la valeur 1 si Total_acheté est non null et 0 sinon
# Créons une nouvelle variable "A_acheté" en utilisant une fonction indicatrice
cereales$A_acheté <- ifelse(!is.na(cereales$Total_acheté), 1, 0)
# Affichons la colonne "A_acheté" dans le dataframe cereales
View(cereales$A_acheté)

#########################    Recoder>> Variable choisie:  "Total_acheté"
# Définissons les seuils 
seuil_beaucoup <- 10000  # Par exemple, si la quantité est supérieure à 10000
seuil_suffisant <- 3000       # Par exemple, si la quantité est entre 3000 et 10000
seuil_insuffisant <- 500  # Par exemple, si la quantité est supérieure à 500
# Par défaut, si la quantité est inférieure à 500, on considère qu'e rien n'a été acheté'il a atteint le seuil critique
# Recoder la variable "Total_acheté" en fonction du montant achetée
cereales <- cereales %>%
  mutate(Total_acheté_recodé = case_when(
    Total_acheté > seuil_beaucoup ~ "A Beaucoup acheté",
    Total_acheté > seuil_suffisant & Total_acheté <= seuil_beaucoup ~ "A suffisamment acheté",
    Total_acheté <= seuil_suffisant& Total_acheté <= seuil_suffisant ~ "Achat insuffisant",
    Total_acheté <= seuil_insuffisant ~ "Situation critique"
  ))
View(cereales$Total_acheté_recodé)

#########################  Changer le type d'une variable 

#Variable choisie: "Qtty_cons" >> en numérique
cereales$Qtty_cons <- as.numeric(cereales$Qtty_cons)

