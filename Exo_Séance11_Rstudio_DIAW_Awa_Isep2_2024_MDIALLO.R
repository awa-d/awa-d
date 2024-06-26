#====================================================================================#
#     ENSAE Pierre NDIAYE de Dakar ISEP2 2023-2024                                   #
#     COURS DE Traitement statistiques avec le logiciel R avec M. DIALLO             #
#          Exercice de la seance 11 faire avant le cours du 06 avril 2024            #
#====================================================================================#

#============== Faire le traitement des valeurs manquantes du last homework en utilisant le script des exposants sur le theme ==============
#============== We are going to use three methods: "Deleting missing values", "imputate by mean" , "hot deck method" =======================
#===========================================================================================================================================
#===========================================================================================================================================


#######################################################################################   Actualisations des bases de donnees
# Installer et charger les packages nécessaires
install.packages(c("readxl", "haven", "magrittr", "data.table"))
library(readxl)
library(haven)
library(magrittr)
library(data.table)

# Charger les données
library(haven)
Table_de_conversion_phase_2 <- read_excel("C:/Users/hp/Desktop/Traitement_statistique_avec_R/CoursR_Isep2_2024_ENSAE/Table_de_conversion_phase_2.xlsx")
cereales <- read_dta("C:/Users/hp/Desktop/Traitement_statistique_avec_R/CoursR_Isep2_2024_ENSAE/cereales.dta")
# Renommer les colonnes dans cereales
colnames(cereales)[4:14] <- c("AutresCereales", "Qtty_cons", "Unite_cons", "Taille_cons", "AutoCons", "AutresProv", "DernierAchat", "Qtty_achat", "Unite_achat", "Taille_achat", "Value_achat")

####################################################################################### The Homework

################################################### Method 1 DELETE THE ENTIRE LINES WHERE MISSIING VALUES EXIST

# Database
data1 <- cereales
# Check for missing values
any_missing <- anyNA(data1$Qtty_cons)

# Extract rows without missing values in Qtty_cons column
complete_data1 <- data1[complete.cases(data1$Qtty_cons), ]

# Print the Qtty_cons column and the complete_data1 dataset
print(data1$Qtty_cons)
print(complete_data1) #Now "complete_data1" no longer contain any missing values

################################################### Method 2 IMPUTATE BY MEAN

# Database
data2 <- cereales
# Mean of the Qtty_cons variable
mean_Qtty_cons <- mean(data2$Qtty_cons, na.rm = TRUE)

# Replace missing values in Qtty_cons with the mean
data2$Qtty_cons[is.na(data2$Qtty_cons)] <- mean_Qtty_cons

# Print the updated dataset
print(data2) # Now, "data2" 's values on the Qtty_cons variable have been replaced by the the mean of the variable's observations

################################################### Method 3 IMPUTATE BY HOT DECK method
#'Elle consiste a remplacer les valeurs manquantes par des valeurs observ�es similaires provenant d'autres
#'observations du jeu de donn�es.
#'Fonctionnement:
#' 1. D�finir les groupes d'imputation: On divise le jeu de donn�es en groupes d'observations similaires
#' en fonction de variables cl�s.
#' 2. S�lectionner un donneur: Pour chaque observation avec une valeur manquante, on s�lectionne un
#' donneur al�atoire dans le m�me groupe d'imputation.
#' 3. Imputation de la valeur manquante: On remplace la valeur manquante par la valeur observ�e du
#' donneur pour la variable concern�e.
#' 


#Database
data3 <- cereales
# Define the impute_hot_deck_custom function
impute_hot_deck_custom <- function(data3, similarity_matrix) {
  # For each missing value in Qtty_achat
  for (i in which(is.na(data3$Qtty_achat))) {
    # Identify the indices of observations with non-missing values in Qtty_achat
    non_missing_indices <- which(!is.na(data3$Qtty_achat))
    # Calculate the similarity between observation i and non-missing observations of Qtty_achat
    similarities <- similarity_matrix[i, non_missing_indices]
    # Identify the most similar observation
    most_similar_index <- non_missing_indices[which.min(similarities)]
    # Impute the value of Qtty_achat from the most similar observation
    data3$Qtty_achat[i] <- data3$Qtty_achat[most_similar_index]
  }
  return(data3)
}

# Impute missing values using the defined function
data_imputed <- impute_hot_deck_custom(data3, custom_similarity_matrix)

# Print the imputed data
print(data_imputed)