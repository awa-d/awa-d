#################################################################Exercice à faire pour la séance 3
                                 # 1) Créer une base de données fictive
ma_bdd <- data.frame (
  prenoms_nom = c("Awa DIAW", "Mame Balla BOUSSO", "Ahmadou NIASS", "Khadidiatou COULIBALY", "Khadidiatou DIAKHATE", "Papa Amadoou NIANG","Alioune Abdou Salam KANE", "Malick SENE", "Ange Emilson Rayan RAHERINASOLO", "Jeanne de la flèche ONANENA AMANA", "Sarah-Laure FOGWOUNG DJOUFACK", "Célina NGUEMFOUO NGOUMTSA", "Hildegarde Edima BIYENDA", "Samba DIENG", "Tamsir NDONG", "Ameth FAYE", "Mouhamadou Hady DIALLO"),
  surnom = c("Diawa", "MBB ISEP", "Imam", "Championne", "Experte Diakité", "Expert PAN","Businessman", "El Hadji Malick ML", "Expert VBA ML", "Fléchette", "Génie", "Princesse", "Mme Charlemagne", "Bathie Simplexe", "Inspecteur", "El maestro", "M.Diallo R"),
  sexe = factor(c(2,1,1,2,2,1,1,1,1, 2, 2, 2, 2, 1,1,1,1), levels = c(1, 2), labels = c("Masculin", "Feminin")),
  nationalité=factor(c("Sénégalaise","Sénégalaise","Sénégalaise","Sénégalaise","Sénégalaise","Sénégalaise","Sénégalaise","Sénégalaise","Malgache","Camerounaise","Camerounaise","Camerounaise","Camerounaise","Sénégalaise","Sénégalaise","Sénégalaise","Sénégalaise" )),
  
  distance_ensae_maison_en_min = sample(0:100, 17, replace = TRUE),
  age = sample(17:35, 17, replace = TRUE),
  commentaire_sur_l_age=factor(c("Néant","Néant","Néant","Appréhende ses 20 ans","Néant","Néant","Néant","Néant","Néant","Néant","A passé la barre des 20 ans hier HBD","Néant","Néant","Néant","Néant","Néant","Néant"))
)
#  Affichons la base de données
View(ma_bdd)
                                 # 2) Créer une matrice à partir des données
ma_matrice <- as.matrix(ma_bdd)
        
                                # 3) Renommer les lcolonnes de la matrice
colnames(ma_matrice) <- c("first_and_last_names", "surname", "sex", "nationality", "home_to_ensae_duration", "age", "comment_about_age")
#  Affichons la base de données
View(ma_matrice)

         # Ajouter une variable à calculer
ma_bdd$Appréciation_distance_ensae_maison_en_min <- ifelse(ma_bdd$distance_ensae_maison_en_min >= 90, "Situation inacceptable à laquelle il faut remédier",
                              ifelse(ma_bdd$distance_ensae_maison_en_min >= 60, "Situation pénible",
                                     ifelse(ma_bdd$distance_ensae_maison_en_min >= 30,"Situation Difficile",
                                            ifelse(ma_bdd$distance_ensae_maison_en_min >= 15,"Situation Facile",
                                                   ifelse(ma_bdd$distance_ensae_maison_en_min >= 0, "Situation très facile","A/N")))))

        # Ajouter une variable de classement
library(dplyr) #pour utiliser la fonction min_rank() pour donner à R la manière dont il doit organiser l'attribution de rang
ma_bdd$classement_maison_la_plus_proche <- min_rank(ma_bdd$distance_ensae_maison_en_min)

        #  Affichons la base de données
View(ma_bdd)

        # Faire des statistiques descriptives: mean, max, quartiles; Histogrammes; Camembert
summary(ma_bdd)
        # Représentations graphiques
# Histogramme de l'âge
hist(ma_bdd$age, main = "Distribution des âges", xlab = "Âge", ylab = "Fréquence")

           # Pour visualiser la répartition des catégories, par exemple, la répartition des sexes.
# Diagramme en circulaire pour la répartition des sexes
pie(table(ma_bdd$sexe), main = "Répartition des sexes", xlab = "Sexe", ylab = "Effectifs",c("skyblue", "lightcoral")

           #Diagramme circulaire (Camembert) : Pour visualiser la proportion des différentes nationalités.
# Diagramme circulaire pour la répartition des nationalités
pie(table(ma_bdd$nationalité), main = "Répartition des nationalités",col=c("darkorange", "lightpink","lightgreen"))

           #Diagramme à boîtes (Boxplot) : Pour visualiser la distribution des données numériques et détecter les valeurs aberrantes.
# Diagramme à boîtes pour la distance entre le domicile et l'école
boxplot(ma_bdd$distance_ensae_maison_en_min, main = "Distance entre le domicile et l'école", ylab = "Minutes",col = c("lightgreen", "lightcoral"))

##Appréciation de la durée du parcours ENSAE-Domicile
barplot(table(ma_bdd$Appréciation_distance_ensae_maison_en_min),
        main = "Distribution de l'Appréciation",
        xlab = "Appréciation",
        ylab = "Fréquence",
        col = c("skyblue", "lightgreen", "lightcoral", "gold", "mediumorchid"))  # Add more colors if needed
#Graphique en barres empilées : Pour visualiser les catégories croisées, par exemple, la répartition des appréciations en fonction du sexe.
# Graphique en barres empilées pour la répartition des appréciations par sexe
barplot(table(ma_bdd$Appréciation_distance_ensae_maison_en_min, ma_bdd$sexe), main = "Répartition des appréciations de la distance_ensae_maison_en_min par sexe", xlab = "Appréciation", ylab = "Nombre de personnes", legend = TRUE, beside = TRUE,col = c("skyblue", "lightcoral"))  # mettre des couleurs 


############################################################## Exercice 2: PROBLEME D'OPTIMISATION
#Trouver un exercice d'optimisation et le faire

#exemple de donnée
#vecteur de coût
c <- c(-8, -4, 0, 0, 0)

#Matrices de restriction
A<-matrix(nrow=3,ncol=5)
A[1,] <- c(5, -2, 1, 0, 0)
A[2,] <- c(8, -2, 0, 1, 0)
A[3,] <- c(8,  1, 0, 0, 1)

# côtés
b<-c(0,1,2)

#solution initiales
B<-matrix(nrow=3,ncol=3)
B[1,] <- c(1, 0, 0)
B[2,] <- c(0, 1, 0)
B[3,] <- c(0, 0, 1)
solIndexes <- c(3,4,5)

#intialisation
simplex <- function(c,A,b,B,solIndexes){
  i = 0
  j = 1
  sum = 0
  max = -1
  min = 1000000
  entryVariable = -1
  exitVariable = -1
  entryVariable.relative = -1
  exitVariable.relative = -1
  cb <- c()
  entryCriterion <- c()
  
  #Etape 1: initialisation
  invB=solve(B)               #inversion de la matrice
  xb <- invB %*% b            #tableau de solution initiale
  for(i in solIndexes){       #tableau 
    cb <- c(cb, c[i])
  }
  cb[is.na(cb)] <- 0
  
  noSolIndexes <- c()         #indexes des variables candidats
  for(i in 1:5){
    if(!i %in% solIndexes){
      noSolIndexes <- c(noSolIndexes,i)
    }
  }
  
  #itération par l'algorithme
  while(TRUE){
    #Étape 2 : critère d'entrée 
    for(i in noSolIndexes){     #on obtient le critère pour décider quelle variable va entrer dans la solution
      ac <- A[,i]
      y  <- invB %*% ac
      
      candidateVariableCost = c[i]
      if(is.na(candidateVariableCost))  candidateVariableCost = 0
      entryCriterion <- c(entryCriterion, cb %*% y - candidateVariableCost)
    }
    
    for(i in entryCriterion){  #maximum (la variable qui va entrer est obtenue)
      if(i<=0){
        sum = sum+1
      }
      else if(i > max){
        max = i
        entryVariable.relative = j
      }
      j = j + 1
    }
    
    if(sum == length(entryCriterion)){ #une solution optimale a été trouvée
      print("[ Optimal solution ]")
      break
    }
    
    entryVariable = noSolIndexes[entryVariable.relative] #l'index de la variable d'entrée est obtenu
    
    
    
    #Étape 3 : critère de sortie
    y <- c()
    sum = 0
    j=1
    y <- invB %*% A[,entryVariable]
    for(i in y){
      if(i <= 0){
        sum = sum + 1
      }else if(xb[j]/i < min){
        min = xb[j]/i
        exitVariable.relative = j
      }
      j = j + 1
    }
    
    exitVariable = solIndexes[exitVariable.relative]
    
    
    if(sum == length(A[,entryVariable])){
      return("[ Unbounded problem ]")
    }
    
    
    #Étape 4 : la solution est recalculée
    B[,exitVariable.relative] = A[,entryVariable]
    
    invB=solve(B)               #inverse de la matrice B
    xb <- invB %*% b            #la solution est obtenue
    solIndexes[exitVariable.relative] = entryVariable 
    noSolIndexes[which(noSolIndexes==entryVariable)] = exitVariable
    cb[exitVariable.relative] = c[entryVariable]
    if(is.na(cb[exitVariable.relative]))  cb[exitVariable.relative] = 0
    
    #les variables temporaires sont nettoyées
    i = 0
    j = 1
    sum = 0
    max = -1
    min = 1000000
    entryVariable = -1
    exitVariable = -1
    entryVariable.relative = -1
    exitVariable.relative = -1
    entryCriterion <- c()
  }
  
  #retour des valeurs
  z = cb[i]%*%xb[i]
  return(list("Valeur des variables" = xb, "coûts minimal" = z, "Base" = solIndexes))
}
#vérification
simplex(c,A,b,B,solIndexes)













# AUTREEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE

simplex <- function(c, A, b, B, solIndexes) {
  invB <- solve(B)
  xb <- invB %*% b
  noSolIndexes <- setdiff(1:ncol(A), solIndexes)
  
  while (TRUE) {
    # Étape 2 : critère d'entrée
    entryCriterion <- numeric(length = length(noSolIndexes))
    for (i in seq_along(noSolIndexes)) {
      ac <- A[, noSolIndexes[i]]
      y <- invB %*% ac
      entryCriterion[i] <- t(c[solIndexes]) %*% y - c[noSolIndexes[i]]
    }
    entryVariable.relative <- which.min(entryCriterion)
    entryVariable <- noSolIndexes[entryVariable.relative]
    
    if (entryCriterion[entryVariable.relative] >= 0) {
      cat("[Optimal solution]")
      break
    }
    
    # Étape 3 : critère de sortie
    y <- invB %*% A[, entryVariable]
    exitVariable.relative <- which.max(ifelse(y <= 0, -Inf, xb / y))
    
    if (all(y <= 0)) {
      return("[Unbounded problem]")
    }
    
    # Étape 4 : mise à jour de la base
    exitVariable <- solIndexes[exitVariable.relative]
    B[, exitVariable.relative] <- A[, entryVariable]
    invB <- solve(B)
    xb <- invB %*% b
    solIndexes[exitVariable.relative] <- entryVariable
    noSolIndexes[entryVariable.relative] <- exitVariable
  }
  
  z <- t(c[solIndexes]) %*% xb
  return(list("Valeur des variables" = xb, "coûts minimal" = z, "Base" = solIndexes))
}

# Fonction à minimiser (coûts)
c <- c(8, 2, 0, 0, 0)

# Vérification
resultat <- simplex(c, A, b, B, solIndexes)
resultat

################################################EXPLICATION
# Le résultat dépendra de la fonction objectif que vous essayez de minimiser et des contraintes définies par les matrices de restriction (A) et les vecteurs de côté (b).

#Dans ce cas particulier, la fonction objectif que nous essayons de minimiser est donnée par le vecteur c, où les coefficients sont c(8, 2, 0, 0, 0). Les variables de décision associées à ces coefficients sont les éléments 1 et 2.

#La solution optimale sera trouvée en utilisant l'algorithme simplex pour déplacer le long des contraintes définies par les matrices de restriction (A) jusqu'à ce qu'une solution optimale soit atteinte. Cette solution devrait minimiser la fonction objectif tout en satisfaisant toutes les contraintes.

#Le résultat sera affiché comme une liste, avec les valeurs des variables de décision, le coût minimal et la base optimale. Vous pouvez examiner ces valeurs pour déterminer les valeurs optimales des variables de décision et le coût associé.
