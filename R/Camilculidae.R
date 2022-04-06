#### TP Camilculidae : taxonomie numérique ####


# A installer :
SciViews::R
install.packages("readxl")
install.packages("FD")
install.packages("stats")
install.packages("pvclust")


# Les variables peuvent :
# Etre binaires : 0 (plésion), 1 ou NA (le plésion peut être NA si le caractère ne le concerne pas)
# Etre qualitatives : utiliser du texte (+ NA si nécessaire)
# Etre ordonnées : 0<1<2 ou petit<moyen<grand (+NA). A convertir en véritable variable ordonnée ! (voir plus loin)
# Etre quantitatives : noter directement la valeur de la mesure (ou NA si nécessaire)

# Une fois la matrice créée, remplacer tous les NA par des cases vides(dans excel, utiliser ctrl + H).

# Une fois tout ceci vérifié, procéder à l'importation de la matrice.

library(readxl) # Importation du package readxl (permet d'importer des fichiers excel)
test <- read_excel("data/mat_cam2.xlsx") # Importation de la matrice de caractères
test <- as.data.frame(unclass(test),stringsAsFactors = TRUE) # Convertir les chaînes de caractère sen facteurs
otu  = test$OTU # On sauvegarde le nom des OTU dans un vecteur appelé "otu"
test = test[,-1] # On supprime la colonne n°1, celle des noms
rownames(test) = otu # On renomme les rangs avec le vecteur "otu"

# Variables ordinales : à convertir manuellement ! Exemple :

test$rhino <- ordered(test$rhino, levels = c("1", "2", "3"))
test$taille_yeux <- ordered(test$taille_yeux, levels = c("1", "2", "3", "4"))
test$long_style <- ordered(test$long_style, levels = c("1", "2", "3", "4", "5"))
test$courb_style <- ordered(test$courb_style, levels = c("1", "2", "3"))
test$taille_deamb <- ordered(test$taille_deamb, levels = c("1", "2", "3"))
test$long_cami <- ordered(test$long_cami, levels = c("1", "2", "3"))
test$taille_halio <- ordered(test$taille_halio, levels = c("1", "2", "3"))
test$long_pedi <- ordered(test$long_pedi, levels = c("1", "2", "3"))


#### MATRICE DE DISTANCE, PHENOGRAMME ####

library(FD) # Importation de la librairie nécessaire
gowdis(test, ord ="podani") -> distmatrix #Matrice de distance avec l'indice de gower


tree = hclust(distmatrix, method = "average")# hclust crée l'arbre. méthode = type de liens
plot(tree) # Afficher l'arbre

# Essayer les différents types de liens.

tree2 = hclust(distmatrix, method = "single")# hclust crée l'arbre. méthode = type de liens
plot(tree) # Afficher l'arbre

tree3 = hclust(distmatrix, method = "complete")# hclust crée l'arbre. méthode = type de liens
plot(tree) # Afficher l'arbre



#### FONCTION ANNEXE ####

GowerDist <- function(x) { # Une fonction spéciale pour utiliser Gower dans pvclust.  A compléter où nécessaire !
  transp =as.data.frame(t(x)) # pvclust utilise la transposée, il faut donc s'adapter
  # malheureusement, la transposition altère la classe de toutes les variables !
  i = 1 # on démarre une boucle pour récupérer les variables binaires
  while (i<= ncol(transp)){
    if(all(transp[,i] %in% list(1,0," 1"," 0")|is.na(transp[,i])==TRUE)){
      transp[,i]=as.numeric((transp[,i]))
    }
    i=i+1
  }
  i=1 # boucle pour les variables quantitatives
  while (i<= ncol(transp)){
    if (any(as.numeric(transp[,i]) > 1,na.rm =TRUE) ==TRUE){
      transp[,i]=as.numeric(transp[,i])
    }
    i=i+1
  }
  i=1 # boucle pour les variables ordonnées, à compléter deux lignes plus bas !
  while (i<= ncol(transp)){
    if (colnames(transp)[i] %in% c("rhino", "taille_yeux", "long_style", "courb_style", "taille_deamb", "long_cami", "taille_halio", "long_pedi")){ # ICI : liste de vos variables ordonnées
      transp[,i]=as.numeric(as.character(transp[,i]))
      transp[,i]=ordered(transp[,i], levels = as.character(min(transp[,i],na.rm = TRUE):max(transp[,i],na.rm = TRUE)))
    }
    i=i+1
  }

  # les variables qualitatives ont été converties en caractères et gowdis les convertira pour nous en facteurs !
  rownames(transp)=otu # on récupère le nom des otus
  y <- gowdis(transp, ord ="podani")# on peut enfin calculer la distance de gower
  y = as.dist(y)
  attr(y, "method") <- "GowerDist"
  return(y)
}






#### BOOTSTRAP ####

# Recréer un arbre avec la fonction pvclust qui permet de bootstraper l'arbre.
library(pvclust) # Nouveau package nécessaire.
boottree = pvclust(t(test), method.dist = GowerDist, method.hclust = "average", nboot =1000) # Travaille sur la transposée t() de notre matrice.
# Indiquer le type de lien et l'indice de dissimilarité. Indiquer avec nboot le nombre de rééchantillonnages.
plot(boottree) # Afficher le résultat
pvrect(boottree, alpha = 0.95, pv = "au")# Encadrer les groupes avec au > 95
pvrect(boottree, alpha = 0.91, pv = "au", border = 4) # Encadrer les groupes à 90 %. Border permet de changer la couleur.
#test2 = as.data.frame(lapply(test,function(x)as.numeric(factor(x)))) # Peut être utile : transformer les labels des variables qualitatives en nombres

boottree

