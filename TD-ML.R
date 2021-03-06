#TD ML
#pour charger la lib
library(kerlab)
#pour charger les donn�es 
data(spam)
#visualiser les donn�es
View(spam)
#aide
help(spam)
#information sur les donn�es
#on note qu'on a 
summary(spam)
#on pose une graine qui fera qu'on aura toujours les m�mes lignes si on lance la fonction createDataPartition
#tres important pour s'assurer qu'on travaille bien sur les m�mes donn�es
set.seed(100)
#on fait un tirage au sort de 80% des lignes de nos donn�es
ind_train <- createDataPartition(spam$type,p=0.8,list=F)
#nombre de lignes de ma partition / de mon vecteur de donn�es de training
length(ind_train)
#Pour voir les valeurs au format valeurs discretes
View(ind_train)
#Pour voir les 6 premi�res lignes d'un tableau
head(ind_train)
#on assigne la matrice spam qui contient toutes les lignes qui sont dans ind-train et toutes les colonnes de spam
#dim(Dtrain] donne la dimension du tableau
Dtrain<-spam[ind_train,]; dim(Dtrain)
#Creer la table des indices de test � partir de toutes les donn�es qui sont pas dans Dtrain
Dtest<-spam[-ind_train,]; dim(Dtest)

#On vient donc de cr�er les donn�es d'entrainement et les donn�es de validation, maintenant on va apprendre

#methode est validation croisee et on utilise des blocs de 10 (les plus utilis�s sont 5 ou 10)
param_train<-trainControl(method="cv",number=10)
#on lance la fonction train ,de variable "type" avec la m�thode "glm"
fit_glm<-train(type~.,data=Dtrain,method="glm",trControl=param_train)
#d�tail des resultats, le taux pour chaque fold, avec le champ resample
#Fold0x c'est un bloc
#la machine a entrain� l'algo avec une m�thode et affiche ici le 1-taux d'erreur (accuracy)
#Donc accuracy = 0,93 --> taux erreur = 0,07 = 7%
#Kappa c'est si on avait fait l'apprentissage avec des fold aleatoires --> on voit bien ici qu'utiliser la methode donne de meilleurs taux de accuracy
fit_glm$resample
#donne le taux moyen de tous les taux d'erreur
fit_glm


#On cree la variable qui contiendra la prediction
pred_glm<-predict(fit_glm,newdata=Dtest)
head(pred_glm)
table(pred_glm)
#Compter le nb de fois o� on se trompe quand on utilise les donn�es de test avec mon algo entrain�
#affiche la matric de confusion
table(predite=pred_glm,observee=Dtest$type)
#la moyenne des erreurs sur la variable type
mean(pred_glm!=Dtest$type)

#Afficher les 6 premieres lignes 
#on observe qu'il y a 1 erreur
head(Dtest$type)
head(pred_glm)


