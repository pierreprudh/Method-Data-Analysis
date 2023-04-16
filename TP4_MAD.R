library(MASS)
library(FactoMineR)
library(corrplot)


data(crabs)
crabsquant <- crabs[,4:8]


#ACP = Division des données en plusieurs composantes sur des sous-espaces (plans orthogonaux), tout en gardant l'information 

plot(crabsquant)
PCA(crabsquant)

prcomp(crabsquant)


# 95% sur le premier axe car variables corrélées donc on divise les données par cette variable pour les décorrélées. 
# X1 = A*X2 + B, soit X2 la variable la plus corrélée
# plot et voir la variable la plus corrélées ici CL 

summary(crabs)

corrplot(cor(crabsquant))
#celle qui a les plus grandes valeurs partout 

crabsquant_decorel <- crabsquant/crabsquant$CL

crabsquant_decorel <- crabsquant_decorel[,-3]

PCA_crabs <- PCA(crabsquant_decorel)



plot(PCA_crabs$ind$coord[,1:2], col = as.numeric(crabs$sp), pch = as.numeric(crabs$sex))
legend(x = 'bottomright', legend=c("Male", "Femelle"), pch = c(0,2))
legend(x = 'bottomleft', legend=c('Orange', 'Blue'), col = c("Red", "Black") ,pch= 1)

# Cercle de Corrélation  



# B/M |  O/M
#     |
#------------
#     |
# B/F |  O/F

# matrice A -> Aij = dissimilartié entre xj et xi

d <-  read.table("neighbor_globin.dat")
d[d<0]  #dissimilarité tjr positive
sum(as.matrix(d[,-1]) - (t(as.matrix(d[,-1]))))
diag(as.matrix(d[,-1])) #distancec plutot que similarité 

colnames(d) <- c("globine", d$V1)
head(d)

image(as.matrix(d[,-1]))
#donne un avis sur cb de groupe on va avoir ici globalement 3 groupes


delta <- as.matrix(d[,-1])**2

# J = I - 1/n * 1(n,n)

n <- nrow(delta)
J <- diag(n) - 1/n * matrix(1, n , n)

B <- -0.5 * J %*% delta %*%  J

tmp <- eigen(B)
#Décomposition Spectrale 


J <- tmp$vectors
A <- diag(tmp$values)
plot(tmp$values)

barplot(diag(A))
#matrice distance valeur à 0 

#on garde 3 valeurs propres (visuellement on en voit 3 qui sortent du lot) -> réduire la dimension 
Valeurs_propres <- c(A[1,1], A[2,2], A[3,3])
Valeurs_propres

m = 3  #nbr de valeurs propres
X <- U[,1:m] %*% (A[1:m,1:m]**1/2)#projection des individus sur des axes



#---------------------------------------------------------------
library(mclust)
X <- rnorm(100, 0,0.5)
Y <- rnorm(100, 3, 0.5)

X = seq(0,40)

X_poiss <- 0.4*dnorm(X, 0) + 0.6*dnorm(X, 3)
mc <- Mclust(X_poiss)

plot(mc, "BIC")
