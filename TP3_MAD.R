library(mclust)

X1 <- rnorm(1000,0, 1)
X2 <- rnorm(1000,4, 0.5)

Pi1 <- 1/3


B <- rbinom(1000, 1, Pi1 )

data = c() 
for (i in 1:1000) {
  if (B[i] == 1 ){
    data = c(data, X1[i])
  }
  else {
    data = c(data, X2[i])
  }
}



dens <- density(data)
hist(data, breaks = 50, freq = FALSE )
lines(dens, col = "red", lwd = 2)

res <- kmeans(data, 2)
X1 = X1[res$cluster==1]
X2 = X2[res$cluster==2]

MU_mean_1 = mean(X1)
MU_mean_2 = mean(X2)

Sig_mean_1 = sd(X1)
Sig_mean_2 = sd(X2)

PI1_estim = length(X1) / 1000

# On suppose sig1 = sig2 = ... = sigg egalité des variances à tester

# max L(theta) - (log(n)*nb de paramtre)/ 2

mcl1 <- Mclust(data = data, modelNames = 'E')
mcl2 <- Mclust(data = data, modelNames = 'V')

summary(mcl1)
summary(mcl2)


# Exercice II 

#modele E ou V choisir avec les variances constantes
# MClust choisi les hyper parametres grace au critère précédent et choisis le nombre de cluster

faithful

faith_mcl <- Mclust(data = faithful)
plot(faithful,col=faith_mcl$classification)
summary(faith_mcl)

par(mfrow= c(2,2))
plot(faith_mcl)

faith_mcl$parameters

hcl <- hclust(dist(faithful), method = 'ward.D2') 

model_hclust = cutree(hcl, k = 3)
model_mclust = Mclust(faithful, G= 3)

table(model_hclust, model_mclust$classification)

