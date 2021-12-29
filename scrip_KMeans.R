# The idea here is follow the content herein: https://www.kaggle.com/vjchoudhary7/customer-segmentation-tutorial-in-python
# In order to review K-Means techniques of clustering

#Loading data...
mall_dataset <- read.csv(file = "Mall_Customers.csv",header =  TRUE)

# A bit of data wrangling...
install.packages("tidyverse")
library("tidyverse")

mall_dataset <- mall_dataset %>% rename(ID = 1, Genero = 2, Idade = 3, Renda_Anual = 4, Ranking_de_Gastos = 5)

mall_dataset <- mall_dataset %>% mutate(Genero = recode(Genero, "Female" = "Feminino","Male" = "Masculino")
)

rownames(mall_dataset) <- mall_dataset[,1]
mall_dataset <- mall_dataset[,-1]

# Initializing Clustering....

install.packages("dendextend")
install.packages("factoextra")
install.packages("fpc")
install.packages("gridExtra")

library(cluster) #algoritmo de cluster
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(fpc) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange
library(readxl)

# Exploratory Analisys...

ggplot(mall_dataset) +
  geom_point(aes(x = mall_dataset$Renda_Anual,
                 y = mall_dataset$Ranking_de_Gastos),
             size = 3)

# Selecting data...
data_new <- select(mall_dataset,Renda_Anual, Ranking_de_Gastos)
data_new_pad <- scale(data_new)

cluster.k3 <- kmeans(data_new_pad, centers = 3)

#Creating new groups
grupo_clientes_kmeans3 <- data.frame(cluster.k3$cluster)

#Updating base...
mall_dataset <- cbind(mall_dataset, grupo_clientes_kmeans3)

ggplot(mall_dataset) +
  geom_point(aes(x = mall_dataset$Renda_Anual,
                 y = mall_dataset$Ranking_de_Gastos),color = as.factor(cluster.k3$cluster),size = 3)

# Why 3 groups...
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data_new_pad, k, nstart = 10)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Analysing this plot the best choice would 3 or 5 groups...