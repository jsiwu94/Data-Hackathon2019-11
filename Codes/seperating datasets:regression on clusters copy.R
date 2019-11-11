#seperating the dataframes by cluster

Clustered_data <- colnames(clustering_data)=="segment" = "Cluster"
Clustered_data = clustering_data[2:16]

pca_clustered = Clustered_data[2:11]

clustered_data.pca <- prcomp(pca_clustered, center = TRUE,scale. = TRUE)

summary(clustered_data.pca)

library(ggfortify)
autoplot(prcomp(pca_clustered),colour = clustering_data$segment,legend = TRUE, legendLabs = c("CLuster1","Cluster2","Cluster3","Cluster4"),label = clustering_data$segment,loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4)

library(Rtsne)
y = Rtsne(pca_clustered,pca_scale = T,check_duplicates = FALSE)
plot(y$Y,col = clustering_data$Cluster)

#cluster 1 regression
c1 <- filter(Clustered_data, Cluster == 1)
model = lm(rowavg~art_galleries+dance_clubs+ juice_bars+restaurants+ museums+
             resorts+ parks_picnics +beaches  + religious_institutions, data = c1)              
summary(model)

#cluster 2 regression
c2 <- filter(Clustered_data, Cluster == 2)
model = lm(rowavg~art_galleries+dance_clubs+ juice_bars+restaurants+ museums+
             resorts+ parks_picnics +beaches  + religious_institutions, data = c2)              
summary(model)
           
#cluster 3 regression
c3 <- filter(Clustered_data, Cluster == 3)
model = lm(rowavg~art_galleries+dance_clubs+ juice_bars+restaurants+ museums+
             resorts+ parks_picnics +beaches  + religious_institutions, data = c3)              
summary(model)

#cluster 4 regression
c4 <- filter(Clustered_data, Cluster == 4)
model = lm(rowavg~art_galleries+dance_clubs+ juice_bars+restaurants+ museums+
             resorts+ parks_picnics +beaches  + religious_institutions, data = c4)              
summary(model)