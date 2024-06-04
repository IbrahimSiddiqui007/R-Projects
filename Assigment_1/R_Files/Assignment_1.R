install.packages("tidyverse") #run this is you needed to install R again
install.packages("fastDummies") #for one hot encoding
install.packages("cluster") #clustering and cluster visualization
install.packages("factoextra") #for visualization
install.packages("plotly") #interactive plot
install.packages("pheatmap") #heat-map
install.packages('igraph') #for fviz_dent
install.packages('mclust') #for statistical model
install.packages("dbscan") #dbscan
install.packages("dplyr")
install.packages("ggforce")

library(ggplot2)
library(tidyverse)
library(dplyr) #for data manipulation
library(fastDummies)
library(cluster)
library(factoextra)
library(plotly)
library(pheatmap)
library(igraph)
library(mclust)
library(dbscan)
library(readr)
library(ggthemes)
library(scales)
library(ggforce)

# Loading the dataset
adult <- read.csv("adult.data", header = TRUE, sep = "," ,stringsAsFactors = FALSE,  na.strings = c(" ?") )

View(adult)

# PreProcessing ----

### Viewing the dataset to see the issues with it
head(adult)

summary(adult)

str(adult)

### creating the vector that contains column names
names=c("age", "workclass", "fnlwgt", "education", "education-num", 
        "marital-status", "occupation", "relationship", "race", "sex", 
        "capital-gain", "capital-loss", "hours-per-week", "native-country",
        "Income")

### adding the names to the dataset/frame
colnames(adult) <- names

# Remove NA rows
adult[adult==""]<-NA

adult<-adult[complete.cases(adult),]

View(adult)

## Seperate the data
Numeric <-adult %>% select_if(is.numeric)

View(Numeric)

categorical<-adult %>% select_if(is.character) 

View(categorical)


# PreProcessed Visuals -----
# Age vs Hours per week scatter plot
ggplot(Numeric, aes(x = age, y = `hours-per-week`)) +
  geom_point()

# Relationship that shows how working hours change the older the person gets
ggplot(Numeric, aes(x = age, y = `hours-per-week`)) + 
  geom_smooth(method = "loess")

'''
# Trying to do min max lines
Numeric_min_max <- Numeric %>%
  group_by(age) %>%
  summarise(min_hours = min(`hours-per-week`),
            max_hours = max(`hours-per-week`))

ggplot(data = Numeric, aes(x = age, y = hours_per_week)) +
  geom_point() +
  geom_line() +
  geom_ribbon(data = Numeric_min_max, aes(ymin = min_hours, ymax = max_hours), alpha = 0.1) 
'''


# Clustering -------------------------------------------------------------------

## One hot encoding and Scaling

one_hot_encoded<-dummy_cols(categorical,remove_first_dummy = TRUE)
View(one_hot_encoded)
one_hot_encoded<-one_hot_encoded[,10:100]
View(one_hot_encoded)

DTA<-cbind(Numeric,one_hot_encoded)
View(DTA)

DTA[,1:97] <- scale(DTA[,1:97])
View(DTA)

# Sample creation
Adult_Smpl <- DTA[sample(nrow(DTA), 2000), ]

View(Adult_Smpl)

## K means ---------------------------------------------------------------------

set.seed(333)

fviz_nbclust(Adult_Smpl, kmeans, method="wss")+ labs(subtitle="Elbow Method -wss") # 4
fviz_nbclust(Adult_Smpl, kmeans, method="gap_stat")+ labs(subtitle="Elbow Method - Gap Stat") # 9
fviz_nbclust(Adult_Smpl, kmeans, method="silhouette")+ labs(subtitle="Elbow Method -Silhouette") # 2

QN_Adult_Smpl <- Adult_Smpl[,c(1,6,11,22,31)]

Kcluster1 <-kmeans(QN_Adult_Smpl, centers=2, iter.max=30) 
Kcluster1

'''
Kcluster2 <-kmeans(QN_Adult_Smpl, centers=2, nstart = 10)
Kcluster2
'''

clst <- Kcluster1$cluster

QN_Adult_Smpl <- cbind(QN_Adult_Smpl,Kcluster1=clst)

write.csv(Kcluster1$centers, file = "clusters.csv")

Kclstr <- fviz_cluster(Kcluster1, QN_Adult_Smpl, ellipse.type = "norm", main = "Adult Data")+
  labs(title="K-Means Plot")
ggplotly(Kclstr)


clusters<-paste("clusters",Kcluster1)

View(QN_Adult_Smpl)
View(Kcluster1$cluster)

ggplot(QN_Adult_Smpl, aes(x = `age`, y = `education_ Doctorate`)) +
  geom_point(aes(fill = factor(clst)),   
             shape = 23,                     
             alpha = 0.6,                    
             size = 2) +                     
  labs(title = 'Cluster Distribution: Age vs Education_ Doctorate',
       x = 'Age',
       y = 'Education_ Doctorate')                

ggplot(QN_Adult_Smpl, aes(x = `age`, y = `hours-per-week`)) +
  geom_point(aes(fill = factor(clst)),   
             shape = 23,                     
             alpha = 0.6,                    
             size = 2) +                     
  labs(title = 'Cluster Distribution: Age vs Hours per Week',
       x = 'Age',
       y = 'Hours per Week')   

ggplot(QN_Adult_Smpl, aes(x = `workclass_ State-gov`, y = `hours-per-week`)) +
  geom_point(aes(fill = factor(clst)),   
             shape = 23,                     
             alpha = 0.6,                    
             size = 2) +                      
  labs(title = 'Cluster Distribution: workclass_ State-gov vs hours-per-week',
       x = 'workclass_ State-gov',
       y = 'hours-per-week')   

sos <- Kcluster1$betweenss / Kcluster1$totss
sos

# slhtt <- silhouette(clusters,dist(QN_Adult_Smpl))

## Hierachal Clustering --------------------------------------------------------

set.seed(333)


Hclust_Adult_Smpl <- DTA[sample(nrow(DTA), 30), ]

Hclust_matrix <- data.matrix(Hclust_Adult_Smpl)

pheatmap(Hclust_matrix, 
         main = "Pretty heatmap",
         cellwidth =  15,
         cellheight = 7,
         fontsize = 3,
         display_numbers = TRUE,
         cluster_row = TRUE,
         cluster_col = TRUE,
         cutree_col=10,
         cutree_rows=15)


'''
Hclust_Model_ward.D2 <- hclust(dist(QN_Adult_Smpl),"ward.D2")

Hclusters_ward.D2 <- cutree(Hclust_Model_ward.D2,8)

Hclusters

plot(Hclust_Model_ward.D2)

rect.hclust(Hclust_Model_ward.D2, k = 8, border = "green")

plot(QN_Adult_Smpl, col = Hclusters_ward.D2)

fviz_dend(as.dendrogram(Hclust_Model_ward.D2),k=8, cex=0.6)

fviz_dend(as.dendrogram(Hclust_Model_ward.D2),k=8, cex=0.6,
          type="phylogenic", # rectangle, circular, phylogenic(repel = TRUE)
          phylo_layout="layout_as_tree",repel = TRUE) # layout.auto
'''

Hclust_Model_complete <- hclust(dist(QN_Adult_Smpl),"complete")

Hclusters_complete <- cutree(Hclust_Model_complete,8)

Hclusters

plot(Hclust_Model_complete)

rect.hclust(Hclust_Model_complete, k = 8, border = "green")

plot(QN_Adult_Smpl, col = Hclust_Model_complete)

fviz_dend(as.dendrogram(Hclust_Model_complete),k=8, cex=0.6)

fviz_dend(as.dendrogram(Hclust_Model_complete),k=8, cex=0.6,
          type="phylogenic", # rectangle, circular, phylogenic(repel = TRUE)
          phylo_layout="layout_as_tree",repel = TRUE) # layout.auto

# Model Based ------------------------------------------------------------------
set.seed(333)

Model_based <-Mclust(QN_Adult_Smpl)
plot(Model_based)

# Density Model ----------------------------------------------------------------
set.seed(333)

kNNdistplot(QN_Adult_Smpl,k=2)
abline(h=0.85, col="red", lty=2)


fit_dbscan<-dbscan(QN_Adult_Smpl,eps=0.85,minPts=9)

g<-fviz_cluster(fit_dbscan, data = QN_Adult_Smpl, 
                geom = "point", main = "DBSCAN Clustering")
ggplotly(g)


# Patterns ---------------------------------------------------------------------
set.seed(333)

Kmeans_all <- kmeans(DTA, centers = 10)
one_hot_encoded$Cluster <- as.factor(Kmeans_all$cluster)
Kmeans_visuals <- cbind(adult,cluster=one_hot_encoded$Cluster)
Kmeans_visuals <- na.omit(Kmeans_visuals)

View(Kmeans_visuals)
head(Kmeans_visuals)

# Vis 1
ggplot(Kmeans_visuals, aes(x = `hours-per-week`, y = age, fill = cluster)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.5) +
  theme_minimal() +
  labs(title = "K-means Clustering with Density Shading", x = "Hours per Week", y = "Age") 


# Vis 2
ggplot(Kmeans_visuals, aes(x = `marital-status`, y = age, fill = cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Relationship Between Age and Marital Status", x = "Marital Status", y = "Age") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.8)) +
  scale_fill_brewer(palette = "rickandmorty", guide = FALSE)


# Vis 3
ggplot(Kmeans_visuals, aes(x = race, y = `native-country`, color = factor(cluster))) +
  geom_point() +
  stat_ellipse(aes(fill = factor(cluster)), geom = "polygon", alpha = 0.2, level = 0.95) +
  labs(title = "Scatter Plot of Race and Native Country with Cluster Shading",
       x = "Race",
       y = "Native Country",
       color = "Cluster",
       fill = "Cluster") +
  theme_minimal()

# Vis 4
ggplot(Kmeans_visuals, aes(x = `hours-per-week`, y = occupation, color = cluster)) +
  geom_point() +
  stat_ellipse(aes(fill = factor(cluster)), geom = "polygon", alpha = 0.2, level = 0.95) +
  geom_line(method = "loess", se = FALSE, aes(linetype = factor(cluster))) +
  labs(title = "Scatter Plot of hours per week and occupation",
       x = " hours per week",
       y = "occupation",
       color = "Cluster",
       fill = "Cluster",
       linetype = "Cluster") +
  theme_minimal()
