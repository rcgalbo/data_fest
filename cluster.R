library(data.table)
library(dplyr)
library(ggfortify)
library(plotly)
library(fpc)
library(cluster)
library(mclust)


#**load in features for clusertering
r_user = fread("data_fest/return_users.csv", na.strings = c('NA',NA,"NULL",NULL))
ruser = data.frame(r_user)
colMeans(is.na(ruser))

#lets explore better feature creation!
ruser['lg_ab_dif'] = abs(ruser$diff_reg)
ruser['cit_st'] = ruser$num_state*ruser$num_cities

#check correaltion
library(corrplot)
cor_features= cor(ruser[,3:19])
corrplot(cor_features)
#alot of correaltion


#remove small purchase users
smp = filter(ruser, num_purchase >= 50)

cor_smp= cor(smp[3:19])
corrplot(cor_smp)
#eval
names(sort(colSums(cor_smp)+rowSums(cor_smp)))

#less_corr
smp_ag = smp[,c(6,8,16,17,13,15)]

#**PCA**
pca = prcomp(smp_ag, scale = T)
c(summary(pca),sum(summary(pca)[1]$sdev))

#plot primary comps
autoplot(pca,colour='red')
#relationships arise, but no tight cluster

#plot 3 components
plot_ly(x = pca$x[,'PC1'] , y = pca$x[,'PC2']
                      , z = pca$x[,'PC3'], type = "scatter3d", mode = "markers")

# pc_res = data.frame(pca$x[,c('PC1','PC2','PC3')])
# pc_res = data.frame(pc_res,fit1$cluster)
# pc_res = data.frame(pc_res,groups)
# write.csv(pc_res,'data_fest/pca_res.csv')

#fit k-means with n clusters



#scale data
smp = scale(smp)
smp_ag = scale(smp_ag)

#*kmeans*
fit1 = kmeans(smp_ag, 2)

aggregate(smp,by=list(fit1$cluster),FUN=mean)
smp_ag = data.frame(smp_ag, fit1$cluster)
smp = data.frame(smp, fit1$cluster)

plotcluster(smp_ag, fit1$cluster)
clusplot(smp_ag, fit1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#*hierar ag*
d <- dist(smp_ag, method = "euclidean") # distance matrix
fit2 <- hclust(d, method="ward") 

plot(fit2)
groups <- cutree(fit2, k=2)
rect.hclust(fit2, k=2, border="red")


clusplot(smp_ag, pred$dec_val, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

#*mod_based*
fit3 <- Mclust(smp[3:19])
plot(fit3) # plot results 
summary(fit3)

#compare clust_makers
cluster.stats(d, fit1$cluster, fit2$cluster)


#create ensemble prediction
pred = data.frame(fit1$cluster,groups)

#randomly break ties
smp$dec_val[smp$dec_val == 3] = sample(x = c(1,2),size = 1)

#final predictions by id
pred = smp[,c(2,20)]

ruser['dec_val'] = 3

fuser = ruser[,c(2,18)]

pred_mult = merge(fuser, pred, by = 'purch_party_lkup_id', all.y = TRUE)
