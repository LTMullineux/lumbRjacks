#######################################
# analysis of iris test of new lumbRjacks script
library(ggplot2)
library(cowplot)
library(ConsensusClusterPlus)

#####################
iris.rf = readRDS("iris_rf_forest_test.RData")
iris.rrf = readRDS("iris_rrf_forest_test.RData")
iris.cfuncond = readRDS("iris_cfuncond_forest_test.RData")
iris.cfcond = readRDS("iris_cfcond_forest_test.RData")
#####################

#####################
# consensus clustering
iris.rf.cc = ConsensusClusterPlus(iris.rf$distance,
                                  maxK = 6,
                                  reps = 1000,
                                  pItem = 0.9,
                                  clusterAlg = "hc",
                                  distance = "euclidean",
                                  innerLinkage = "ward.D2",
                                  finalLinkage = "ward.D2")

iris.rrf.cc = ConsensusClusterPlus(iris.rrf$distance,
                                   maxK = 6,
                                   reps = 1000,
                                   pItem = 0.9,
                                   clusterAlg = "hc",
                                   distance = "euclidean",
                                   innerLinkage = "ward.D2",
                                   finalLinkage = "ward.D2")

iris.cfuncond.cc = ConsensusClusterPlus(iris.cfuncond$distance,
                                        maxK = 6,
                                        reps = 1000,
                                        pItem = 0.9,
                                        clusterAlg = "hc",
                                        distance = "euclidean",
                                        innerLinkage = "ward.D2",
                                        finalLinkage = "ward.D2")

iris.cfcond.cc = ConsensusClusterPlus(iris.cfcond$distance,
                                      maxK = 6,
                                      reps = 1000,
                                      pItem = 0.9,
                                      clusterAlg = "hc",
                                      distance = "euclidean",
                                      innerLinkage = "ward.D2",
                                      finalLinkage = "ward.D2")

# k chosen from consensusclustering
iris.rf.k = 3
iris.rrf.k = 3
iris.cfuncond.k = 3
iris.cfcond.k = 3

# confusion matrix
table(iris$Species, iris.rf.cc[[iris.rf.k]]$consensusClass)
table(iris$Species, iris.rrf.cc[[iris.rrf.k]]$consensusClass)
table(iris$Species, iris.cfuncond.cc[[iris.cfuncond.k]]$consensusClass)
table(iris$Species, iris.cfcond.cc[[iris.cfcond.k]]$consensusClass)

# make plots of iris
p = list()
plot.titles = c("RF", "RRF", "CFuncond", "CFcond")
plot.labels = list(iris.rf.cc[[iris.rf.k]]$consensusClass,
                   iris.rrf.cc[[iris.rrf.k]]$consensusClass,
                   iris.cfuncond.cc[[iris.cfuncond.k]]$consensusClass,
                   iris.cfcond.cc[[iris.cfcond.k]]$consensusClass)

for (i in 1:4) {
    # data
    plot.dat = data.frame(iris,
                          true.label = factor(ifelse(iris$Species == "setosa", 1,
                                                     ifelse(iris$Species == "versicolor", 2, 3))),
                          pred.label = factor(plot.labels[[i]]))
    
    plot.dat$miscluster = plot.dat$true.label != plot.dat$pred.label
    
    # title
    title = ggdraw() + draw_label(paste("Iris for CLustering \n with", plot.titles[i]), fontface='bold')
    
    # individual plots
    p1 = ggplot(plot.dat, aes(x=Sepal.Length, y=Sepal.Width, 
                              fill=pred.label, col=true.label)) + 
        geom_point(shape=21, size=3) +
        geom_point(data = subset(plot.dat, miscluster==TRUE),
                   shape=1, size = 5, col="black")
    
    p2 = ggplot(plot.dat, aes(x=Sepal.Length, y=Petal.Length, 
                              fill=pred.label, col=true.label)) + 
        geom_point(shape=21, size=3) +
        geom_point(data = subset(plot.dat, miscluster==TRUE),
                   shape=1, size = 5, col="black")
    
    p3 = ggplot(plot.dat, aes(x=Sepal.Width, y=Petal.Length, 
                              fill=pred.label, col=true.label)) + 
        geom_point(shape=21, size=3) +
        geom_point(data = subset(plot.dat, miscluster==TRUE),
                   shape=1, size = 5, col="black")
    
    # grid of plots
    p[[i]] = plot_grid(title, p1, p2, p3, ncol = 2, nrow = 2)
}

pdf("iris_forestClust_results.pdf", paper="a4r", width = 18, height=10); print(p); dev.off()



