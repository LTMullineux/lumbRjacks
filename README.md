## Background
Clustering with mixed-type data is a mathematically hard task. Trying to determined suitable metrics that are able to place pair-wise distance between two objects given their features of varying data type, such as the [Gower Index](https://www.jstor.org/stable/2528823?seq=1#metadata_info_tab_contents) provide some progress but lack rigor, and have an disconcerting level of user input.

To arrive at a distance matrix for data that is mixed-type data and is empirically determined, lumbRjacks chooses the avenue of random forests and their siblings. In the supervised setting forest classfiers have many desirable attributes that translate well to the unsupervised setting. Building on the work of [Shi and Horvath 2012](https://amstat.tandfonline.com/doi/abs/10.1198/106186006X94072#.XEih_1z7Q2w) lumbRjacks allows clustering to be performed by the most popular forest classifiers currently implemented in R:

- Random Forest (RF), from the [randomForest](https://link.springer.com/article/10.1023/A:1010933404324) package, only gives reliable variable importance when all features are numerical and features are relatively uncorrelated.
- Regularized Random Forest (RRF), from the [RRF](https://ieeexplore.ieee.org/abstract/document/6252640) package, only gives reliable variable importance when all features are numerical.
- Conditional Inference Forest (CF), from the [party](https://www.tandfonline.com/doi/abs/10.1198/106186006X133933) package and ideal for mixed-type data as the variable importance remains unbiased between features.

## Data Preparation
To ensure that the data you provide to lumbRclust, the main function within lumbRjacks, is correctly encode with numeric/integer/factor/logical/ordinal features it is recommended prepare you data in R first:
- read in the csv or what ever data object that you data is stored in.
- transform it to a dataframe
- ensure that all the features are encoded as the correct data type, i.e. careful that categorical features ARE factors.
- save the object down as a .RData objects so that the data-type is known for sure.

## Usage

lumbRclust is a script built on top of existing R packages, so all must be preinstalled before use and are are freely available via CRAN:
- [randomForest](https://cran.r-project.org/web/packages/randomForest/)
- [RRF](https://cran.r-project.org/web/packages/RRF/)
- [party](https://cran.r-project.org/web/packages/party/)
- [caret](https://cran.r-project.org/web/packages/caret/)
- [doParallel](https://cran.r-project.org/web/packages/doParallel/)

A basic example with Iris, with full script available [here]():

```r
#######
# preprocessing step just for fun
iris.data = iris[,1:4]

# check the features are the correct data-type
str(iris.data)

# all good! save down as .RData object
saveRDS(iris.data, "somespecialplace/iris_clean.RData")


#######
# The fun bit, read in clean and checked data and use lumbRclust to get the pair-wise distance between data points using a forest classifier
source("lumbRjacks.R")
dat = readRDS("somespecialplace/iris_clean.RData")

# constants
no.cores = 10
f.type = "CF"
cond_ = TRUE
no.forests = 100
mtry_= 2
no.trees = 200
varImp.babyForest = FALSE

forest = lumbRclust(forest.data=dat,
		            no.forests = no.forests,
		            mtry_ = mtry_,
		            no.trees = no.trees,
		            forest.type = f.type,
		            cond.=cond_,
		            no.cores=no.cores,
		            varImp.babyForest=varImp.babyForest)

```

A number of parameters need to be set for lumbClust to do its magic:
- forest.data ->  data to be clustered (observations x features)
- no.forests -> number of forests build over which the distance, variable importance and oob error are averaged
- mtry_ -> number of features randomly selected for each tree within each forest
- no.trees -> number of tree in each forest
- forest.type -> type of underlying forest classifier to use, one of RF / RRF / CF
- cond. -> boolean if conditional variable importance should be used when forest.type="CF"
- no.cores -> number of cores to use to run forests in parallel, the bigger the better, the faster
- varImp.babyForest -> size of baby forest to build to calculate variable importance when using CF, when observations >> 500 the calculation can be very slow. By using a smaller forest the absolute measure many not be accurate, but the rank of the feature importances will be.
- varImp.babyN -> number of randomly sample observations to be used in the baby forest
- varImp.babyTrees -> number of trees in the baby forest

Once the forest distance has been returned, any of the traditional clustering methods are good to use. It is recommended however to use [consensus clustering](https://bioconductor.org/packages/release/bioc/html/ConsensusClusterPlus.html):

```r
library(ConsensusClusterPlus)

iris.cc = ConsensusClusterPlus(forest$distance,
                               maxK = 6,
                               reps = 1000,
                               pItem = 0.9,
                               clusterAlg = "hc",
                               distance = "euclidean",
                               innerLinkage = "ward.D2",
                               finalLinkage = "ward.D2")

# check the plots to decide on the number of clusters K
K = 3
pred.labels = iris.cc[[K]]$consensusClass

# confusion matrix of results
table(iris$Species, pred.labels)

```
