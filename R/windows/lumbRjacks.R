#########################################################################
# FUNCTIONS FOR FOREST CLUSTERING METHODS...
# ... RETURN PROXIMITIY AND VARIABLE IMPORTANCE
# ... METHODS INCLUDE:
#     - RANDOM FOREST CLUSTERING
#     - REGULARIZED RANDOM FOREST CLUSTERING
#     - CONDITIONAL INFERENCE FOREST CLUSTERING
#
# THE CODE HERE IS BUILT ON THE WORK OF HORVATH AND SHI, SPECIFICALLY REFEREING TO:
#     - collect.garbage
#     - RFdist
#
# PLEASE SEE ORIGINAL PAPER AND AND WEBSITE FOR MORE DETAILS:
#     - https://horvath.genetics.ucla.edu/rfclustering/
#     - Tao Shi and Steve Horvath (2006) Unsupervised Learning with Random Forest
#       Predictors. Journal of Computational and Graphical Statistics. Volume 15,
#       Number 1, March 2006, pp. 118-138(21)
#
# THE REMAINING CODE IS ADAPTED FOR DIFFERENT FOREST TYPES, LAWSON TAYLOR 28/11/2018
#
#########################################################################
library(randomForest); library(RRF); library(party); library(caret); library(doParallel)

# WINDOWS VERSION

###############################################################################################
# HELPER FUNCTIONS

# create fake data by sampling independently from the marginal feature distributions
createSyntheticData <- function(dat) {
    sample1 <- function(X)   { sample(X, replace=T) }
    g1      <- function(dat) { lapply(dat, sample1) }
    nrow1 <- dim(dat)[[1]];
    yy <- rep(c(1,2),c(nrow1,nrow1));
    tmp_df <- data.frame(g1(dat))
    names(tmp_df) <- names(dat)
    data.frame(cbind(yy,rbind(dat,tmp_df)))
}

# ensure no zero distances
cleandist <- function(x) {
    x1 <- as.dist(x)
    x1[x1<=0] <- 0.0000000001
    as.matrix(x1)
}

# clean up rubbish
if (exists("collect.garbage") ) rm(collect.garbage)
collect.garbage <- function(){
    ## The following function collects garbage until the memory is clean.
    ## Usage: 1. immediately call this function after you call a function or
    ##        2. rm()
    while (gc()[2,4] != gc()[2,4]){}
}

# close parallel backend
unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
}

###############################################################################################
# SPECIFIC FOREST CLUSTERING METHODS - ONE FOREST ONLY - RETURNS LIST OF PROXIMITY AND VAR.IMP
if (exists("forestDist") ) rm(forestDist)
forestDist <- function(forest.data, 
                       mtry_=ceiling(sqrt(ncol(forest.data))), 
                       no.trees=1000, 
                       forest.type="CF",
                       cond.=FALSE,
                       varImp.babyForest=TRUE,
                       varImp.babyN=500, 
                       varImp.babyTrees=500) {

    # constants and initialize placeholder matrices/vectors
    rowN <- dim(forest.data)[[1]]
    colN <- dim(forest.data)[[2]]
    rep_ <- rep(99999,2*rowN)

    # create fake data to classify against
    index_ <- sample(c(1:(2*rowN)))
    rep_[index_] <-  c(1:(2*rowN))
    fake.data <- createSyntheticData(forest.data)[index_,]
    yy <- fake.data[,1]

    # create forest and get proximity + variable importance
    if (forest.type == "RF") {
        forest <- randomForest(x = fake.data[,-1],
                               y = factor(yy),
                               ntree=no.trees,
                               oob.prox=T,
                               proximity=T,
                               replace=F,
                               do.trace=F,
                               mtry=mtry_,
                               importance=T,
                               keep.forest=F)

        collect.garbage()
        
        # importance comes as vector, make named vector
        F.importance.tmp <- randomForest::importance(forest, type=1, scale=F)
        F.importance <- as.numeric(F.importance.tmp); names(F.importance) <- rownames(F.importance.tmp)
            
        F.prox.real <- forest$proximity[rep_,rep_]
        F.prox <- F.prox.real[c(1:rowN), c(1:rowN)]
        F.oob.err <- forest$err.rate[no.trees]

    } else if (forest.type == "RRF") {
        forest <- RRF(x = fake.data[,-1],
                      y = factor(yy),
                      ntree=no.trees,
                      oob.prox=T,
                      proximity=T,
                      replace=F,
                      do.trace=F,
                      mtry=mtry_,
                      importance=T,
                      keep.forest=F)

        collect.garbage()
        
        # importance comes as vector, make named vector
        F.importance.tmp <- RRF::importance(forest, type=1, scale=F)
        F.importance <- as.numeric(F.importance.tmp); names(F.importance) <- rownames(F.importance.tmp)

        F.prox.fake <- forest$proximity[rep_,rep_]
        F.prox <- F.prox.fake[c(1:rowN), c(1:rowN)]
        F.oob.err <- forest$err.rate[no.trees]

    } else if (forest.type == "CF") {
        forest <- cforest(factor(yy)~.,
                          data=fake.data,
                          controls=cforest_unbiased(ntree=no.trees, mtry=mtry_))

        collect.garbage()
        F.prox.fake <- proximity(forest)[rep_,rep_]
        F.prox <- F.prox.fake[c(1:rowN), c(1:rowN)]
        F.oob.err <- 1 - as.numeric(caret::cforestStats(forest)[1])

        # for CIF and cond.varImp 
        # if nrow of real data > than varImp.size:
        # do on a random sample of real+fake, size = varImp.size
        # build smaller forest and get variable importance (conditional if cond.)
        
        if (!varImp.babyForest) {
            F.importance <- varimp(forest, conditional=cond.)
        } else {
            small.fake.data <- createSyntheticData(forest.data[sample(nrow(forest.data), varImp.babyN), ])
            
            small.forest <- cforest(factor(yy)~.,
                                    data=small.fake.data,
                                    controls=cforest_unbiased(ntree=varImp.babyTrees, mtry=mtry_))
            
            collect.garbage()
            F.importance <- varimp(small.forest, conditional=cond.)
        }
    }
    # remove forest
    rm(forest)
    
    # return the goods
    return(list(importance=F.importance,
                distance=cleandist(sqrt(1-F.prox)),
                oob.err=F.oob.err))
}

###############################################################################################
# SPECIFIC FOREST CLUSTERING METHODS - ONE FOREST ONLY - RETURNS LIST OF PROXIMITY AND VAR.IMP
if (exists("lumbRclust") ) rm(lumbRclust)
lumbRclust <- function(forest.data,
                       no.forests = 100, 
                       mtry_ = ceiling(sqrt(ncol(forest.data))),
                       no.trees = 1000,
                       forest.type = "CF",
                       cond.=FALSE,
                       no.cores=1,
                       varImp.babyForest=TRUE, 
                       varImp.babyN=500, 
                       varImp.babyTrees=500) {

    # placeholders, constants and required forest package
    rowN <- dim(forest.data)[[1]]
    colN <- dim(forest.data)[[2]]
    F.dist <- matrix(0, nrow=rowN, ncol=rowN)
    F.importance <- vector(mode = "numeric", length = colN)
    F.oob.err <- 0

    forest.packages <- list("RF"="randomForest", "RRF"="RRF", "CF"=c("party", "caret"))
    required.package <- forest.packages[[forest.type]]

    # detect the cores, min is 1
    # and give warnings
    cores.to.use = max(1, min(no.cores, detectCores()))
    
    if (no.cores < 1) message(paste("Number of cores selected", no.cores, "to small, no.cores set to 1"))
    if (no.cores > detectCores()) message(paste("Number of cores selected", no.cores, "is greater than the", detectCores(), "available, no.cores set to", detectCores()))
        
    # set up cluster and run in parallel, save a core = detectCores()-1
    woods <- makeCluster(cores.to.use, type = "PSOCK")
    registerDoParallel(woods)

    init.time <- Sys.time()
    forests.info = foreach(i=1:no.forests, .packages = required.package) %dopar% {
        source("lumbRjacks_v2.R", local = TRUE)
        forestDist(forest.data, 
                   mtry_=mtry_, 
                   no.trees=no.trees, 
                   forest.type=forest.type,
                   cond.=cond.,
                   varImp.babyForest=varImp.babyForest,
                   varImp.babyN=varImp.babyN, 
                   varImp.babyTrees=varImp.babyTrees)
    }
    execution.time <- Sys.time() - init.time
    stopCluster(woods)
    unregister()

    # aggregate distance and variable importance from forests = woods
    for (j in 1:no.forests){
        F.importance <- F.importance + forests.info[[j]]$importance / no.forests
        F.dist <- F.dist + forests.info[[j]]$distance / no.forests
        F.oob.err <- F.oob.err + forests.info[[j]]$oob.err / no.forests
    }

    # return the averaged results, averaged over forests already
    return(list(importance=F.importance,
                distance=as.dist(F.dist),
                oob.err=F.oob.err,
                execution.time=as.numeric(execution.time),
                no.forests=no.forests, 
                mtry_=mtry_,
                no.trees=no.trees,
                forest.type=forest.type,
                cond.=cond.,
                varImp.babyForest=varImp.babyForest, 
                varImp.babyN=varImp.babyN, 
                varImp.babyTrees=varImp.babyTrees
                ))
}
