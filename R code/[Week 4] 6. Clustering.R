### Week 4: Clustering ###

## K-means Clustering ##


# Generating a simulation data matrix
set.seed(2)
x <- matrix(rnorm(50*2), ncol=2)
dim(x) # 50 by 2: 2-variate data with 50 points


# making a distinction
x[1:25, 1] <- x[1:25, 1]+3
x[1:25, 2] <- x[1:25, 2]-4


# Clustering
km.out <- kmeans(x, 2, nstart=20)
# 2: predetermined number of K
# why 2? 데이터를 둘로 나뉘게 만들었으니까
# nstart=20: the number of random start
# 항상 같은 결과 X: depend on the starting value
# try a lot of random starts at the beginning
# compare the results and choose the one that yields the best

km.out$cluster
# shows the labels of the clusters that k-means has figured out
# perfect job: 25개씩 2개 클러스터


# Visualization
par(mfrow=c(1, 2))
plot(x, col=(km.out$cluster+1),
     main="K-Means Clustering Results with K=2",
     xlab="", ylab="", pch=20, cex=2)
# colored each observation depending on the clustering results
# different colors = different clusters
# well-separated


## Trying a slightly different thing
# in reality, we do not know how many clusters exist
# we would make a guess about it
set.seed(4)
km.out <- kmeans(x, 3, nstart=20)
km.out
# yielded three centers
# not perfectly separating
# one of the clusters at least has been mixed up with the third cluster
plot(x, col=(km.out$cluster+1),
     main="K-Means Clustering Results with K=3",
     xlab="", ylab="", pch=20, cex=2)
# less-well separated compared to K=2 case


## Verifying that having the larger number of random starts is actually beneficial in clustering
# k-means clustering result dependson where it started
# so, try a lot of number of random starts and choose the best result
# what would we mean by saying the "best"?
# we want to find the cluster that minimizes the objective function
# where it was the total Within Cluster Distance
# to compare the results, take a look at that measure
set.seed(4)
km.out <- kmeans(x, 3, nstart=1)
km.out$tot.withinss # 104.3319 거리, 즉 유사도

km.out <- kmeans(x, 3, nstart=20)
km.out$tot.withinss # 97.97927: smaller
# reasonable: 1 => not be able to see the results from the other cases
# 20 => have 20 outputs => the one we have as a final result is what is chosen among the twenty outputs
# so, it is anticipated that we observe smaller, and equivalently the better within-cluster-closeness measure for larger nstart
# it is strongly recommended to perform k-means with a larger number of nstarts, say, 20 or 50
