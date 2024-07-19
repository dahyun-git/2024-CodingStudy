### Week 4: PCA ###

states <- row.names(USArrests)
# observatiosn <= states
# columns <= murder, assault, rape, and urbanpop
states # Alphabetic order

names(USArrests) # column names

apply(USArrests, 2, mean)
# column means
# varies a lot
# assault: very large, murder: small => common sense

apply(USArrests, 2, var)
# coulmn variances
# assualt var is much larger than the others
# implementing PCA, assault would drive the PCA a lot more than the others

# PCA with standardized variables
pr.out <- prcomp(USArrests, scale=TRUE)
# scale=TRUE: standardizing
# no response variable since it is a unsupervised learning method
# no model syntax or prediction

## Taking a look at the result
names(pr.out)
pr.out$center
# these values are the same as the column mean of the raw data

pr.out$scale
# sqrt of the variances of the raw data
# standard deviation in a column-wise manner

pr.out$rotation
# shows the loadings of each principal component
# loading corresponds to the weights associated with each covariate to construct each component
# Murder*(PC1값) + Assault*(PC1값) + UrbanPop*(PC1값) + Rape(PC1값)
# = the first principal component variable

dim(pr.out$x)
# 50 4
# 4: the number of principal compnents
head(pr.out$x)
# pricipal component scores for each observation


## Visualization
biplot(pr.out, scale=0)
# first two PCs of the observations
# scale=0: plot the loadings in addition to the PC scores
# signs are upside down

par(mfrow=c(1, 1))
pr.out$rotation = -pr.out$rotation
pr.out$x = pr.out$x
biplot(pr.out, scale=0)


pr.out$sdev
# the values seem to be keep decreasing
# this is standard deviation of each PC
# the first comp has the largest possible variance that could be achieved from the linear combination of Xs
# and the second comp has the second possible variance\
# on the constrain that it is perpendicular to the previously achieved one
# so, sdev is decreasing (sqrt of variance)


## Calculating the sdev
pr.var <- pr.out$sdev^2
pr.var


## PVE
# we are interested in the proportion of variance explained (PVE) by each comp
pve <- pr.var / sum(pr.var)
pve
# the first comp explains 62%

# plot the PVE
par(mfrow=c(1, 2))
plot(pve,
     xlab="Principal Component",
     ylab="Proportion of Variance Explained",
     ylim=c(0, 1),
     type="b")

# cumulative sum of the pve
plot(cumsum(pve),
     xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained",
     ylim=c(0, 1),
     type="b")