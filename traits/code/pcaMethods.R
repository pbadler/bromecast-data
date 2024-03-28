# Test out pcaMethods package for Bayesian PCA that allows for imputation of
# missing values

## INTRODUCTION ####
# Load library and example data
library(pcaMethods); library(tidyverse)
data("metaboliteData")
data("metaboliteDataComplete")

# Center the datasets (do this here or default below)
md <- prep(metaboliteData, scale="none", center=TRUE) # This has some missing values
mdC <- prep(metaboliteDataComplete, scale="none", center=TRUE)

# Run SVD pca, PPCA, BPCA, SVDimpute and nipalsPCA on the data, using
# the pca() wrapper function. The result is always a pcaRes object.

# svd = same as standard prcomp() PCA in R -- need complete data
resPCA <- pca(mdC, method="svd", center=FALSE, nPcs=5)
# This gives the same result -- just centering at a different point in the
# process
pca(metaboliteDataComplete, method = "svd", nPcs = 5)

# PPCA = probabilistic PCA (expectation-maximization approach with probabilistic
# model; assumes latent variables and noise are Gaussian; tolerant to 10-15%
# missing values)
resPPCA <- pca(md, method="ppca", center=FALSE, nPcs=5)

# BPCA = Bayesian PCA (EM approach with Bayesian model; tolerant to high amounts
# of missing data; developed specifically for missing value imputation)
resBPCA <- pca(md, method="bpca", center=FALSE, nPcs=5)

# There are a couple of other types too, but I think we want Bayesian PCA

# Q2 = goodness of fit measure for internal cross validation; (removes random
# data points and then predicts them using PCA missing value imputation);
# maximum value (and best) is 1
Q2(resBPCA, md, fold = 10)

# NRMSEP (normalized root mean square error of prediction) normalizes square
# difference between real and estimated values. This should be the error measure
# of choice unless variance scaling was applied -- then use Q2.
errEsti <- kEstimate(md, method = "bpca", evalPcs=1:5, nruncv=1, em="nrmsep")
# Want the errors by PC to be less than 1 (?)

# Visualizing results
slplot(resBPCA)
plotPcs(resBPCA)

# Create ggplot
df <- merge(md, scores(resBPCA), by = 0)
# by = 0 makes sure there aren't repeats here
ggplot(df, aes(x = PC1, y = PC2)) +
  geom_point()

## MISSING DATA IMPUTATION ####

# See how many missing data points there are
sum(is.na(md)) / (nrow(md)*ncol(md)) # ~5% missing data

# Re-run Bayesian PCA (same as above)
resBPCA <- pca(md, method="bpca", center=FALSE, nPcs=5)
# Get all values including imputed values
imputed <- completeObs(resBPCA)
