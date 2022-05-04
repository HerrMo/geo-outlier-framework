library(data.table)
library(fda)
library(fda.usc)
library(dbscan)
library(ggplot2)
library(igraph)
library(StatPerMeCo)
library(pROC)
library(cowplot)
library(latex2exp)
# library(classiFunc)
library(kableExtra)
library(patchwork)
library(GGally)

# devtools::load_all("~/projects/manifun")

reps <- 1:50
ratios <- c(0.01, 0.025, 0.05, 0.1)
dists <- function(x) as.matrix(proxy::dist(x))
frank <- function(x, ndim = 2, k = 0.75 * nrow(extract_points(x))) {rank(-lof(extract_points(x, ndim = ndim), minPts = k))}
label_size <- 3
