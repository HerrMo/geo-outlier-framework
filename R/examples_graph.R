library(igraph)
library(StatPerMeCo)
library(cowplot)
library(GGally)

set.seed(5)
l_net004 <- replicate(100, erdos.renyi.game(20, 0.1, "gnp"), simplify = FALSE)
net04 <- erdos.renyi.game(20, 0.4, "gnp")

plts_graph <- list(
  ggnet2(l_net004[[1]], size = 2) + theme_bw() + theme(axis.title = element_blank()),
  ggnet2(l_net004[[2]], size = 2) + theme_bw() + theme(axis.title = element_blank()),
  ggnet2(l_net004[[3]], size = 2) + theme_bw() + theme(axis.title = element_blank()),
  ggnet2(net04, size = 2, color = "red") + theme_bw() + theme(axis.title = element_blank())
)

graph_plt <- plot_grid(plotlist = plts_graph)

# embeddings
# lap_004 <- lapply(l_0.04, graph.laplacian)
# lap_04 <- graph.laplacian(net04)
#
#
# l_lap <- c(lap_004, list(lap_04))
#
# cmbs <- combn(1:101, 2)
# net_dists <- diag(x = 0, 101)
#
# for (i in seq_len(ncol(cmbs))) {
#   ind <- cmbs[, i]
#   ind1 <- ind[1]
#   ind2 <- ind[2]
#   net_dists[ind1, ind2] <- Frobenius(as.matrix(l_lap[[ind1]]), as.matrix(l_lap[[ind2]]))
# }
# net_dists[lower.tri(net_dists)] <- t(net_dists)[lower.tri(net_dists)]
# net_dists
#
# net_emb <- embed(net_dists, "mds")
# plot_emb(net_emb, color = c(rep(0, 100), 1))
