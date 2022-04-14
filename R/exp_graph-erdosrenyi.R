set.seed(1001)
l_net004 <- replicate(100, erdos.renyi.game(20, 0.1, "gnp"), simplify = FALSE)
l_net04 <- replicate(10, erdos.renyi.game(20, 0.4, "gnp"), simplify = FALSE)

lap_004 <- lapply(l_net004, graph.laplacian)
lap_04 <- lapply(l_net04, graph.laplacian)

l_lap <- c(lap_004, lap_04)
lbls <- rep(c(1, 3), c(length(l_net004), length(l_net04)))

cmbs <- combn(1:length(l_lap), 2)
net_dists <- diag(x = 0, length(l_lap))

for (i in seq_len(ncol(cmbs))) {
  ind <- cmbs[, i]
  ind1 <- ind[1]
  ind2 <- ind[2]
  net_dists[ind1, ind2] <- Frobenius(as.matrix(l_lap[[ind1]]), as.matrix(l_lap[[ind2]]))
}
net_dists[lower.tri(net_dists)] <- t(net_dists)[lower.tri(net_dists)]

spec_cols <- list(
  scale_colour_manual(
    values = c(
      "grey",
      "blue",
      "red"
    )
  )
)


net_emb <- embed(net_dists, "mds")

out_ranks <- rank(-lof(apply(net_emb, 2, scale), minPts = nrow(net_emb) * 0.75))
raw_ranks <- rep("", length(out_ranks))
raw_ranks[out_ranks %in% 1:13] <- out_ranks[out_ranks %in% 1:13]

lbls[out_ranks %in% c(11, 10, 13)] <- 2
lbls <- as.factor(lbls)

plt_emb_erdr <-
  plot_emb(net_emb,
           color = lbls,
           labels_off = FALSE,
           labels = raw_ranks,
           size = 0.45, label_size = 0.5) +
  list(xlab(TeX("y_1")), ylab(TeX("y_2"))) +
  spec_cols

plts_net_in <- lapply(
  l_net004[1:6],
  function(net) ggnet2(net, size = 1) + theme_bw() + theme(axis.title = element_blank())
)

plts_net_dis <- lapply(
  l_net004[which(out_ranks %in% c(11, 10, 13))],
  function(net) ggnet2(net, size = 1, color = "blue") + theme_bw() + theme(axis.title = element_blank())
)

plts_net_str <-
  lapply(
    l_net04[1:3],
    function(net) ggnet2(net, size = 1, color = "red") + theme_bw() + theme(axis.title = element_blank())
  )

# plts_graphs <- wrap_plots(c(l_plts_004, l_plts_04), ncol = 3) + plt_emb_erdr

## Further analysis
dist_outs <- which(out_ranks %in% c(11, 10, 13))

max_degree <- sapply(lap_004, function(x) max(diag(x)))
summary(max_degree)
max_degree[dist_outs]

isolated_nodes <- sapply(lap_004, function(x) sum(diag(x == 0)))
summary(isolated_nodes)
isolated_nodes[dist_outs]
