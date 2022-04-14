# fabians shift example: shift as off-manifold

set.seed(1221)
x <- seq(.01, .99, l = 50)
n <- 100
r <- .1
f_m2 <- replicate(n, dbeta(x, runif(1, 0.1, 2), runif(1, 0.1, 2)))
f_m1 <- replicate(n * r, runif(1, -5, 5) + dbeta(x, runif(1, 0.1, 2), runif(1, 0.1, 2)))
f <- cbind(f_m1, f_m2)
d_l2 <- as.matrix(dist(t(f)))
mds <- embed(d_l2, "mds")


# distributional out 81, 104, 61, 101
lbls <- ifelse(11:110 %in% c(81, 104, 61, 101), 2, 1)
lbls <- as.factor(c(rep(3, r * n), lbls))

out_ranks <- rank(-lof(mds, nrow(mds) * 0.75))
raw_ranks <- rep("", length(out_ranks))
raw_ranks[out_ranks %in% 1:10] <- out_ranks[out_ranks %in% 1:10]


plt_funs <- plot_funs_temp(t(f), col = lbls, size = 0.27,
                           alpha = ifelse(lbls == 1, 0.99999999999975, 1),
                           args = rep(seq(0, 1, length.out = ncol(t(f))))) +
  list(xlab("t"), ylab("x(t)")) +
  scale_color_manual(values = c("darkgrey", "blue", "red"))

plt_mds <- plot_emb(mds, color = lbls, labels_off = FALSE,
                    labels = raw_ranks, size = 0.25) +
  list(xlab(TeX("y_1")), ylab(TeX("y_2"))) +
  scale_color_manual(values = c("grey", "blue", "red")) +
  scale_alpha_manual(values = 0.2)


