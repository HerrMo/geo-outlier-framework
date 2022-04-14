# simple simulated example shape-based structural outliers

set.seed(1221)
n <- 100
r <- .05 # 0.1 if hm_dat
f_hm <- hm_dat_ext(n, n * r)
# f_hm <- hm_dat(n, n * r)
d_l2_hm <- as.matrix(dist(f_hm))
emb_hm <- embed(d_l2_hm, "mds")

lbls <- ifelse(1:100 %in% c(44, 5, 10), 2, 1)
lbls <- as.factor(c(lbls, rep(3, r * n), rep(4, r * n)))

out_ranks <- rank(-lof(apply(emb_hm, 2, scale), minPts = nrow(emb_hm) * 0.75))
raw_ranks <- rep("", length(out_ranks))
raw_ranks[out_ranks %in% 1:10] <- out_ranks[out_ranks %in% 1:10]

spec_cols <- list(
  scale_colour_manual(
    values = c(
      "grey",
      "blue",
      "red",
      "black"
    )
  )
)

plt_funs_hm <- plot_funs_temp(f_hm, col = lbls, size = 0.4, args = rep(seq(0, 1, length.out = ncol(f_hm))),
                         alpha = ifelse(lbls == 1, 1, 1)) +
  list(xlab("t"), ylab("x(t)")) +
  spec_cols
plt_emb_hm <- plot_emb(emb_hm, color = lbls, labels_off = FALSE,
                       labels = raw_ranks, size = 0.45, label_size = 0.5) +
  list(xlab(TeX("y_1")), ylab(TeX("y_2"))) +
  spec_cols
