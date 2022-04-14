# outlier type examples
library(ggplot2)
# FS: needs these files as well: MH: leads to errors on my side
# source("../R/simulate_data.R")
# source("../R/help_funs.R")

set.seed(1)
hm <- hm_dat(49, 1)
# tt <- embed(as.matrix(proxy::dist(hm)), "mds")
# plot_emb(tt, labels = TRUE)

lbls <- ifelse(1:49 %in% c(24, 14), 2, 1)
lbls <- c(lbls, 3)

fun_plt <-
  plot_funs.default(hm, col = as.factor(lbls), args = rep(seq(0, 1, length.out = ncol(hm)), nrow(hm))) +
  scale_color_manual(values = c(scales::alpha("grey", .2), "blue", "red")) +
  scale_x_continuous(name="t", limits=c(0, 1)) +
  ylab("x(t)")
