# create abstract graphic

load(here::here("data/coil20.RData"))
source(here::here("R/simulate_data.R"))
source(here::here("R/examples_funs.R"))

emb_funs <- embed(as.matrix(proxy::dist(hm)), "mds")
out_ranks_funs <- frank(emb_funs, k = 6)
raw_ranks <- rep("", length(out_ranks_funs))
raw_ranks[out_ranks_funs <= 3] <- out_ranks_funs[out_ranks_funs <= 3]

lbls <- ifelse(1:49 %in% c(24, 14), 2, 1)
lbls <- c(lbls, 3)

plt_emb_funs <-
  plot_emb(emb_funs, color = as.factor(lbls), labels_off = FALSE, labels = raw_ranks) +
  list(xlab(TeX("y_1")), ylab(TeX("y_2"))) +
  scale_color_manual(values = c("grey", "blue", "red")) +
  ggtitle("")


source("R/exp_image-coil.R")
source("R/exp_graph-erdosrenyi.R")
plt_graphs <- ((wrap_plots(c(plts_net_in, plts_net_dis, plts_net_str), ncol = 3))) & plot_annotation(title = "B.1")

plt_theme <- theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "None")

plts_dat <- plot_grid(
  plotlist = list(
    fun_plt + plt_theme + ggtitle("A.1"),
    plt_graphs,
    plt_pic_coil + ggtitle("C.1")
  ),
  nrow = 1)

# plts_dat <- ((fun_plt + plt_theme) + plt_graphs + plt_pic_coil) & plot_annotation()

plt_rect <- ggplot() +
  geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1.1, color = "darkgrey", fill = NA) +
  theme(panel.border = element_rect(linetype = 2))

plt_dat <- plt_rect + inset_element(plts_dat, 0.05, 0.05, 0.9, 0.9)

plt_ttl <- ggplot() +
  annotate("text", y = 0.5, x = 0.5, label = "High-dimensional and/or non-tabular data", size = 4) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

plt_dat <- plt_dat + inset_element(plt_ttl, 0.2, 0.95, 0.8, 1.05)

plt_theme2 <-
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "None",
        panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_rect(fill = "white"))

plts_emb <- plot_grid(
  plotlist = list(
    plt_emb_funs + plt_theme2 + ggtitle("A.2"),
    plt_emb_erdr + plt_theme2 + ggtitle("B.2"),
    plt_emb_coil + plt_theme2 + ggtitle("C.2")
  ),
  nrow = 1
)

arrow <- ggplot() + geom_polygon(
  data = data.frame(
    x = c(-1, -1, -2.5, 0, 2.5, 1, 1),
    y = c(0, -4, -4, -6, -4, -4, 0)
  ),
  aes(x = x, y = y), fill = "grey", alpha = 0.3, color = "black") +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  annotate("text", label = "manifold \n learning", x = -1.9, y = -2, size = 3) +
  # annotate("text", label = "&", x = -0, y = -2, size = 3) +
  annotate("text", label = "outlier \n scoring", x = 1.9, y = -2, size = 3)

plt_emb <- plt_rect + inset_element(plts_emb, 0.05, 0.05, 0.9, 0.9)

plt_ttl2 <- ggplot() +
  annotate("text", y = 0.5, x = 0.5, label = "Data embeddings and outlier rankings", size = 4) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

plt_emb <- plt_emb + inset_element(plt_ttl2, 0.2, 0.95, 0.8, 1.05)

dsgn <- c("111
           #2#
           333")

graphical_abstract <- plt_dat / arrow / plt_emb + plot_layout(heights = c(0.45, 0.1, 0.45), design = dsgn)


