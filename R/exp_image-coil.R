# COIL
set.seed(1105)
load(here::here("data/coil20.RData"))
coil_in <- as.matrix(coil20[coil20$Label == 1, -16385])
coil_outs <- as.matrix(coil20[coil20$Label == 3, -16385])

ratio <- 0.1
n_in <- nrow(coil_in)
n_out <- floor(n_in * ratio)
smpl_out <- sample(1:nrow(coil_outs), n_out)

coil_out <- rbind(coil_in, coil_outs[smpl_out, ])
coil_out_lbls <- as.factor(c(rep(0, nrow(coil_in)), rep(1, nrow(coil_outs[smpl_out, ]))))

coil_l2 <- dist(coil_out)
coil_mds <- embed(coil_l2, "mds", k = 2)

out_ranks <- rank(-lof(apply(coil_mds, 2, scale), minPts = nrow(coil_mds) * 0.75))
raw_ranks <- rep("", length(out_ranks))
raw_ranks[out_ranks %in% seq_len(n_out)] <- out_ranks[out_ranks %in% seq_len(n_out)]

plt_emb_coil <-
  plot_emb(coil_mds, col = coil_out_lbls, labels_off = FALSE,
         labels = raw_ranks, size = 0.45) +
  list(xlab(TeX("y_1")), ylab(TeX("y_2"))) +
  scale_color_manual(values = c("grey", "red")) +
  ggtitle(label = "COIL: tMDS")

# plt_pic_coil <-
#   plot_pics(coil_out[c(seq(1, 72, by = 12), 73:75), ], ncol = 3) +
#   coord_flip() + scale_x_reverse() + theme(legend.position = "None",
#                                            strip.background = element_blank(),
#                                            strip.text = element_blank())

plot_pics_temp <- function(dat, labels = NULL, nrow = NULL, ncol = NULL) {
  n_pxls <- ncol(dat)
  n_obs <- nrow(dat)

  dt_dat <- as.data.table(dat)

  tt_image <- melt(dt_dat, measure.vars = colnames(dt_dat))
  tt_image$id <- rep(1:n_obs, n_pxls)

  pixels <- expand.grid(0:(sqrt(n_pxls)-1), 0:(sqrt(n_pxls)-1))

  tt_image$x <- rep(pixels$Var1, each = n_obs)
  tt_image$y <- rep(pixels$Var2, each = n_obs)
  setnames(tt_image, "value", "intensity")

  tt_image$id_fac <- as.factor(tt_image$id)
  if (!is.null(labels)) levels(tt_image$id_fac) <- labels

  rect <- data.frame(ymin = c(rep(NA, 6), rep(0, 3)),
                     xmin = c(rep(NA, 6), rep(0, 3)),
                     ymax = c(rep(NA, 6), rep(127, 3)),
                     xmax = c(rep(NA, 6), rep(125, 3)))
  ggplot(data = tt_image) +
    geom_raster(aes(x, y, fill = intensity)) +
    geom_rect(data = transform(rect, id_fac = tt_image$id_fac),
              aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax), color = "red", size = 1, fill = NA, inherit.aes = FALSE) +
    facet_wrap(
      ~ id_fac,
      nrow = if (is.null(nrow)) max(6, ceiling(n_obs/6)) else nrow,
      ncol = if (is.null(ncol)) 6 else ncol) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          strip.text = element_text(size = 15))
}

plt_pic_coil <-
  plot_pics_temp(coil_out[c(seq(1, 72, by = 12), 73:75), ], ncol = 3) +
  coord_flip() + scale_x_reverse() + theme(legend.position = "None",
                                           strip.background = element_blank(),
                                           strip.text = element_blank())
