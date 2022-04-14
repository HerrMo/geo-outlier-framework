# coil data
dt_coil <- data.table::as.data.table(coil20)

set.seed(15)
smpl_coil3 <- sample(1:72, 1)
coil_1 <- dt_coil[coil20$Label == 1, -16385]
coil_3 <- dt_coil[coil20$Label == 3, -16385]
coil_dat <- rbind(coil_1, coil_3[smpl_coil3])


# pic_plt <- plot_pics(
#   coil_dat[c(1, 3, 5, nrow(coil_dat)), ],
#   nrow = 2,
#   ncol = 2
# ) + coord_flip() + scale_x_reverse() + theme(legend.position = "None",
#                                              strip.background = element_blank(),
#                                              strip.text = element_blank())

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

  rect <- data.frame(ymin = c(rep(NA, 3), 0),
                     xmin = c(rep(NA, 3), 0),
                     ymax = c(rep(NA, 3), 127),
                     xmax = c(rep(NA, 3), 125))
  ggplot(data = tt_image) +
    geom_raster(aes(x, y, fill = intensity)) +
    geom_rect(data = transform(rect, id_fac = as.character(1:4)),
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

pic_plt <- plot_pics_temp(
  coil_dat[c(1, 3, 5, nrow(coil_dat)), ],
  nrow = 2,
  ncol = 2
) + coord_flip() + scale_x_reverse() + theme(legend.position = "None",
                                             strip.background = element_blank(),
                                             strip.text = element_blank())
