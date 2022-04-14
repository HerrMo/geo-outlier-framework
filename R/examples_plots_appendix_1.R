## Appendix plots 1

ratio <- 0.05

# partially copied from https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r
theme = theme_minimal() +
  theme(legend.background=element_blank()) +
  theme(legend.key=element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(panel.spacing=grid::unit(2, "lines")) +
  theme(axis.line=element_line(color="#2b2b2b", size=0.15)) +
  theme(axis.text.x=element_text(margin=margin(t=0))) +
  theme(axis.text.y=element_text(margin=margin(r=0)))

axis_title_just = "rt"
xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=0.9)
theme = theme + theme(axis.title.x=element_text(size = 12, hjust = xj))
theme = theme + theme(axis.title.y=element_text(size = 12, hjust = yj, vjust = -1))
theme = theme + theme(axis.title.y.right=element_text(hjust=yj, angle=90))
theme = theme + theme(strip.text=element_text(hjust=0))
theme = theme + theme(axis.text.x = element_text(size = 7),
                      axis.text.y = element_text(size = 7))

theme_set(theme)

## dodgers data
set.seed(1101)
dodgers_te <- read.table(here::here("data/DodgerLoopWeekend_TEST.txt"))
dodgers_tr <- read.table(here::here("data/DodgerLoopWeekend_TRAIN.txt"))

dodgers <- rbind(dodgers_te, dodgers_tr)
nas <- apply(dodgers, 1, function(x) any(is.na(x))) # obs with nas
dodgers <- dodgers[!nas, ]
dodgers_lbls <- dodgers$V1

##  contamination
dodgers_in <- dodgers[dodgers_lbls == 1, ]
# dodgers_in <- dodgers_in[-c(15, 39, 20), ] # !!! removing label independent inclass structural outlier improves results !!!
dodgers_outs <- dodgers[dodgers_lbls == 2, ]

n_in <- nrow(dodgers_in)
n_out <- ceiling(ratio * n_in)
dodgers_smpl_out <- sample(1:nrow(dodgers_outs), n_out)
dodgers_outs_tmp <- dodgers_outs[dodgers_smpl_out, ]

dodgers_out <- rbind(dodgers_in, dodgers_outs_tmp)
dodgers_out_lbls <- as.factor(dodgers_out$V1)
dodgers_out <- dodgers_out[, -1]

dodgers_out_l2 <- dists(dodgers_out)
dodgers_out_emb <- embed(dodgers_out_l2, "mds")

plt_funs_dodg <-
  plot_funs_temp(dodgers_out, col = dodgers_out_lbls, size = 0.2,
                 alpha = ifelse(dodgers_out_lbls == 1, 0.2, 0.75)) +
  list(xlab("t"), ylab("x(t)")) +
  scale_color_manual(values = c("grey", "red"))
plt_emb_dodg <-
  plot_emb(dodgers_out_emb, col = dodgers_out_lbls)  +
  list(xlab(TeX("y_1")), ylab(TeX("y_2"))) +
  scale_color_manual(values = c("grey", "red"))

plt_app_dodgers <- plot_grid(plotlist = list(plt_funs_dodg + ggtitle(TeX("dodgers: functional observations")),
                                             plt_emb_dodg + ggtitle(("dodgers: tMDS embedding"))),
                             nrow = 1)


##  phoneme data
set.seed(1102)
data("phoneme")

pho <- rbind(phoneme$learn$data, phoneme$test$data)
pho_lbls <- c(phoneme$classlearn, phoneme$classtest)

pho_outs <- pho[pho_lbls == 3, ]
pho_ins <- pho[pho_lbls != 3, ]

n_in <- nrow(pho_ins)
n_out <-  ceiling(ratio * n_in)
out_smpl <- sample(1:nrow(pho_outs), n_out)

pho_out <- rbind(pho_ins, pho_outs[out_smpl, ])
pho_out_lbls <- as.factor(c(pho_lbls[pho_lbls != 3], pho_lbls[pho_lbls == 3][out_smpl]))
pho_out_lbls[pho_out_lbls != 3] <- 1

pho_l2 <- dists(pho_out)
pho_out_emb <- embed(pho_l2, "mds")

plt_funs_pho <-
  plot_funs_temp(pho_out, col = pho_out_lbls, size = 0.2,
            alpha = ifelse(pho_out_lbls != 3, 0.5, 0.75)) +
  list(xlab("t"), ylab("x(t)")) +
  scale_color_manual(values = c("grey", "red"))
plt_emb_pho <-
  plot_emb(pho_out_emb, col = pho_out_lbls, alpha = 0.5)  +
  list(xlab(TeX("y_1")), ylab(TeX("y_2"))) +
  scale_color_manual(values = c("grey", "red"))

plt_app_pho <- plot_grid(plotlist = list(plt_funs_pho + ggtitle(TeX("phoneme: functional observations")),
                                         plt_emb_pho + ggtitle(TeX("phoneme: tMDS embedding"))),
                         nrow = 1)


## starlight
set.seed(1103)
star1 <- read.table(here::here("data/StarLightCurves_TRAIN.txt"))
star2 <- read.table(here::here("data/StarLightCurves_TEST.txt"))

star <- rbind(star1, star2)
star_lbls <- as.factor(star$V1)

star_in <- star[star_lbls %in% c(1, 3), ]
star_outs <- star[star_lbls == 2, ]

in_smpl <- sample(1:nrow(star_in), 1000)
n_in <- length(in_smpl)
star_in_temp <- star_in[in_smpl, ]
n_out <- ceiling(ratio * nrow(star_in_temp))
out_smpl <- sample(1:nrow(star_outs), n_out)
star_out_temp <- star_outs[out_smpl, ]

star_out <- rbind(star_in_temp, star_out_temp)
star_out_lbls <- star_out[, 1]
star_out_lbls[star_out_lbls != 2] <- 1
star_out_lbls <- as.factor(star_out_lbls)
star_out <- star_out[, -1]

star_out_l2 <- dists(star_out)
star_out_emb <- embed(star_out_l2, "mds")

plt_funs_star <-
  plot_funs_temp(star_out, col = star_out_lbls, size = 0.2,
                 alpha = ifelse(star_out_lbls %in% c(1, 2), 0.25, 0.75)) +
  list(xlab("t"), ylab("x(t)")) +
  scale_color_manual(values = c("grey", "red"))
plt_emb_star <-
  plot_emb(star_out_emb, col = star_out_lbls, alpha = 0.3)  +
  list(xlab(TeX("y_1")), ylab(TeX("y_2"))) +
  scale_color_manual(values = c("grey", "red"))

plt_app_star <- plot_grid(plotlist = list(plt_funs_star + ggtitle(TeX("starlight: functional observations")),
                                          plt_emb_star + ggtitle("starlight: tMDS embedding")),
                          nrow = 1)

