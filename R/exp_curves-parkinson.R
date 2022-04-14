### parkinson spirals

### make complete dataset

ctrls_files <- list.files(here::here("data/hw_dataset/hw_dataset/control"))
park_files <- list.files(here::here("data/hw_dataset/hw_dataset/parkinson"))

ctrls <- lapply(
  paste0(here::here("data/hw_dataset/hw_dataset/control/"), ctrls_files),
  function(file) {
    temp <- read.csv2(file, header = FALSE)
    names(temp) <- c("x", "y", "z", "pressure", "gripangle", "timestamp", "testID")
    temp
  }
)

parks <- lapply(
  paste0(here::here("data/hw_dataset/hw_dataset/parkinson/"), park_files),
  function(file) {
    temp <- read.csv2(file, header = FALSE)
    names(temp) <- c("x", "y", "z", "pressure", "gripangle", "timestamp", "testID")
    temp
  }
)

# spiral curvs dynamic test
ctrl_crvs <- lapply(ctrls, function(x) t(x[x$testID == 1, 1:2]))
park_crvs <- lapply(parks, function(x) t(x[x$testID == 1, 1:2]))

n_out <- 2
out_smpl <- sample(seq_along(park_crvs), n_out)
crvs_out <- c(ctrl_crvs, park_crvs[c(1, 12)])
crvs_lbls <- rep(c(1, 3), c(length(ctrl_crvs), n_out))

crvs_smpl <- 200
crvs_out_algn <- parallel::mclapply(
  crvs_out,
  function(x) fdasrvf::resamplecurve(x, N = crvs_smpl, "O"),
  mc.cores = 6
)

shape_dist <- function(l_dat, method = "fdasrvf", ...) {
  nobs <- length(l_dat)
  combs <- combn(1:nobs, 2)
  m_geodist <- m_phasedist <- diag(x = 0, nobs)
  for (i in seq_len(ncol(combs))) {
    ind <- combs[, i]
    ind1 <- ind[1]
    ind2 <- ind[2]

    m_geodist[ind1, ind2] <-
      fdasrvf::calc_shape_dist(l_dat[[ind1]], l_dat[[ind2]], ...)$d
    m_phasedist[ind, ind2] <-
      fdasrvf::calc_shape_dist(l_dat[[ind1]], l_dat[[ind2]], ...)$dx
  }
  l_dists <- lapply(
    list(m_geodist, m_phasedist),
    function(m_dist) {
      m_dist[lower.tri(m_dist)] <- t(m_dist)[lower.tri(m_dist)]
      m_dist
    }
  )

  names(l_dists) <- c("geodesic", "phase")
  l_dists
}


# computations commented out due to computation time: ~ 2.5 min
# crvs_shp_dists <- shape_dist(crvs_out_algn, mode = "0")
# save(crvs_shp_dists, file = "data/curves_shp_dist.RData")

load(here::here("data/curves_shp_dist.RData"))
mds_curves <- embed(crvs_shp_dists$geodesic, "mds", k = 2)


spec_cols <- list(
  scale_colour_manual(
    values = c(
      scales::alpha("grey", 0.75),
      scales::alpha("blue", 0.5),
      "red"
    )
  )
)

out_ranks <- rank(-lof(apply(mds_curves, 2, scale), minPts = nrow(mds_curves) * 0.75))
raw_ranks <- rep("", length(out_ranks))
raw_ranks[out_ranks %in% 1:3] <- out_ranks[out_ranks %in% 1:3]

outlier_lbls <- crvs_lbls
outlier_lbls[out_ranks == 3] <- "2"
status_lbls <- as.factor(crvs_lbls)
outlier_lbls <- as.factor(outlier_lbls)
levels(outlier_lbls) <- c("Inlier", "Distributional Outlier", "Structural Outlier")

plt_emb_crvs <-
  plot_emb(mds_curves,
           color = outlier_lbls,
           labels_off = FALSE,
           labels = raw_ranks,
           size = 0.45, label_size = 0.5) +
  list(xlab(TeX("y_1")), ylab(TeX("y_2"))) +
  spec_cols


dt_curvs <- rbindlist(lapply(crvs_out, function(dat) as.data.frame(t(dat))))
dt_curvs[, ":="(id = rep(seq_along(crvs_out), sapply(crvs_out, ncol)),
                outlier = as.factor(rep(outlier_lbls, sapply(crvs_out, ncol))),
                status =
                  factor(rep(
                   c(rep("control", length(ctrl_crvs)),
                     rep("parkinson", n_out)),
                   sapply(crvs_out, ncol)),
                  levels = c("parkinson", "control"))
                )
         ]

plt_crvs <-
  ggplot(dt_curvs) +
  geom_path(
    aes(x = x, y = y, group = id, color = outlier),#, lty = status),
    size = 0.25
  ) +
  spec_cols +
  theme(legend.position = "bottom")



