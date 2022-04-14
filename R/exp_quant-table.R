
# minPts/k
k <- c(0.01, 0.1, 0.75, 0.9)

## dodgers data
dodgers_te <- read.table(here::here("data/DodgerLoopWeekend_TEST.txt"))
dodgers_tr <- read.table(here::here("data/DodgerLoopWeekend_TRAIN.txt"))

dodgers <- rbind(dodgers_te, dodgers_tr)
nas <- apply(dodgers, 1, function(x) any(is.na(x))) # obs with nas
dodgers <- dodgers[!nas, ]
dodgers_lbls <- dodgers$V1

# plot_funs(dodgers, col = dodgers_lbls)
# plot_emb(embed(dists(dodgers[, -1]), "mds"), col = dodgers_lbls)

##  contamination
dodgers_in <- dodgers[dodgers_lbls == 1, ]
# dodgers_in <- dodgers_in[-c(15, 39, 20), ] # !!! removing label independent inclass structural outlier improves results !!!
dodgers_outs <- dodgers[dodgers_lbls == 2, ]

tps_dodg <- tps_dodg_m <- vector("list", length(ratios))
names(tps_dodg) <- names(tps_dodg_m) <- ratios

for (i in seq_along(ratios)) {
  tps_k <- vector("list", length(k))
  names(tps_k) <- k
  for (j in seq_along(k)) {
    tp <- vector("numeric", length(reps))
    for (rep in reps) {
      set.seed(50 + rep)
      n_in <- nrow(dodgers_in)
      n_out <- ceiling(ratios[i] * n_in)
      dodgers_out_smpl <- sample(1:nrow(dodgers_outs), n_out)
      dodgers_out_tmp <- dodgers_outs[dodgers_out_smpl, ]

      dodgers_out <- rbind(dodgers_in, dodgers_out_tmp)
      dodgers_out_lbls <- dodgers_out$V1
      dodgers_out <- dodgers_out[, -1]

      dodgers_out_l2 <- dists(dodgers_out)
      dodgers_out_emb <- embed(dodgers_out_l2, "mds")

      k_temp <- floor(k[j] * nrow(dodgers_out_emb))
      out_rank <- frank(dodgers_out_emb, k = k_temp)

      tp[rep] <- auc(roc(c(rep(0, n_in), rep(1, n_out)), out_rank))
      print(paste("rat:", ratios[i], "rep:", rep, "k:", k[j]))
    }
    tps_k[[j]] <- tp
  }
  tps_dodg[[i]] <- tps_k
  tps_dodg_m[[i]] <- vapply(tps_k, mean, numeric(1))
}

apply(t(simplify2array(tps_dodg_m)), 2, round, digits = 2)


##  phoneme data
data("phoneme")

pho <- rbind(phoneme$learn$data, phoneme$test$data)
pho_lbls <- c(phoneme$classlearn, phoneme$classtest)
# plot_funs(pho)

pho_outs <- pho[pho_lbls == 3, ]
pho_ins <- pho[pho_lbls != 3, ]

tps_pho <- tps_pho_m <-  vector("list", length(ratios))
names(tps_pho) <- names(tps_pho_m) <- ratios
for (i in seq_along(ratios)) {
  tps_k <- vector("list", length(k))
  names(tps_k) <- k
  for (j in seq_along(k)) {
    tp <- vector("numeric", length(reps))
    for (rep in reps) {
      set.seed(100 + rep)
      n_in <- nrow(pho_ins)
      n_out <-  ceiling(ratios[i] * n_in)
      pho_out_smpl <- sample(1:nrow(pho_outs), n_out)

      pho_out <- rbind(pho_ins, pho_outs[pho_out_smpl, ])
      pho_l2 <- dists(pho_out)
      pho_emb <- embed(pho_l2, "mds")

      k_temp <- floor(k[j] * nrow(pho_emb))
      out_rank <- frank(pho_emb, k = k_temp)

      tp[rep] <- auc(roc(c(rep(0, n_in), rep(1, n_out)), out_rank))
      print(paste("rat:", ratios[i], "rep:", rep, "k:", k[j]))
    }
    tps_k[[j]] <- tp
  }
  tps_pho[[i]] <- tps_k
  tps_pho_m[[i]] <- vapply(tps_k, mean, numeric(1))
}
apply(t(simplify2array(tps_pho_m)), 2, round, digits = 2)


## starlight
star1 <- read.table(here::here("data/StarLightCurves_TRAIN.txt"))
star2 <- read.table(here::here("data/StarLightCurves_TEST.txt"))

star <- rbind(star1, star2)
star_lbls <- as.factor(star$V1)

star_in <- star[star_lbls %in% c(1, 3), ]
star_outs <- star[star_lbls == 2, ]

tps_star <- tps_star_m <- vector("list", length(ratios))
names(tps_star) <- names(tps_star_m) <- ratios
for (i in seq_along(ratios)) {
  tps_k <- vector("list", length(k))
  names(tps_k) <- k
  for (j in seq_along(k)) {
    tp <- vector("numeric", length(reps))
    for (rep in reps) {
      set.seed(150 + rep)
      n_in <- 1000
      n_out <- ceiling(ratios[i] * n_in)
      in_smpl <- sample(1:nrow(star_in), n_in)
      star_in_temp <- star_in[in_smpl, ]
      star_out_smpl <- sample(1:nrow(star_outs), n_out)
      star_out_temp <- star_outs[star_out_smpl, ]

      star_out <- rbind(star_in_temp, star_out_temp)
      star_out <- star_out[, -1]

      star_l2 <- dists(star_out)
      star_emb <- embed(star_l2, "mds")

      k_temp <- floor(k[j] * nrow(star_emb))
      out_rank <- frank(star_emb, k = k_temp)

      tp[rep] <- auc(roc(c(rep(0, n_in), rep(1, n_out)), out_rank))
      print(paste("rat:", ratios[i], "rep:", rep, "k:", k[j]))
    }
    tps_k[[j]] <- tp
  }
  tps_star[[i]] <- tps_k
  tps_star_m[[i]] <- vapply(tps_k, mean, numeric(1))
}
apply(t(simplify2array(tps_star_m)), 2, round, digits = 2)

