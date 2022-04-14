# ECG data from TSC bake-off
# http://www.timeseriesclassification.com/description.php?Dataset=TwoLeadECG

ecg_tr <- read.table(here::here("data/TwoLeadECG_TRAIN.txt"))
ecg_te <- read.table(here::here("data/TwoLeadECG_TEST.txt"))

set.seed(1221)

ecg_lbls <- c(ecg_tr$V1, ecg_te$V1)
ecg <- rbind(ecg_tr[, -1], ecg_te[, -1])

ecg2 <- sample(which(ecg_lbls == 2), 10)
ecg1 <- which(ecg_lbls == 1)
ecg_out <- ecg[c(ecg1, ecg2), ]
ecg_lbls_out <- ecg_lbls[c(ecg2, ecg1)]

ecg_dists <- dist(ecg_out)
ecg_mds <- embed(ecg_dists, "mds")
# ecg_imap <- embed(ecg_dists, "isomap", k = 50)

lbls_out <- c(586, 582, 591, 590)
lbls <- rep(1, nrow(ecg_mds))
lbls[lbls_out[1:3]] <- 2
lbls[lbls_out[4]] <- 3
lbls <- as.factor(lbls)

# lbls <- as.factor(ifelse(1:length(c(ecg1, ecg2)) %in% lbls_out, 2, 1))

# print("here we are")
# print(dim(ecg_mds))
# print(dists)
out_ranks <- rank(-lof(apply(ecg_mds, 2, scale), minPts = nrow(ecg_mds) * 0.75))
raw_ranks <- rep("", length(out_ranks))
raw_ranks[out_ranks %in% 1:3] <- out_ranks[out_ranks %in% 1:3]
raw_ranks[lbls_out[4]] <- out_ranks[lbls_out[4]]

out_ranks_local <- rank(-lof(apply(ecg_mds, 2, scale), minPts = 5))
out_ranks_local[which(out_ranks_local > 10)] <- ""

spec_cols <- list(
  scale_colour_manual(
    values = c(
      scales::alpha("grey", 0.5),
      "blue",
      "green"
    )
  )
)

plt_emb_ecg <-
  plot_emb(ecg_mds, color = lbls, labels_off = FALSE, labels = raw_ranks) +
  list(xlab(TeX("y_1")), ylab(TeX("y_2"))) +
  spec_cols

plt_funs_ecg <-
  plot_funs(ecg_out, col = lbls) +
  spec_cols +
  list(xlab("t"), ylab("x(t)")) +
  scale_x_continuous(labels = seq(420, 1020, l = 5))



