outdetect <- function(dat, B = 50, param = seq(0.01, 5, length.out = 50)) {
  count <- 1

  opt_mod <- vector("list", B)
  perf <- vector("list", B * length(param))

  task <- makeOneClassTask(data = data.frame(V1 = dat$emb[, 1],
                                             V2 = dat$emb[, 2],
                                             target = as.factor(dat$lbls)),
                           target = "target",
                           positive = "1",
                           negative = "0")
  AMV = makeAMVMeasure(id = "AMV", minimize = TRUE, alphas = c(0.7, 0.99), n.alpha = 20, n.sim = 10e3, best = 0, worst = NULL)

  for (i in seq_len(B)) {
    inds.split = BBmisc::chunk(seq_len(nrow(dat$dat)), shuffle = TRUE, props = c(0.8, 0.2))
    train.inds = inds.split[[1]]
    test.inds = inds.split[[2]]

    par_count <- 1
    for (par in param) {
      lrn = makeLearner("oneclass.svm", predict.type = "prob", gamma = par)
      temp_mod = train(lrn, task, subset = train.inds)
      pred = predict(temp_mod, task, subset = test.inds)

      perf[[count]] <- performance(pred = pred, measures = list(AMV, f1, auc), model = temp_mod, task = task)
      if (par_count == 1) {
        opt_mod[[i]] <- temp_mod
      } else if (perf[[par_count]]["AMV"] < perf[[par_count - 1]]["AMV"]) {
        opt_mod[[i]] <- temp_mod
      }
      count <- count + 1
      par_count <- par_count + 1
    }
  }
  list("mod" = opt_mod, "perf" = perf)
}

d_dist <- function(mat, a = 0.5, grid, d_only = TRUE) {

  derivs <- t(apply(mat, 1, function(f) my_deriv(grid = grid, vals = f)))

  dfuns <- dist(mat)
  dders <- dist(derivs)

  a * as.matrix(dfuns) + (1 - a) * as.matrix(dders)
}

my_deriv = function(grid, vals){
  diff(vals)/diff(grid)
}

#' Compute distances from graph laplacian's
#' @param grs list of graphs
#' @param file File to dump to usign saveRDS. NULL: Do not save
graph_distances = function(grs, file = NULL) {
  grs_lap <- lapply(grs, graph.laplacian)
  cmbs <- combn(1:length(grs_lap), 2)
  net_dists <- diag(x = 0, length(grs_lap))

  for (i in seq_len(ncol(cmbs))) {
    ind <- cmbs[, i]
    ind1 <- ind[1]
    ind2 <- ind[2]
    net_dists[ind1, ind2] <- Frobenius(as.matrix(grs_lap[[ind1]]), as.matrix(grs_lap[[ind2]]))
  }
  net_dists[lower.tri(net_dists)] <- t(net_dists)[lower.tri(net_dists)]
  if (!is.null(file))
    saveRDS(net_dists, file)
  return(net_dists)
}

# adjusting plot_funs function to specific alpha level in aes
plot_funs_temp <- function(data, col = NULL, args = NULL, alpha, ...) {
  n <- nrow(data)
  grid_len <- ncol(data)
  df_dat <- data.frame(
    args = if (is.null(args)) rep(1:grid_len, n) else args,
    vals = c(t(data)),
    id = as.factor(rep(1:n, each = grid_len)),
    alpha = rep(alpha, each = grid_len)
  )

  if (!is.null(col)) df_dat$col <- rep(col, each = grid_len)

  ggplot(df_dat) +
    geom_line(aes(x = args,
                  y = vals,
                  group = id,
                  colour = if (is.null(col)) {id} else {col},
                  alpha = alpha),
              ...) +
    theme(legend.position = "None")}


## load fmnist

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('data/train-images-idx3-ubyte')
  test <<- load_image_file('data/t10k-images-idx3-ubyte')

  train$y <<- load_label_file('data/train-labels-idx1-ubyte')
  test$y <<- load_label_file('data/t10k-labels-idx1-ubyte')
}
