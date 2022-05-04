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


## misc
#' @export
plot_funs <- function(data, ...) {
  UseMethod("plot_funs")
}


# default function for data in fundata in matrix format
#' @export
plot_funs.default <- function(data, col = NULL, args = NULL, ...) {
  n <- nrow(data)
  grid_len <- ncol(data)
  df_dat <- data.frame(
    args = if (is.null(args)) rep(1:grid_len, n) else args,
    vals = c(t(data)),
    id = as.factor(rep(1:n, each = grid_len))
  )

  if (!is.null(col)) df_dat$col <- rep(col, each = grid_len)

  ggplot(df_dat) +
    geom_line(aes(x = args,
                  y = vals,
                  group = id,
                  colour = if (is.null(col)) {id} else {col}),
              ...) +
    theme(legend.position = "None")
}

## embedding
embed <- function(dist_mat, method = c("isomap", "umap", "diffmap", "mds", "tsne"), ...) {
  method <- match.arg(method, c("isomap", "umap", "diffmap", "mds", "tsne"))

  # change to assert: accept only matrices?
  if (inherits(dist_mat, "dist")) dist_mat <- as.matrix(dist_mat)

  emb <-
    switch(
      method,
      "isomap" = vegan::isomap(dist_mat, ...),
      "umap" = umap::umap(dist_mat, input = "dist", ...),
      "diffmap" =  diffuse2(dist_mat, ...),
      "mds" = cmdscale(dist_mat, ...),
      "tsne" = Rtsne::Rtsne(dist_mat, is_distance = TRUE, ...)
    )

  if (method == "tsne") class(emb) <- "tsne"

  emb
}


# help fun S3 class to extract embedding coordinates
#' @export
extract_points <- function(x, ...) {
  UseMethod("extract_points")
}

# S3 method for isomap
#' @export
extract_points.isomap <- function(embedding, ndim = dim(embedding$points)[2]) {
  embedding$points[, 1:ndim, drop = FALSE]
}

# S3 method for umap
#' @export
extract_points.umap <- function(embedding, ndim = dim(embedding$layout)[2]) {
  embedding$layout[, 1:ndim, drop = FALSE]
}

# S3 method for diffusionMap
#' @export
extract_points.diffuse <- function(embedding, ndim = dim(embedding$X)[2]) {
  embedding$X[, 1:ndim, drop = FALSE]
}

# S3 method for matrix output, e.g. mds
#' @export
extract_points.matrix <- function(embedding, ndim = dim(embedding)[2]) {
  embedding[, 1:ndim, drop = FALSE]
}

# S3 method for tsne
#' @export
extract_points.tsne <- function(embedding, ndim = dim(embedding$Y)[2]) {
  embedding$Y[, 1:ndim, drop = FALSE]
}


## plotting
# function to plot embeddings
#' @export
plot_emb <- function(embedding, ...) {
  UseMethod("plot_emb")
}

# default method for embedding data in 2d matrix format
#' @export
plot_emb.default <- function(pts, color = NULL, size = 1, ...) {

  dat <- data.frame(dim1 = pts[, 1],
                    dim2 = pts[, 2],
                    color = 1:nrow(pts))

  if (!is.null(color)) dat$color <- color

  p <- ggplot(dat) +
    geom_point(aes(x = dim1,
                   y = dim2,
                   colour = color),
               size = size) +
    theme(legend.position = "Non") +
    ggtitle(label = "2d-embedding")
  p
}

#' @export
plot_emb.matrix <- function(embedding, color = NULL, labels_off = TRUE, labels = NULL, size = 1, ...) {
  # TODO argument checking (min 2-d data, etc)

  pts <- extract_points(embedding, 2)
  p <- plot_emb.default(pts, color = color, labels = labels, size = size, ...)
  if (!labels_off) p <- if (is.null(labels)) {
    p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = 1:nrow(pts)), size = label_size)
  } else {
    p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = labels), size = label_size)
  }
  p
}

# for objects of class embedding
#' @export
plot_emb.embedding <- function(embedding, color = NULL, labels = FALSE, size = 1) {
  # TODO argument checking (min 2-d data, etc)

  emb <- embedding$emb
  pts <- extract_points(emb, 2)
  p <- plot_emb.default(pts, color = color, labels = labels, size = size, ...)
  if (labels) p <- p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = 1:nrow(pts)))
  p
}

