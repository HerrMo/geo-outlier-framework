outscore <- function(dat, lbls, mod, B = 50) {
  temp_task <- makeOneClassTask(data = data.frame(V1 = dat[, 1],
                                                  V2 = dat[, 2],
                                                  target = as.factor(lbls)),
                                target = "target",
                                positiv = "1",
                                negative = "0")

  rowSums(sapply(mod[[1]], function(x) predict(x, temp_task)$data$prob.0)) / B
}


emb_dat <- function(n_in, n_out, dat, meth = "mds", derive = FALSE, ...) {
  temp_dat <- switch(dat,
                     hm = hm_dat(n_in = n_in, n_out = n_out),
                     mod1 = g_dat(n_in = n_in, n_out = n_out, mod = "mod1"),
                     mod2 = g_dat(n_in = n_in, n_out = n_out, mod = "mod2"),
                     mod3 = g_dat(n_in = n_in, n_out = n_out, mod = "mod3"),
                     mod4 = g_dat(n_in = n_in, n_out = n_out, mod = "mod4"),
                     mod5 = g_dat(n_in = n_in, n_out = n_out, mod = "mod5"),
                     mod6 = g_dat(n_in = n_in, n_out = n_out, mod = "mod6"))

  temp_lbls <- c(rep(0, n_in), rep(1, n_out))

  temp_dists  <- if (derive) as.matrix(d_dist(temp_dat, grid = seq(0, 1, length.out = 30))) else dists(temp_dat)
  temp_emb <- embed(temp_dists, method = meth, ...)
  list("dat" = temp_dat, "lbls" = temp_lbls, "emb" = temp_emb)
}

hm_dat <- function(n_in = 95, n_out = 5) {
  a <- rnorm(n_in, 5, 2)
  b <- rnorm(n_out, 5, sqrt(3))

  t <- 1:500
  xt <- t/500

  inclass <- t(simplify2array(lapply(a, function(x) {x + 0.05 * t + sin(pi * (xt)^2)})))
  outclass <- t(simplify2array(lapply(b, function(x) {x + 0.05 * t + cos(20 * pi * xt)})))
  dat <- rbind(inclass, outclass)
  dat
}

hm_dat_ext <- function(n_in = 100, n_out = 5) {
  a <- rnorm(n_in, 15, 2)
  b <- rnorm(n_out, 5, sqrt(3))
  c <- rnorm(n_out, 25, sqrt(3))

  t <- 1:500
  xt <- t/500

  inclass <- t(simplify2array(lapply(a, function(x) {x + 0.01 * t + sin(pi * (xt)^2)})))
  outclass <- t(simplify2array(lapply(b, function(x) {x + 0.05 * t + cos(20 * pi * xt)})))
  outclass2 <- t(simplify2array(lapply(c, function(x) {(x + -0.05 * t) + rnorm(length(t), 0, 2)})))
  dat <- rbind(inclass, outclass, outclass2)
  dat
}

g_dat <- function(n_in = 49, n_out = 1, grid = seq(0, 1, length.out = 30), mod = "mod1") {
  # null model producing inclass observation
  kern0 <- function(s, t) {exp(-abs(s - t))}
  mod0_1 <- 4 * grid
  mod0_2 <- t(sim_gauss(n_in, grid = grid, kernel = kern0))

  mod0 <- mod0_2 + rep(mod0_1, each = nrow(mod0_2))

  # model with outliers
  if (mod == "mod1") {
    mod1_out <- mod0_1 + ifelse(grid > runif(1), 3, 0)
    mod1_out <-  t(replicate(n_out, mod1_out + t(sim_gauss(1, grid = grid, kernel = kern0)), simplify = TRUE))
    rbind(mod0, mod1_out)
  } else if (mod == "mod2") {
    U <- runif(1)
    mod2_out <- mod0_1 + ifelse((grid > U) & (grid < (U + 0.04)), 3, 0)
    mod2_out <- t(replicate(n_out, mod2_out + t(sim_gauss(1, grid = grid, kernel = kern0)), simplify = TRUE))
    rbind(mod0, mod2_out)
  } else if (mod == "mod3") {
    mod3_kern1 <- function(s, t) {exp((-(abs(s - t) ^ 2)))}
    mod3_kern2 <- function(s, t) {exp((-(abs(s - t) ^ 0.2)))}

    mod3_main <- t(sim_gauss(n_in, grid = grid, kernel = mod3_kern1))
    mod3_main <- mod3_main + rep(mod0_1, each = nrow(mod3_main))

    mod3_out <- t(replicate(n_out, mod0_1 + sim_gauss(1, grid = grid, kernel = mod3_kern2), simplify = TRUE))

    rbind(mod3_main, mod3_out)
  } else if (mod == "mod4") {
    mod4_kern <- function(s, t) {0.3 * exp((-(abs(s - t) / 0.3)))}
    mod4_g1 <- 30 * grid * (1 - grid) ^ (3 / 2)
    mod4_g2 <- 30 * (1 - grid) * (grid ^ (3/2))

    mod4_main <- t(sim_gauss(n_in, grid = grid, kernel = mod4_kern))
    mod4_main <- mod4_main + rep(mod4_g1, each = nrow(mod4_main))
    mod4_out <- t(replicate(n_out, sim_gauss(1, grid = grid, kernel = mod4_kern) + mod4_g2, simplify = TRUE))

    rbind(mod4_main, mod4_out)
  } else if (mod == "mod5") {
    mod5_kern <- function(s, t) {0.1 * exp((-(abs(s - t) / 0.3)))}

    mod5_main <- t(replicate(n_in, rnorm(1, sd = 2) + rexp(1) * atan(grid) +
                                sim_gauss(1, grid = grid, kernel = mod5_kern), simplify = TRUE))
    mod5_out <- t(replicate(n_out, 1 - 2 * atan(grid) + sim_gauss(1, grid = grid, kernel = mod5_kern), simplify = TRUE))

    # mod5_main <- sim_gauss(n_in, grid = grid, kernel = mod5_kern)
    # mod5_main <- t(mod5_main) + t(replicate(n_in, rnorm(1, sd = 2) + rexp(1) * atan(grid)))
    #
    # mod5_out <- t(replicate(n_out, 1 - 2 * atan(grid) + sim_gauss(1, grid = grid, kernel = mod5_kern), simplify = TRUE))

    rbind(mod5_main, mod5_out)
  } else if (mod == "mod6") {
    mod6_main <- t(replicate(n_in, runif(1, 0, 0.1) * cos(2 * pi * grid) + runif(1, 0, 0.1) * sin(2 * pi * grid)))
    mod6_out <- t(replicate(n_out, runif(1, 0.1, 0.12) * cos(2 * pi * grid) + runif(1, 0.1, 0.12) * sin(2 * pi * grid)))

    rbind(mod6_main, mod6_out)
  }
}
