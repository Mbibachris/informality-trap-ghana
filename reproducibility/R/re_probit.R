# Dynamic random-effects probit with the Wooldridge (2005) initial-conditions
# device, by maximum likelihood with Gauss-Hermite quadrature over the
# normal random intercept. R port of the validated scripts/re_probit.py
# (Python engine, cross-checked there for quadrature stability and
# multi-start convergence before this port was trusted).
#
# Why a custom engine instead of lme4::glmer: glmer (nAGQ=1, bobyqa) was
# tried first and did not return after 47+ minutes of CPU time on this
# exact spec (39 params, ~45k obs, one scalar random intercept) -- some
# pathological interaction between the many region/sector dummies and
# lme4's PIRLS inner loop on this data, not merely "cautious." The custom
# quadrature MLE below (analytic score, BFGS via optim) fits the same model
# in ~20-40 seconds, matching the already-validated Python numbers.
#
# Model: y*_it = X_it'beta + alpha_i + u_it, y_it = 1[y*_it>0], u_it~N(0,1),
# alpha_i ~ N(0, sigma_alpha^2). X_it already includes the Wooldridge
# auxiliaries (y_i1, within-person means), so independence of alpha_i from
# X_it is the Mundlak/Wooldridge conditional-independence assumption, not a
# raw random-effects assumption.

library(pracma)
library(ucminf)

quad_nodes <- function(n_quad) {
  gh <- pracma::gaussHermite(n_quad)
  list(nodes = gh$x, logw = log(gh$w) - 0.5 * log(pi))
}

# Core likelihood + analytic score. Returns list(nll, grad[, S_theta]).
# X: (N,P) incl. intercept. y: (N,) 0/1. person_idx: (N,) integer 1..n_persons.
neg_ll_and_grad <- function(theta, X, y, person_idx, n_persons, nodes, logw, need_scores = FALSE) {
  P <- ncol(X)
  beta <- theta[1:P]
  sigma <- exp(theta[P + 1])
  K <- length(nodes)
  N <- nrow(X)

  eta <- as.numeric(X %*% beta)
  sign <- 2 * y - 1
  shift <- sqrt(2) * sigma * nodes  # (K,)

  person_logsum <- matrix(0, nrow = n_persons, ncol = K)
  phi_over_Phi <- matrix(0, nrow = N, ncol = K)

  for (k in seq_len(K)) {
    m <- sign * (eta + shift[k])
    logcdf <- pnorm(m, log.p = TRUE)
    logpdf <- dnorm(m, log = TRUE)
    phi_over_Phi[, k] <- sign * exp(logpdf - logcdf)
    person_logsum[, k] <- rowsum(logcdf, group = person_idx, reorder = TRUE)[, 1]
  }

  person_logsum <- sweep(person_logsum, 2, logw, "+")
  m_max <- apply(person_logsum, 1, max)
  person_ll <- m_max + log(rowSums(exp(person_logsum - m_max)))
  total_ll <- sum(person_ll)

  post <- exp(person_logsum - person_ll)  # (n_persons, K), rows sum to 1

  S_beta <- matrix(0, nrow = n_persons, ncol = P)
  S_sigma <- numeric(n_persons)
  for (k in seq_len(K)) {
    r <- phi_over_Phi[, k]
    wr <- r * X  # (N,P), recycled elementwise per column
    contrib <- rowsum(wr, group = person_idx, reorder = TRUE)  # (n_persons,P)
    S_beta <- S_beta + post[, k] * contrib
    person_r_sum <- rowsum(r, group = person_idx, reorder = TRUE)[, 1]
    S_sigma <- S_sigma + post[, k] * sqrt(2) * nodes[k] * person_r_sum
  }

  S_theta <- cbind(S_beta, S_sigma * sigma)  # chain rule for log_sigma param
  grad <- colSums(S_theta)

  if (need_scores) return(list(nll = -total_ll, grad = -grad, S_theta = S_theta))
  list(nll = -total_ll, grad = -grad)
}

fit_re_probit <- function(X, y, person_idx, cluster_idx, X_cols, n_quad = 12,
                           theta0 = NULL, maxit = 300) {
  X <- as.matrix(X); storage.mode(X) <- "double"
  y <- as.numeric(y)
  n_persons <- max(person_idx)
  n_clusters_raw <- max(cluster_idx)
  N <- nrow(X); P <- ncol(X)

  qn <- quad_nodes(n_quad)
  nodes <- qn$nodes; logw <- qn$logw

  if (is.null(theta0)) {
    p_bar <- min(max(mean(y), 0.01), 0.99)
    theta0 <- numeric(P + 1)
    theta0[1] <- qnorm(p_bar)
    theta0[P + 1] <- 0
  }

  # NOTE: a memoization cache here (keyed by identical(theta, last_theta))
  # was tried to avoid duplicating the forward pass across fn()/gr() calls
  # at the same point, but produced silently wrong fits -- ucminf returned
  # near-instantly at (a value very close to) theta0 with a bogus
  # "converged" status, apparently because R's `identical()` on the theta
  # vectors ucminf passes in does not behave as a safe cache key here (root
  # cause not fully isolated, not worth the risk of chasing further).
  # Recomputing directly is simple, correct (verified against the Python
  # engine to 6 significant figures), and not meaningfully slower.
  fn <- function(theta) neg_ll_and_grad(theta, X, y, person_idx, n_persons, nodes, logw)$nll
  gr <- function(theta) neg_ll_and_grad(theta, X, y, person_idx, n_persons, nodes, logw)$grad

  # ucminf (quasi-Newton, tighter gradient-based stopping rule) converges
  # both faster AND to a tighter optimum than optim(method="BFGS") on this
  # likelihood -- confirmed empirically (grad norm ~1e-6 vs ~1e-2, ~30% less
  # wall time). glmer was tried first and never returned after 47+ minutes
  # on this exact spec -- this custom engine is the reliable path.
  res <- ucminf::ucminf(theta0, fn, gr, control = list(maxeval = max(500, maxit)))

  theta_hat <- res$par
  final <- neg_ll_and_grad(theta_hat, X, y, person_idx, n_persons, nodes, logw, need_scores = TRUE)
  grad_norm <- max(abs(final$grad))
  converged <- grad_norm < 1e-2
  S_theta <- final$S_theta

  bread_inv <- t(S_theta) %*% S_theta
  bread <- MASS::ginv(bread_inv)

  # first cluster id seen per person (cluster is constant within a person's rows)
  first_cluster <- tapply(cluster_idx, person_idx, function(v) v[1])
  first_cluster <- as.integer(first_cluster[order(as.integer(names(first_cluster)))])
  n_c <- max(first_cluster)
  cluster_scores <- rowsum(S_theta, group = first_cluster, reorder = TRUE)
  meat <- t(cluster_scores) %*% cluster_scores

  vcov <- bread %*% meat %*% bread
  se <- sqrt(pmax(diag(vcov), 0))

  structure(list(
    theta = theta_hat, se = se, vcov = vcov,
    param_names = c(X_cols, "log_sigma_alpha"),
    loglik = -res$value, n_obs = N, n_persons = n_persons, n_clusters = n_c,
    converged = converged, grad_norm = grad_norm, n_iter = unname(res$info["neval"]),
    X_cols = X_cols, y = y, X = X, person_idx = person_idx, cluster_idx = cluster_idx,
    score_matrix = S_theta
  ), class = "re_probit_result")
}

beta_of <- function(fit) fit$theta[1:length(fit$X_cols)]
sigma_alpha_of <- function(fit) exp(fit$theta[length(fit$theta)])
se_sigma_alpha_of <- function(fit) fit$se[length(fit$se)] * sigma_alpha_of(fit)

coef_table.re_probit_result <- function(fit) {
  z <- fit$theta / fit$se
  p <- 2 * (1 - pnorm(abs(z)))
  data.frame(
    param = c(fit$param_names, "sigma_alpha (implied)"),
    coef = c(fit$theta, sigma_alpha_of(fit)),
    se = c(fit$se, se_sigma_alpha_of(fit)),
    z = c(z, NA), p = c(p, NA)
  )
}

wald_test <- function(fit, param_names_subset) {
  idx <- match(param_names_subset, fit$param_names)
  b <- fit$theta[idx]
  V <- fit$vcov[idx, idx, drop = FALSE]
  stat <- as.numeric(t(b) %*% solve(V, b))
  df <- length(idx)
  pval <- 1 - pchisq(stat, df)
  list(stat = stat, df = df, p = pval)
}

# Average partial effect of switching `col_name` between value0/value1,
# integrating out the random intercept via the closed form
# E_alpha[Phi(x'beta+alpha)] = Phi(x'beta / sqrt(1+sigma_alpha^2)).
# `interacted_cols`: named list {other_col: multiplier vector} for columns
# that are themselves col_name*multiplier (e.g. year x lag, group x lag) --
# must be recomputed consistently with the counterfactual swap or the
# interaction's contribution silently drops out of the counterfactual.
ape <- function(fit, col_name, X_override = NULL, value1 = 1, value0 = 0, interacted_cols = NULL) {
  X <- if (is.null(X_override)) fit$X else X_override
  j <- match(col_name, fit$X_cols)
  inter_idx <- list()
  if (!is.null(interacted_cols)) {
    for (nm in names(interacted_cols)) inter_idx[[as.character(match(nm, fit$X_cols))]] <- interacted_cols[[nm]]
  }

  ape_of <- function(theta) {
    P <- ncol(X)
    b <- theta[1:P]
    sigma <- exp(theta[P + 1])
    denom <- sqrt(1 + sigma^2)
    X1 <- X; X1[, j] <- value1
    X0 <- X; X0[, j] <- value0
    for (k_chr in names(inter_idx)) {
      k <- as.integer(k_chr)
      X1[, k] <- inter_idx[[k_chr]] * value1
      X0[, k] <- inter_idx[[k_chr]] * value0
    }
    p1 <- pnorm(as.numeric(X1 %*% b) / denom)
    p0 <- pnorm(as.numeric(X0 %*% b) / denom)
    mean(p1 - p0)
  }

  point <- ape_of(fit$theta)
  eps <- 1e-5
  grad <- numeric(length(fit$theta))
  for (i in seq_along(fit$theta)) {
    step <- eps * max(1, abs(fit$theta[i]))
    th1 <- fit$theta; th1[i] <- th1[i] + step
    th0 <- fit$theta; th0[i] <- th0[i] - step
    grad[i] <- (ape_of(th1) - ape_of(th0)) / (2 * step)
  }
  var <- as.numeric(t(grad) %*% fit$vcov %*% grad)
  se <- sqrt(max(var, 0))
  list(ape = point, se = se, ci_lo = point - 1.96 * se, ci_hi = point + 1.96 * se)
}
