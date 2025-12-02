medidas <- function(x, rf = 0) {
  # Annualized Average
  AV <- mean(x, na.rm = TRUE)
  # Annualized SD
  SD <- sd(x, na.rm = TRUE)
  # Information (or Sharpe) Ratio
  SR <- (mean(x, na.rm = TRUE) - rf)/sd(x, na.rm = TRUE)
  # Adjusted Sharpe Ratio
  ASR <- SR*(1 + (moments::skewness(x, na.rm = TRUE)/6)*SR - ((moments::kurtosis(x, na.rm = TRUE) - 3)/24)*SR^2)
  # Sortino Ratio
  SO <- (mean(x, na.rm = TRUE) - rf)/sqrt(mean(ifelse(x - rf < 0, 0, (x - rf)^2), na.rm = TRUE))
  output <- c(252*AV, sqrt(252)*SD, sqrt(252)*SR, sqrt(252)*ASR, sqrt(252)*SO)
  return(output)
}

calculate_to<- function(previous_weights, desired_weights, oos_returns, p) {
  num <- previous_weights*(1 + oos_returns/100)
  den <- sum(num, na.rm = TRUE)
  updated_weights <- num/den
  desired_weights[is.na(desired_weights)] <- 0
  updated_weights[is.na(updated_weights)] <- 0
  to <- sum(abs(desired_weights - updated_weights))
  return(to)
}

cvc_shrinkage <- function(R) {
  S <- cov(R)
  R_c <- scale(R, scale = FALSE)
  p <- ncol(S)
  n <- nrow(R)
  ones <- matrix(1, p, p)
  
  phi <- mean(diag(S))
  nu <- mean(S[row(S) != col(S)]) 
  
  Fcvc <-  matrix(nu, p, p)
  diag(Fcvc) <- phi
  
  zeta2 <- sum((Fcvc- S)^2)
  tau2 <- min(sum(sapply(1:n, function(t) sum((tcrossprod(R_c[t, ]) - S)^2)))/n^2,  zeta2)
  delta <- tau2 / zeta2
  
  return(delta * Fcvc + (1 - delta) * S)
  
}


cov_nlshrink <- function(X) {
  X <- as.matrix(X)
  results <- tryCatch({
    nlshrink_cov(X, method = "nlminb")
  }, error = function(e) {
    nlshrink_cov(X, method = "nloptr")
  })
  return(results)
}


HCAA_Portfolio2 <- function(covar, linkage = "ward", graph = FALSE, clusters = NULL) {
  if (linkage %in% c("single", "complete", "average", "ward")) {
    if (linkage == "ward") {
      linkage = "ward.D2"
    }
  } else {
    return("ERROR: linkage argument only supports 'single', 'complete', 'average' or 'ward' options")
  }
  corre <- stats::cov2cor(covar)
  distance <- sqrt(0.5 * (1 - corre))
  euclidean_distance <- stats::dist(distance, method = "euclidean", diag = TRUE, upper = TRUE, p = 2)
  clustering <- fastcluster::hclust(euclidean_distance , method = linkage, members = NULL)
  n_cols <- ncol(corre)
  if (is.null(clusters)) {
    fun_clus_num <- function(x,k) list(cluster = stats::cutree(fastcluster::hclust(stats::as.dist(x) , method = linkage, members = NULL), k))
    gap <- cluster::clusGap(as.matrix(euclidean_distance), FUN = fun_clus_num, K.max = floor(n_cols/2), B = 100)
    n_clusters <- cluster::maxSE(gap$Tab[,"gap"], gap$Tab[,"SE.sim"], method = "Tibs2001SEmax")
    n_clusters <- max(2, n_clusters)
  } else{
    n_clusters <- clusters
  }
  print(sprintf("Number of clusters: %i", n_clusters))
  elements_in_cluster <- matrix(stats::cutree(clustering, 2:n_clusters), ncol = n_clusters - 1)
  # Using cluster's hierarchy
  indexa <- elements_in_cluster[,1] == 1
  indexb <- elements_in_cluster[,1] == 2
  weights <- rep(1, n_cols)
  weights[indexa] <- weights[indexa]/2
  weights[indexb] <- weights[indexb]/2
  if (n_clusters > 2) {
    for (i in 2:(n_clusters - 1)) {
      groups <- unique(elements_in_cluster[,i - 1])
      for (j in 1:i) {
        index <- elements_in_cluster[, i - 1] == groups[j]
        if (length(unique(elements_in_cluster[index, i])) > 1) {
          sub_groups <- unique(elements_in_cluster[index, i])
          indexa <- elements_in_cluster[ ,i] == sub_groups[1]
          indexb <- elements_in_cluster[ ,i] == sub_groups[2]
          weights[indexa] <- weights[indexa]/2
          weights[indexb] <- weights[indexb]/2
          break
        }
      }
    }
  }
  for (j in 1:n_clusters) {
    index <- elements_in_cluster[, n_clusters - 1] == j
    weights[index] <- weights[index]/sum(index)
  }
  if (graph) plot(clustering, xlab = "", ylab = "", main = "Cluster Dendrogram - HCAA")
  weights <- data.frame(weights)
  row.names(weights) <- colnames(covar)
  return(list(weights, n_clusters))
}