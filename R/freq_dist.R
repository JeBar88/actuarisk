freq_dist <- function(k, dist, ...) {
  f <- switch(tolower(dist),
              "pois"   = dpois(k, ...),
              "bin"    = dbinom(k, ...),
              "binneg" = dnbinom(k, ...),
              "geo"    = dgeom(k, ...),
              stop("Distribution non supportée : '", dist, "'.")
  )
  if (!isTRUE(all.equal(sum(f), 1)))
    stop("La fonction de densité doit sommer à 1")

  f
}
