#' Fonction de masse de probabilité des distributions de fréquence
#'
#' Évalue la fonction de masse de probabilité (fmp) d'une distribution discrète
#' aux valeurs \code{k}. Retourne une erreur si les probabilités ne somment pas à 1,
#' assurant que le support fourni est complet.
#'
#' @param k Vecteur d'entiers. Valeurs auxquelles évaluer la fmp.
#' @param dist Chaîne de caractères. Nom de la distribution — un parmi \code{"Pois"},
#'   \code{"Bin"}, \code{"BinNeg"}, \code{"Geo"} (insensible à la casse).
#' @param ... Paramètres supplémentaires transmis à la fonction \code{d*()} correspondante :
#'   \describe{
#'     \item{\code{"pois"} : \code{lambda}.}
#'     \item{\code{"bin"} : \code{size}, \code{prob}.}
#'     \item{\code{"binneg"} : \code{size}, \code{prob}.}
#'     \item{\code{"geo"} : \code{prob}.}
#'   }
#'
#' @return Fonction de masse de probabilité de la loi choisit.
#'
#' @examples
#' freq_dist(0:20, "pois", lambda = 5)
#' freq_dist(0:10, "bin", size = 10, prob = 0.3)
#' freq_dist(0:30, "binneg", 3, 0.4)
#' freq_dist(0:50, "geo", 0.3)
#'
#' @seealso \code{\link[stats]{dpois}}, \code{\link[stats]{dbinom}},
#'   \code{\link[stats]{dnbinom}}, \code{\link[stats]{dgeom}}
#'
#' @export
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
