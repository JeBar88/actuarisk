#' Fonction de masse de probabilité avec franchise et limite
#'
#' Calcule la fonction de masse de probabilité d'une v.a. discrète
#' après application d'une franchise et d'une limite.
#'
#' @param d Un scalaire numérique représentant la limite maximale de couverture.
#' @param u Un scalaire numérique représentant le déductible.
#' @param k Un vecteur numérique représentant le domaine de la variable aléatoire.
#' @param f Fonction de masse de probabilité. Doit sommer à 1.
#'
#' @return Un vecteur numérique représentant la fonction de masse de probabilité
#'   de la variable aléatoire transformée \code{min(max(k - fran, 0), lim)}.
#'
#' @examples
#' k <- 0:1000
#' r <- 2.5
#' q <- 0.7
#' d <- 2
#' u <- 8
#' fx <- dnbinom(k, r, q)
#' dloi_lim_fran(d, u, k, fx)
#' @export
dloi_lim_fran <- function(d, u, k, f){
  if(!isTRUE(all.equal(sum(f), 1))){
    stop("La fonction de densité doit sommer à 1")
  }
  k <- pmin(pmax(k - d, 0), u)
  tapply(f, k, sum)
}
