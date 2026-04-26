#' Value at Risk (VaR)
#'
#' Calcule VaR à un niveau de confiance donné à partir
#' de la fonction de masse de probabilité pour une v.a. discrète.
#'
#' @param u Un scalaire numérique représentant le niveau de confiance (par
#'   exemple, 0.95 pour 95%).
#' @param f Fonction de masse de probabilité. Doit sommer à 1.
#' @param k Un vecteur numérique représentant le domaine variable aléatoire.
#'
#' @return Un entier représentant la VaR, c'est-à-dire le plus petit résultat
#'   \code{k} tel que la probabilité cumulée est supérieure ou égale à \code{u}.
#'
#' @examples
#' f <- c(0.1, 0.2, 0.4, 0.2, 0.1)
#' k <- 0:4
#' VaR(u = 0.95, k, f)
#'
#' @export
VaR <- function(u, k, f){
  if(!isTRUE(all.equal(sum(f), 1))){
    stop("La fonction de densité doit sommer à 1")
  }
  k[min(which(cumsum(f) >= u))]
}
