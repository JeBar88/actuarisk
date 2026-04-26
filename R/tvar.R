#' Tail Value at Risk (TVaR)
#'
#' Calcule la TVaR à un niveau de confiance donné à partir
#' de la fonction de masse de probabilité pour une v.a. discrète.
#'
#' @param u Un scalaire numérique représentant le niveau de confiance (par
#'   exemple, 0.95 pour 95%).
#' @param f Fonction de masse de probabilité. Doit sommer à 1.
#' @param k Un vecteur numérique représentant le domaine variable aléatoire.
#'
#' @return Un scalaire numérique représentant la TVaR.
#'
#' @examples
#' f <- c(0.1, 0.2, 0.4, 0.2, 0.1)
#' k <- 0:4
#' TVaR(u = 0.95, k, f)
#'
#' @export
TVaR <- function(u, k, f){
  if(!isTRUE(all.equal(sum(f), 1))){
    stop("La fonction de densité doit sommer à 1")
  }
  d <- VaR(f, u)
  d + sum(pmax(k - d, 0) * f)/(1 - u)
}

