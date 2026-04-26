#' Fonction de répartition de la distribution composée
#'
#' Évalue la cdf de la distribution composée \eqn{F_X(x)} où la fréquence suit
#' une distribution discrète quelconque et la sévérité suit une loi Exponentielle ou Gamma.
#'
#' \deqn{F_X(x) = f_M(0) + \sum_{k=1}^{\infty} f_m(k) H(x;\, \alpha k, \beta)}
#'
#' @param x Numérique. Point auquel évaluer la cdf
#' @param k Domaine de la distribution de fréquence.
#' @param a Paramètre de forme de la loi Gamma. Par défaut \code{1}.
#' @param b Paramètre de taux (\emph{rate}) de la loi Gamma.
#' @param freq Distribution de fréquence — un parmi \code{"pois"}, \code{"bin"},
#'   \code{"binneg"}, \code{"geo"}.
#' @param ... Paramètres supplémentaires transmis pour la distribution de fréquence.
#'
#' @return Valeur de la CDF en \code{x}.
#'
#' @examples
#' pcomp(50,  0:20, a = 2, b = 0.02, freq = "pois", lambda = 5)
#' pcomp(100, 0:10, b = 0.05, freq = "bin",  size = 10, prob = 0.3)
#'
#' @seealso \code{\link{freq_dist}}, \code{\link[stats]{pgamma}}
#'
#' @export
pcomp <- function(x, k, a = 1, b, freq, ...) {
  f <- freq_dist(k, freq, ...)
  f[1] + sum(f[-1] * pgamma(x, k[-1] * a, rate = b))
}
pcomp <- function(x, k, a = 1, b, freq, ...) {
  f <- freq_dist(k, freq, ...)
  f[1] + sum(f[-1] * pgamma(x, k[-1] * a, rate = b))
}
