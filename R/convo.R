#' Convolution de distributions indépendantes
#'
#' Calcule la fonction de masse de probabilité de la somme de variables
#' aléatoires discrètes indépendantes à partir de leurs fonctions de masse de probabilité.
#'
#' @param k Un vecteur numérique représentant le domaine de la v.a. Il s'agit également
#'   du domaine de la somme alors il doit être choisit en conséquence.
#' @param fi Une matrice dont chaque colonne représente la fonction de masse de
#'   probabilité d'une variable aléatoire, ou un vecteur si toutes les variables
#'   aléatoires sont identiques. Chaque colonne (ou le vecteur) doit sommer à 1.
#' @param method Une chaîne de caractères indiquant la méthode de calcul.
#'   \code{"fft"} utilise la transformée de Fourier rapide et \code{"brute"}
#'   utilise la convolution directe. Par défaut \code{"fft"}. Lorsque \code{fi}
#'   est un vecteur, seule la méthode \code{"fft"} est disponible.
#' @param n Un entier indiquant le nombre de fois que la variable aléatoire est
#'   sommée. Obligatoire lorsque \code{fi} est un vecteur, ignoré sinon.
#'
#' @return Un vecteur numérique représentant la fonction de masse de probabilité
#'   de la somme des variables aléatoires.
#'
#' @examples
#' k <- 0:8
#' fm1 <- c(0.1, 0.2, 0.4, 0.2, 0.1)
#' fm2 <- c(0.2, 0.3, 0.3, 0.1, 0.1)
#' fmi <- cbind(fm1, fm2)
#' convo(k, fmi, method = "fft")
#'
#' # Somme de n variables aléatoires identiques
#' fm <- c(0.1, 0.2, 0.4, 0.2, 0.1)
#' convo(k, fm, n = 2)
#'
#' @export
convo <- function(k, fi, method = "fft", n = NULL){
  if(is.vector(fi)){
    if(method != "fft"){
      stop("Lorsque fi est un vecteur, seule la méthode 'fft' est disponible")
    }
    if(is.null(n)){
      stop("L'argument n doit être spécifié lorsque fi est un vecteur")
    }
    if(!isTRUE(all.equal(sum(fi), 1))){
      stop("La fonction de densité doit sommer à 1")
    }
    nfft <- 2^ceiling(log2(length(k)))
    fi <- replace(numeric(nfft), 1:length(fi), fi)
    fit <- fft(fi)
    fst <- fit^n
    fs <- Re(fft(fst, inverse = TRUE))/nfft
    fs <- fs[1:length(k)]
  } else {
    if(!isTRUE(all.equal(colSums(fi) |> unname(), rep(1, ncol(fi))))){
      stop("La fonction de densité doit sommer à 1")
    }
    if(method == "fft"){
      nfft <- 2^ceiling(log2(length(k)))
      fi <- apply(fi, 2, function(col) replace(numeric(nfft), 1:nrow(fi), col))
      fit <- mvfft(fi)
      fst <- apply(fit, 1, prod)
      fs <- Re(fft(fst, inverse = TRUE))/nfft
      fs <- fs[1:length(k)]
    } else if(method == "brute"){
      fs <- fi[, 1]
      for (i in 2:ncol(fi)) {
        fs <- sapply(k, function(k) sum(fs[0:k + 1] * fi[k:0 + 1, i]))
      }
    } else {
      stop("La méthode doit être 'fft' ou 'brute'")
    }
  }
  fs
}





