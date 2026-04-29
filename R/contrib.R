#' Contributions aux mesures de risque VaR et TVaR
#'
#' Calcule la contribution de la v.a. \eqn{M_i} à la VaR et TVaR
#' de \eqn{S = M_1 + \cdots + M_d}.
#'
#' @param u Niveau de confiance.
#' @param fi Matrice où chaque colonne est la fonction de masse
#'   de probabilité de \eqn{M_i}.
#'
#' @return Une liste contenant :
#' \describe{
#'   \item{V.a. S}{VaR et TVaR de \eqn{S}.}
#'   \item{CVaR}{Vecteur des contributions à la VaR.}
#'   \item{CTVaR}{Vecteur des contributions à la TVaR.}
#' }
#'
#' @examples
#' k <- 0:1000
#' n <- c(200, 300)
#' q <- c(0.15, 1/15)
#' u <- 0.99
#'
#' fm1 <- dbinom(k, n[1], q[1])
#' fm2 <- dbinom(k, n[2], q[2])
#' fmi <- cbind(fm1, fm2)
#' contrib(u = 0.95, fmi)
#'
#' @export
contrib <- function(u, fi){
  if(is.null(ncol(fi)) || ncol(fi) < 2){
    stop("La dimension doit être de minimum 2")
  }
  if(!isTRUE(all.equal(colSums(fi) |> unname(), rep(1, ncol(fi))))){
    stop("La fonction de densité doit sommer à 1")
  }

  nfft <- 2^ceiling(log2(nrow(fi)))
  d <- ncol(fi)
  k <- 0:(nfft - 1)
  fi <- apply(fi, 2, function(col) replace(numeric(nfft), 1:nrow(fi), col))

  ## Esperance
  Ei <- apply(fi, 2, function(f) sum(k*f))

  ## Variable S
  fs <- convo(k, fi)
  Fs <- cumsum(fs)
  VaRS <- k[min(which(Fs >= u))]
  TVaRS <- TVaRS <- VaRS + 1/(1 - u)*sum(pmax(k - VaRS, 0) * fs)

  ## E[Mi|S = k]
  gi <- k*sweep(fi, 2, Ei, "/")

  fait <- sapply(1:d, function(i) fft(gi[, i])*apply(mvfft(fi[, -i, drop=FALSE]), 1, prod))
  fai <- Re(mvfft(fait, TRUE))/nfft

  Eai <- sweep(fai, 2, Ei, "*")/fs

  ## Contribution au mesure de risque VaR et TVaR
  CVaRi <- sapply(1:d, function(i) Eai[VaRS + 1, i])

  Fami <- apply(fai, 2, cumsum)
  CTVaRi <- sapply(1:d, function(i)
    (Ei[i]*(1 - Fami[VaRS + 1, i]) + CVaRi[i] * (Fs[VaRS + 1] - u))/(1 - u))
  list('V.a. S' = c("VaRS"=VaRS, "TVaRS"=TVaRS),
       CVaR=unname(CVaRi),
       CTVaR=unname(CTVaRi)
       )
}
