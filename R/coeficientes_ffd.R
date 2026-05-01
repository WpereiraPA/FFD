#' Extrai coeficientes do modelo fatorial
#'
#' @param modelo Objeto retornado por fit_ffd()
#'
#' @return Data frame com os coeficientes do modelo
#' @export
#'
#' @examples
#' # coeficientes_ffd(fit)
coeficientes_ffd <- function(modelo) {

  if (missing(modelo)) {
    stop("Informe o modelo ajustado.")
  }

  if (!inherits(modelo, "lm")) {
    stop("O objeto deve ser um modelo do tipo lm.")
  }

  tab <- summary(modelo)$coefficients

  tab <- as.data.frame(tab)

  tab$Termo <- rownames(tab)

  rownames(tab) <- NULL

  tab <- tab[, c("Termo", names(tab)[names(tab) != "Termo"])]

  names(tab) <- c("Termo", "Estimativa", "Erro_Padrao", "t_valor", "p_valor")

  return(tab)
}
