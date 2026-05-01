#' Calcula métricas do modelo fatorial
#'
#' @param modelo Objeto retornado por fit_ffd()
#'
#' @return Data frame com métricas do modelo ajustado
#' @export
#'
#' @examples
#' # metricas_ffd(fit)
metricas_ffd <- function(modelo) {

  if (missing(modelo)) {
    stop("Informe o modelo ajustado.")
  }

  if (!inherits(modelo, "lm")) {
    stop("O objeto deve ser um modelo do tipo lm.")
  }

  s <- summary(modelo)

  metricas <- data.frame(
    Metrica = c(
      "R2",
      "R2_ajustado",
      "Erro_padrao_residual",
      "GL_residual",
      "Numero_observacoes"
    ),
    Valor = c(
      s$r.squared,
      s$adj.r.squared,
      s$sigma,
      modelo$df.residual,
      length(modelo$fitted.values)
    )
  )

  return(metricas)
}
