#' Imprime ponto estacionário FFD
#'
#' @param x Objeto da classe ponto_estacionario_ffd.
#' @param ... Argumentos adicionais.
#'
#' @return Invisivelmente, o próprio objeto.
#' @export
print.ponto_estacionario_ffd <- function(x, ...) {

  cat("\nPonto estacionário do modelo FFD\n")
  cat("--------------------------------\n\n")

  cat("Classificação:", x$classificacao, "\n")
  cat("Status:", x$status, "\n\n")

  cat("Observação:", x$aviso, "\n\n")

  cat("Coordenadas codificadas:\n")

  for (nm in names(x$ponto)) {
    cat(
      nm, "=",
      format(round(x$ponto[[nm]], 4), decimal.mark = ","),
      "\n"
    )
  }

  cat("\nResposta estimada")

  if (!is.null(x$nome_resposta) && nzchar(x$nome_resposta)) {
    cat(" (", x$nome_resposta, ")", sep = "")
  }

  cat(": ",
      format(round(x$resposta_estimada, 4), decimal.mark = ","),
      "\n", sep = "")

  cat("\nAutovalores:\n")
  for (i in seq_along(x$autovalores)) {
    cat(
      "λ", i, " = ",
      format(round(x$autovalores[i], 6), decimal.mark = ","),
      "\n",
      sep = ""
    )
  }

  invisible(x)
}
