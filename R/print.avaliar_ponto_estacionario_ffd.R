#' Imprime avaliação do ponto estacionário FFD
#'
#' @param x Objeto da classe avaliar_ponto_estacionario_ffd.
#' @param ... Argumentos adicionais.
#'
#' @return Invisivelmente, o próprio objeto.
#' @export
print.avaliar_ponto_estacionario_ffd <- function(x, ...) {

  cat("\nAvaliação do ponto estacionário FFD\n")
  cat("-----------------------------------\n\n")

  cat("Objetivo:", ifelse(x$objetivo == "min", "Minimizar", "Maximizar"), "\n")
  cat("Classificação:", x$classificacao, "\n")
  cat("Observação:", x$aviso, "\n\n")

  cat("Coordenadas codificadas:\n")

  for (nm in names(x$ponto)) {
    cat(
      nm, "=",
      format(round(x$ponto[[nm]], 4), decimal.mark = ","),
      "\n"
    )
  }

  cat("\nResposta estimada: ",
      format(round(x$resposta_estimada, 4), decimal.mark = ","),
      "\n", sep = "")

  cat("\nDecisão:\n")
  cat(x$decisao, "\n")

  invisible(x)
}
