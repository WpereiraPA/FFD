#' Imprime resultado do ponto ótimo FFD
#'
#' @param x Objeto da classe otimo_ffd.
#' @param ... Argumentos adicionais.
#'
#' @return Invisivelmente, o próprio objeto.
#' @export
print.otimo_ffd <- function(x, ...) {

  cat("\nPonto ótimo previsto para o planejamento fatorial completo\n")
  cat("---------------------------------------------------------\n\n")

  cat("Objetivo da otimização:",
      ifelse(x$objetivo == "max", "Maximizar", "Minimizar"), "\n\n")

  cat("Coordenadas codificadas:\n")

  for (i in seq_along(x$ponto)) {
    cat(names(x$ponto)[i], "=",
        format(round(x$ponto[i], 4), decimal.mark = ","), "\n")
  }

  cat("\nResposta prevista (", x$nome_resposta, "): ",
      format(round(x$resposta, 4), decimal.mark = ","), "\n", sep = "")

  cat("Convergência:", x$convergencia, "\n")

  if (!is.null(x$mensagem)) {
    cat("\nObservação:\n")
    cat(x$mensagem, "\n")
  }

  invisible(x)
}
