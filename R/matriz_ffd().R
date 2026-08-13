#' Gera matriz de planejamento fatorial completo
#'
#' @param fatores Vetor com os nomes dos fatores
#' @param niveis Número de níveis (2 ou 3)
#' @param replicas Número de réplicas (default = 1)
#' @param exportar Se TRUE, exporta para Excel
#' @param caminho Caminho para salvar o arquivo
#'
#' @return Data frame com a matriz
#' @export
matriz_ffd <- function(fatores,
                       niveis = 2,
                       replicas = 1,
                       exportar = FALSE,
                       caminho = NULL) {

  if (missing(fatores)) {
    stop("Informe os nomes dos fatores.")
  }

  if (!is.character(fatores)) {
    stop("O argumento 'fatores' deve ser texto.")
  }

  if (length(fatores) < 1 || length(fatores) > 4) {
    stop("Use de 1 a 4 fatores.")
  }

  if (!niveis %in% c(2, 3)) {
    stop("niveis deve ser 2 ou 3.")
  }

  if (!is.numeric(replicas) || replicas < 1) {
    stop("replicas deve ser >= 1.")
  }

  replicas <- as.integer(replicas)

  valores <- if (niveis == 2) c(-1, 1) else c(-1, 0, 1)

  lista_niveis <- rep(list(valores), length(fatores))

  base <- expand.grid(lista_niveis)
  names(base) <- fatores

  matriz <- base[rep(seq_len(nrow(base)), times = replicas), , drop = FALSE]

  matriz$Replica <- rep(seq_len(replicas), each = nrow(base))
  matriz$Ensaio <- seq_len(nrow(matriz))

  matriz <- matriz[, c("Ensaio", "Replica", fatores)]

  rownames(matriz) <- NULL

  # 🔥 EXPORTAÇÃO
  if (exportar) {

    if (is.null(caminho)) {
      caminho <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
    }

    nome_arquivo <- paste0(
      "matriz_ffd_",
      format(Sys.time(), "%Y-%m-%d_%H-%M"),
      ".xlsx"
    )

    arquivo <- file.path(caminho, nome_arquivo)

    openxlsx::write.xlsx(matriz, arquivo)

    message("Arquivo exportado em: ", arquivo)
  }

  return(matriz)
}

