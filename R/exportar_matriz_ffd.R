#' Exporta matriz fatorial para Excel
#'
#' @param matriz Data frame gerado por matriz_ffd()
#' @param caminho Caminho para salvar o arquivo (opcional)
#'
#' @return NULL
#' @export
#'
#' @examples
#' m <- matriz_ffd(fatores = c("A","B"), niveis = 2)
#' exportar_matriz_ffd(m)
exportar_matriz_ffd <- function(matriz, caminho = NULL) {

  if (!is.data.frame(matriz)) {
    stop("O objeto informado não é uma matriz válida.")
  }

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

  message("Matriz exportada em: ", arquivo)
}
