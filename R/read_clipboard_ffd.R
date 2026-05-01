#' Lê dados copiados do Excel via clipboard
#'
#' @description
#' Lê uma tabela copiada diretamente do Excel para dentro do R.
#'
#' @return Um data frame com os dados copiados.
#' @export
#'
#' @examples
#' # Copie uma tabela no Excel e depois execute:
#' # dados <- read_clipboard_ffd()
read_clipboard_ffd <- function() {

  dados <- utils::read.table(
    file = "clipboard",
    header = TRUE,
    sep = "\t",
    dec = ",",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  dados <- as.data.frame(dados)

  message("Dados lidos do clipboard com sucesso.")

  return(dados)
}
