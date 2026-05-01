#' Ajusta modelo para planejamento fatorial completo
#'
#' @param dados Data frame com os dados experimentais.
#' @param resposta Nome da variável resposta.
#' @param fatores Vetor com os nomes dos fatores.
#'
#' @return Objeto lm com o modelo ajustado.
#' @export
fit_ffd <- function(dados, resposta, fatores) {

  if (missing(dados)) stop("Informe os dados.")
  if (missing(resposta)) stop("Informe a resposta.")
  if (missing(fatores)) stop("Informe os fatores.")

  if (!resposta %in% names(dados)) {
    stop("Resposta não encontrada.")
  }

  if (!all(fatores %in% names(dados))) {
    stop("Fatores não encontrados.")
  }

  dados[fatores] <- lapply(dados[fatores], function(x) as.numeric(as.character(x)))

  niveis_por_fator <- lapply(dados[fatores], function(x) sort(unique(x)))

  tem_tres_niveis <- any(sapply(niveis_por_fator, function(x) {
    any(x == 0, na.rm = TRUE) || length(x) >= 3
  }))

  if (tem_tres_niveis) {

    termos_lineares <- paste(fatores, collapse = " + ")

    termos_quadraticos <- paste0("I(", fatores, "^2)", collapse = " + ")

    termos_interacao <- NULL

    if (length(fatores) >= 2) {
      combinacoes <- utils::combn(fatores, 2)
      termos_interacao <- apply(combinacoes, 2, paste, collapse = ":")
      termos_interacao <- paste(termos_interacao, collapse = " + ")
    }

    termos <- paste(
      c(termos_lineares, termos_quadraticos, termos_interacao),
      collapse = " + "
    )

    formula <- as.formula(paste(resposta, "~", termos))

  } else {

    termos <- paste(fatores, collapse = " * ")

    formula <- as.formula(paste(resposta, "~", termos))
  }

  modelo <- lm(formula, data = dados)

  attr(modelo, "ffd_formula") <- formula
  attr(modelo, "ffd_fatores") <- fatores
  attr(modelo, "ffd_resposta") <- resposta
  attr(modelo, "ffd_tipo") <- ifelse(
    tem_tres_niveis,
    "3 níveis, modelo quadrático com interações",
    "2 níveis, modelo linear com interações"
  )

  return(modelo)
}
