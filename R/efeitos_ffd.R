#' Calcula efeitos do modelo fatorial
#'
#' @param modelo Objeto retornado por fit_ffd()
#' @param alpha Nível de significância (default = 0.05)
#'
#' @return Data frame com efeitos, tipo e significância
#' @export
#'
#' @examples
#' # efeitos_ffd(fit)
efeitos_ffd <- function(modelo, alpha = 0.05) {

  if (missing(modelo)) {
    stop("Informe o modelo ajustado.")
  }

  if (!inherits(modelo, "lm")) {
    stop("O objeto deve ser um modelo do tipo lm.")
  }

  coef_tab <- summary(modelo)$coefficients
  coef_tab <- as.data.frame(coef_tab)

  coef_tab$Termo <- rownames(coef_tab)
  rownames(coef_tab) <- NULL

  # Remove intercepto
  coef_tab <- coef_tab[coef_tab$Termo != "(Intercept)", ]

  # Renomeia colunas
  names(coef_tab) <- c(
    "Estimativa",
    "Erro_Padrao",
    "t_valor",
    "p_valor",
    "Termo"
  )

  coef_tab <- coef_tab[, c("Termo", "Estimativa", "Erro_Padrao", "t_valor", "p_valor")]

  # Calcula efeito (2 x coeficiente)
  coef_tab$Efeito <- 2 * coef_tab$Estimativa

  # Classificação do tipo de termo
  coef_tab$Tipo <- ifelse(
    grepl("\\^2", coef_tab$Termo),
    "Quadratico",
    ifelse(
      grepl(":", coef_tab$Termo),
      "Interacao",
      "Linear"
    )
  )
  # Ajusta termos quadráticos: A² (A×A)
  coef_tab$Termo <- sapply(coef_tab$Termo, function(term) {

    if (grepl("I\\(.*\\^2\\)", term)) {

      var <- sub("I\\((.*)\\^2\\)", "\\1", term)

      return(paste0(var, "² (", var, "×", var, ")"))

    }

    return(term)
  })

  # Ajusta interação
  coef_tab$Termo <- gsub(":", " × ", coef_tab$Termo)

  # Significância
  coef_tab$Significativo <- ifelse(
    coef_tab$p_valor <= alpha,
    "Sim",
    "Não"
  )

  # Ordena por magnitude do efeito
  coef_tab <- coef_tab[order(abs(coef_tab$Efeito), decreasing = TRUE), ]

  rownames(coef_tab) <- NULL

  return(coef_tab)
}
