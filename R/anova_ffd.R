#' Calcula ANOVA do modelo fatorial
#'
#' @param modelo Objeto retornado por fit_ffd()
#'
#' @return Tabela de ANOVA
#' @export
#'
#' @examples
#' # anova_ffd(fit)
anova_ffd <- function(modelo) {

  if (missing(modelo)) {
    stop("Informe o modelo ajustado.")
  }

  if (!inherits(modelo, "lm")) {
    stop("O objeto deve ser um modelo do tipo lm.")
  }

  aov_tab <- stats::anova(modelo)

  aov_tab <- as.data.frame(aov_tab)

  aov_tab$Termo <- rownames(aov_tab)

  rownames(aov_tab) <- NULL

  # reorganiza
  aov_tab <- aov_tab[, c("Termo", names(aov_tab)[names(aov_tab) != "Termo"])]

  return(aov_tab)
}
