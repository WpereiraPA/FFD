#' Gráfico de Pareto dos efeitos padronizados no FFD
#'
#' @param modelo Objeto retornado por fit_ffd().
#' @param alpha Nível de significância.
#'
#' @return Invisivelmente, a tabela usada no gráfico.
#' @export
pareto_ffd <- function(modelo, alpha = 0.05) {

  if (missing(modelo)) {
    stop("Informe o modelo ajustado.")
  }

  if (!inherits(modelo, "lm")) {
    stop("O objeto deve ser um modelo do tipo lm.")
  }

  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("O argumento 'alpha' deve ser numérico entre 0 e 1.")
  }

  s <- summary(modelo)

  tab <- as.data.frame(s$coefficients)
  tab$Termo <- rownames(tab)
  rownames(tab) <- NULL

  tab <- tab[tab$Termo != "(Intercept)", , drop = FALSE]

  formatar_termo <- function(x) {
    x <- as.character(x)
    x <- gsub(":", " × ", x, fixed = TRUE)
    x <- gsub("I\\(([^\\)]+)\\^2\\)", "\\1²", x)
    x
  }

  tab$TermoBonito <- vapply(tab$Termo, formatar_termo, character(1))

  tab$EfeitoPadronizado <- abs(tab$`t value`)

  tab <- tab[order(tab$EfeitoPadronizado), , drop = FALSE]

  linha_sig <- stats::qt(1 - alpha / 2, df = modelo$df.residual)

  nome_resp <- attr(modelo, "ffd_resposta")

  if (is.null(nome_resp) || !nzchar(nome_resp)) {
    nome_resp <- all.vars(stats::formula(modelo))[1]
  }

  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar))

  graphics::par(mar = c(4.3, 7, 5, 2))

  bp <- graphics::barplot(
    tab$EfeitoPadronizado,
    names.arg = tab$TermoBonito,
    horiz = TRUE,
    col = "cornflowerblue",
    border = "white",
    las = 1,
    xlab = "Efeitos padronizados (|t|)",
    ylab = "",
    mgp = c(2.1, 0.6, 0),
    xlim = c(0, max(tab$EfeitoPadronizado, linha_sig) * 1.18),
    main = paste0(
      "Pareto dos Efeitos Padronizados\n(",
      nome_resp, "; α = ", alpha, ")"
    )
  )

  graphics::title(ylab = "Termos", line = 3.4)

  graphics::text(
    x = tab$EfeitoPadronizado + max(tab$EfeitoPadronizado) * 0.03,
    y = bp,
    labels = format(
      round(tab$EfeitoPadronizado, 3),
      decimal.mark = ","
    ),
    cex = 0.85,
    pos = 4,
    xpd = NA
  )

  graphics::abline(v = linha_sig, col = "red", lwd = 2, lty = 2)

  graphics::text(
    x = linha_sig,
    y = graphics::par("usr")[4] +
      (graphics::par("usr")[4] - graphics::par("usr")[3]) * 0.018,
    labels = format(
      round(linha_sig, 3),
      decimal.mark = ","
    ),
    col = "red",
    cex = 1.1,
    font = 2,
    xpd = NA
  )

  invisible(tab)
}
