#' Ponto ótimo previsto para FFD
#'
#' @param modelo Objeto retornado por fit_ffd().
#' @param objetivo "min" para minimizar a resposta ou "max" para maximizar.
#'
#' @return Lista com ponto ótimo previsto, resposta estimada, convergência e objetivo.
#' @export
otimo_ffd <- function(modelo, objetivo = c("min", "max")) {

  if (missing(modelo)) {
    stop("Informe o modelo ajustado.")
  }

  if (!inherits(modelo, "lm")) {
    stop("O objeto deve ser um modelo do tipo lm.")
  }

  objetivo <- match.arg(objetivo)

  fatores <- attr(modelo, "ffd_fatores")
  resposta <- attr(modelo, "ffd_resposta")

  if (is.null(fatores) || length(fatores) < 2) {
    stop("Não foi possível identificar os fatores do modelo.")
  }

  dados_modelo <- modelo$model

  if (!all(fatores %in% names(dados_modelo))) {
    stop("Os fatores não foram encontrados nos dados do modelo.")
  }

  lim_inf <- vapply(
    fatores,
    function(f) min(dados_modelo[[f]], na.rm = TRUE),
    numeric(1)
  )

  lim_sup <- vapply(
    fatores,
    function(f) max(dados_modelo[[f]], na.rm = TRUE),
    numeric(1)
  )

  f_obj <- function(par) {

    novo <- as.data.frame(as.list(par))
    names(novo) <- fatores
    novo <- novo[, fatores, drop = FALSE]

    pred <- tryCatch(
      as.numeric(stats::predict(modelo, newdata = novo)),
      error = function(e) NA_real_
    )

    if (is.na(pred) || !is.finite(pred)) {
      return(Inf)
    }

    if (objetivo == "min") pred else -pred
  }

  inicio <- (lim_inf + lim_sup) / 2

  res <- tryCatch(
    stats::optim(
      par = inicio,
      fn = f_obj,
      method = "L-BFGS-B",
      lower = lim_inf,
      upper = lim_sup
    ),
    error = function(e) NULL
  )

  if (is.null(res) || is.null(res$par) || any(!is.finite(res$par))) {

    ponto_otimo <- stats::setNames(rep(NA_real_, length(fatores)), fatores)
    resposta_prevista <- NA_real_
    convergencia <- 1
    valor_otimizado <- NA_real_

  } else {

    ponto_otimo <- res$par
    names(ponto_otimo) <- fatores

    novo_ot <- as.data.frame(as.list(ponto_otimo))
    novo_ot <- novo_ot[, fatores, drop = FALSE]

    resposta_prevista <- tryCatch(
      as.numeric(stats::predict(modelo, newdata = novo_ot)),
      error = function(e) NA_real_
    )

    convergencia <- res$convergence
    valor_otimizado <- if (objetivo == "min") res$value else -res$value
  }
  # Verifica se o ponto ótimo está no limite da região experimental
  tol_limite <- 1e-6

  no_limite <- any(
    abs(ponto_otimo - lim_inf) <= tol_limite |
      abs(ponto_otimo - lim_sup) <= tol_limite,
    na.rm = TRUE
  )

  localizacao <- if (no_limite) {
    "limite"
  } else {
    "interior"
  }

  mensagem <- if (no_limite) {
    "Ótimo localizado no limite da região experimental."
  } else {
    "Ótimo localizado no interior da região experimental."
  }
  resultado <- list(
    ponto = ponto_otimo,
    resposta = resposta_prevista,
    convergencia = convergencia,
    valor_otimizado = valor_otimizado,
    objetivo = objetivo,
    localizacao = localizacao,
    mensagem = mensagem,
    nome_resposta = resposta
  )

  class(resultado) <- "otimo_ffd"

  return(resultado)
}
