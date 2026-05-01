#' Calcular ponto estacionário do modelo FFD
#'
#' @param modelo Objeto retornado por fit_ffd().
#'
#' @return Lista com ponto estacionário, classificação, autovalores,
#' matriz B, resposta estimada e status.
#' @export
ponto_estacionario_ffd <- function(modelo) {

  if (missing(modelo)) {
    stop("Informe o modelo ajustado.")
  }

  if (!inherits(modelo, "lm")) {
    stop("O objeto deve ser um modelo do tipo lm.")
  }

  fatores <- attr(modelo, "ffd_fatores")
  resposta <- attr(modelo, "ffd_resposta")
  coefs <- stats::coef(modelo)
  k <- length(fatores)

  if (is.null(fatores) || k < 2 || k > 4) {
    stop("O modelo precisa ter entre 2 e 4 fatores.")
  }

  b <- stats::setNames(rep(0, k), fatores)
  B <- matrix(0, nrow = k, ncol = k, dimnames = list(fatores, fatores))

  for (f in fatores) {

    if (f %in% names(coefs)) {
      b[f] <- unname(coefs[f])
    }

    termo_quad <- grep(
      paste0("^I\\(", f, "\\^2\\)$"),
      names(coefs),
      value = TRUE
    )

    if (length(termo_quad) == 1) {
      B[f, f] <- 2 * unname(coefs[termo_quad])
    }
  }

  combinacoes <- utils::combn(fatores, 2, simplify = FALSE)

  for (par in combinacoes) {

    padrao <- paste0("^(", par[1], ":", par[2], "|", par[2], ":", par[1], ")$")
    termo_inter <- grep(padrao, names(coefs), value = TRUE)

    beta_ij <- 0

    if (length(termo_inter) == 1) {
      beta_ij <- unname(coefs[termo_inter])
    }

    B[par[1], par[2]] <- beta_ij
    B[par[2], par[1]] <- beta_ij
  }

  autovalores <- tryCatch(
    eigen(B, symmetric = TRUE, only.values = TRUE)$values,
    error = function(e) rep(NA_real_, k)
  )

  tol <- 1e-10

  classificacao <- if (all(is.na(autovalores))) {
    "não determinado"
  } else if (all(autovalores < -tol)) {
    "máximo"
  } else if (all(autovalores > tol)) {
    "mínimo"
  } else {
    "sela"
  }

  solucao <- tryCatch(
    solve(B, b),
    error = function(e) NULL
  )

  if (is.null(solucao) || any(!is.finite(solucao))) {

    ponto_df <- stats::setNames(
      as.data.frame(as.list(rep(NA_real_, k))),
      fatores
    )

    resposta_estimada <- NA_real_
    convergencia <- 1

  } else {

    ponto <- -as.numeric(solucao)
    names(ponto) <- fatores

    ponto_df <- as.data.frame(as.list(ponto))
    ponto_df <- ponto_df[, fatores, drop = FALSE]

    resposta_estimada <- tryCatch(
      as.numeric(stats::predict(modelo, newdata = ponto_df)),
      error = function(e) NA_real_
    )

    convergencia <- ifelse(is.finite(resposta_estimada), 0, 1)
  }

  fora_regiao <- any(
    ponto < -1 | ponto > 1
  )

  aviso <- if (fora_regiao) {
    "Ponto fora da região experimental"
  } else {
    "Ponto dentro da região experimental"
  }

  resultado <- list(
    ponto = ponto_df,
    classificacao = classificacao,
    aviso = aviso,
    autovalores = autovalores,
    matriz_B = B,
    resposta_estimada = resposta_estimada,
    convergencia = convergencia,
    status = if (convergencia == 0) "sucesso" else "falha",
    nome_resposta = resposta
  )

  class(resultado) <- "ponto_estacionario_ffd"

  return(resultado)
}
