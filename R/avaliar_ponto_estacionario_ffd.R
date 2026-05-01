#' Avalia o ponto estacionário em relação ao objetivo
#'
#' @param modelo Objeto retornado por fit_ffd().
#' @param objetivo "min" para minimizar ou "max" para maximizar.
#'
#' @return Lista com avaliação interpretativa do ponto estacionário.
#' @export
avaliar_ponto_estacionario_ffd <- function(modelo, objetivo = c("min", "max")) {

  if (missing(modelo)) {
    stop("Informe o modelo ajustado.")
  }

  if (!inherits(modelo, "lm")) {
    stop("O objeto deve ser um modelo do tipo lm.")
  }

  objetivo <- match.arg(objetivo)

  pe <- ponto_estacionario_ffd(modelo)

  classificacao <- pe$classificacao
  dentro_regiao <- !is.null(pe$aviso) &&
    identical(pe$aviso, "Ponto dentro da região experimental")

  decisao <- if (!dentro_regiao) {
    "O ponto estacionário está fora da região experimental. Recomenda-se usar a otimização numérica dentro da região estudada."
  } else if (objetivo == "min" && classificacao == "mínimo") {
    "O ponto estacionário é compatível com o objetivo de minimização."
  } else if (objetivo == "max" && classificacao == "máximo") {
    "O ponto estacionário é compatível com o objetivo de maximização."
  } else if (classificacao == "sela") {
    "O ponto estacionário é um ponto de sela. Recomenda-se avaliar o ponto ótimo numérico dentro da região experimental."
  } else {
    "O ponto estacionário não é compatível com o objetivo informado. Recomenda-se avaliar o ponto ótimo numérico."
  }

  resultado <- list(
    objetivo = objetivo,
    classificacao = classificacao,
    aviso = pe$aviso,
    resposta_estimada = pe$resposta_estimada,
    decisao = decisao,
    ponto = pe$ponto
  )

  class(resultado) <- "avaliar_ponto_estacionario_ffd"

  return(resultado)
}
