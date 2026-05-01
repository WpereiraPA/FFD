#' Gráfico de superfície de resposta para FFD
#'
#' @param modelo Objeto retornado por fit_ffd().
#' @param x1 Nome do primeiro fator.
#' @param x2 Nome do segundo fator.
#' @param n Número de pontos da grade.
#'
#' @return Invisivelmente, uma lista com grade e matriz de predições.
#' @export
superficie_ffd <- function(modelo, x1, x2, n = 45) {

  if (missing(modelo)) stop("Informe o modelo ajustado.")
  if (!inherits(modelo, "lm")) stop("O objeto deve ser um modelo do tipo lm.")

  if (missing(x1) || missing(x2)) {
    stop("Os argumentos 'x1' e 'x2' são obrigatórios.")
  }

  fatores <- attr(modelo, "ffd_fatores")

  if (is.null(fatores)) {
    stop("Não foi possível identificar os fatores do modelo.")
  }

  if (!all(c(x1, x2) %in% fatores)) {
    stop("x1 e x2 precisam estar entre os fatores do modelo.")
  }

  if (x1 == x2) {
    stop("x1 e x2 devem ser diferentes.")
  }

  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 10) {
    stop("O argumento 'n' deve ser numérico e maior ou igual a 10.")
  }

  xs <- seq(-1, 1, length.out = n)
  ys <- seq(-1, 1, length.out = n)

  grade <- expand.grid(xs, ys, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  names(grade) <- c(x1, x2)

  outros_fatores <- setdiff(fatores, c(x1, x2))

  if (length(outros_fatores) > 0) {
    for (f in outros_fatores) {
      grade[[f]] <- 0
    }
  }

  grade <- grade[, fatores, drop = FALSE]

  z <- tryCatch(
    stats::predict(modelo, newdata = grade),
    error = function(e) {
      stop("Não foi possível gerar as predições para o gráfico de superfície.")
    }
  )

  zmat <- matrix(z, nrow = n, ncol = n)
  zlim <- range(zmat, na.rm = TRUE)

  pal <- grDevices::colorRampPalette(
    c("darkgreen", "green3", "chartreuse3", "yellow2", "goldenrod1", "thistle3")
  )

  cols <- pal(160)

  nrz <- nrow(zmat)
  ncz <- ncol(zmat)

  zfacet <- zmat[-1, -1] +
    zmat[-1, -ncz] +
    zmat[-nrz, -1] +
    zmat[-nrz, -ncz]

  zfacet <- c(zfacet / 4, zlim)

  idx <- cut(
    zfacet,
    breaks = length(cols),
    include.lowest = TRUE,
    labels = FALSE
  )

  facetcol <- cols[idx]

  nome_resp <- attr(modelo, "ffd_resposta")

  if (is.null(nome_resp) || !nzchar(nome_resp)) {
    nome_resp <- all.vars(stats::formula(modelo))[1]
  }

  graphics::persp(
    x = xs,
    y = ys,
    z = zmat,
    zlim = zlim,
    theta = 55,
    phi = 26,
    r = 3.00,
    expand = 0.85,
    col = facetcol,
    border = grDevices::adjustcolor("black", alpha.f = 0.35),
    ticktype = "detailed",
    shade = 0.5,
    ltheta = 50,
    lphi = 25,
    xlab = x1,
    ylab = x2,
    zlab = paste0("\n", nome_resp),
    cex.lab = 1.0,
    cex.axis = 0.85,
    main = paste("Superfície de Resposta -", nome_resp)
  )

  invisible(
    list(
      x = xs,
      y = ys,
      z = zmat,
      grade = grade
    )
  )
}
