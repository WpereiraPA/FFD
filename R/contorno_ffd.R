#' Gráfico de contorno para FFD com ponto ótimo e estacionário
#'
#' @param modelo Objeto retornado por fit_ffd().
#' @param x1 Nome do primeiro fator.
#' @param x2 Nome do segundo fator.
#' @param n Número de pontos da grade.
#' @param mostrar_otimo Exibir ponto ótimo.
#' @param mostrar_estacionario Exibir ponto estacionário.
#' @param objetivo "min" ou "max".
#'
#' @export
contorno_ffd <- function(
    modelo,
    x1,
    x2,
    n = 60,
    mostrar_otimo = TRUE,
    mostrar_estacionario = TRUE,
    objetivo = "min"
) {

  if (!inherits(modelo, "lm")) stop("Modelo inválido.")

  fatores <- attr(modelo, "ffd_fatores")
  nome_resp <- attr(modelo, "ffd_resposta")

  xs <- seq(-1, 1, length.out = n)
  ys <- seq(-1, 1, length.out = n)

  grade <- expand.grid(xs, ys)
  names(grade) <- c(x1, x2)

  outros <- setdiff(fatores, c(x1, x2))

  for (f in outros) {
    grade[[f]] <- 0
  }

  grade <- grade[, fatores, drop = FALSE]

  z <- predict(modelo, newdata = grade)
  zmat <- matrix(z, nrow = n)

  zmin <- min(zmat)
  zmax <- max(zmat)

  niveis_fill <- seq(zmin, zmax, length.out = 12)
  niveis_rotulo <- pretty(c(zmin, zmax), n = 6)

  pal <- colorRampPalette(
    c("#004d00", "green3", "chartreuse3", "yellow2", "orange", "thistle3")
  )

  cls <- contourLines(xs, ys, zmat, levels = niveis_rotulo)

  # calcular pontos
  ot <- NULL
  if (isTRUE(mostrar_otimo)) {
    ot <- tryCatch(
      otimo_ffd(modelo, objetivo = objetivo),
      error = function(e) NULL
    )
  }

  pe <- NULL
  if (isTRUE(mostrar_estacionario)) {
    pe <- tryCatch(
      ponto_estacionario_ffd(modelo),
      error = function(e) NULL
    )
  }

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(mar = c(5.2, 5.2, 4.2, 8))

  xlim_plot <- range(xs) + c(-0.08, 0.08)
  ylim_plot <- range(ys) + c(-0.08, 0.08)

  filled.contour(
    xs, ys, zmat,
    levels = niveis_fill,
    color.palette = pal,
    xlab = x1,
    ylab = x2,

    xlim = xlim_plot,
    ylim = ylim_plot,

    main = paste("Gráfico de Contorno -", nome_resp),
    plot.axes = {
      axis(1)
      axis(2)

      contour(xs, ys, zmat,
              levels = niveis_rotulo,
              add = TRUE,
              drawlabels = FALSE,
              col = "gray10"
      )

      # rótulos das curvas
      for (cl in cls) {
        i <- round(length(cl$x) / 2)
        text(cl$x[i], cl$y[i],
             labels = format(round(cl$level, 2), decimal.mark = ","),
             cex = 0.8
        )
      }

      # pontos experimentais
      points(
        modelo$model[[x1]],
        modelo$model[[x2]],
        pch = 15,
        cex = 0.7
      )

      legenda <- c("Pontos experimentais")
      cores <- c("black")
      pchs <- c(15)

      # ponto ótimo
      if (!is.null(ot) && all(is.finite(ot$ponto))) {

        px <- ot$ponto[x1]
        py <- ot$ponto[x2]

        if (px >= -1 && px <= 1 && py >= -1 && py <= 1) {

          points(px, py, pch = 19, col = "red", cex = 1.5)
          text(px, py, "Ótimo", pos = 3, col = "red", cex = 0.9)

          legenda <- c(legenda, "Ótimo")
          cores <- c(cores, "red")
          pchs <- c(pchs, 19)
        }
      }

      # ponto estacionário
      if (!is.null(pe) && all(is.finite(as.numeric(pe$ponto)))) {

        px <- as.numeric(pe$ponto[[x1]])
        py <- as.numeric(pe$ponto[[x2]])

        if (px >= -1 && px <= 1 && py >= -1 && py <= 1) {

          rotulo <- if (pe$classificacao == "sela") {
            "Estacionário (sela)"
          } else {
            "Estacionário"
          }

          points(px, py, pch = 17, col = "blue", cex = 1.5)
          text(px, py, rotulo, pos = 4, col = "blue")

          legenda <- c(legenda, rotulo)
          cores <- c(cores, "blue")
          pchs <- c(pchs, 17)
        }
      }

      legend("topright",
             legend = legenda,
             col = cores,
             pch = pchs,
             bty = "n"
      )

      box()
    }
  )


  invisible(list(x = xs, y = ys, z = zmat))
}
