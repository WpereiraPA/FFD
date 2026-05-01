#' Exporta relatório do FFD para Excel
#'
#' Exporta os resultados do planejamento fatorial completo para Excel,
#' incluindo dados, métricas, ANOVA, coeficientes, efeitos e ponto ótimo.
#'
#' @param modelo Objeto retornado por fit_ffd().
#' @param arquivo Caminho do arquivo Excel.
#' @param alpha Nível de significância.
#' @param objetivo Objetivo da otimização. Use "min" para minimizar ou "max" para maximizar.
#'
#' @return Invisivelmente, o caminho do arquivo gerado.
#' @export
exportar_excel_ffd <- function(modelo, arquivo = NULL, alpha = 0.05, objetivo = c("min", "max")) {

  objetivo <- match.arg(objetivo)

  if (is.null(arquivo)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    arquivo <- file.path(
      Sys.getenv("USERPROFILE"),
      "Desktop",
      paste0("relatorio_ffd_", timestamp, ".xlsx")
    )
  }

  .exportar_excel_ffd_base(
    modelo = modelo,
    arquivo = arquivo,
    alpha = alpha,
    objetivo = objetivo,
    incluir_graficos = FALSE
  )
}

#' Exporta relatório completo do FFD para Excel com gráficos
#'
#' @param modelo Objeto retornado por fit_ffd().
#' @param arquivo Caminho completo do arquivo Excel.
#' @param alpha Nível de significância.
#' @param objetivo Objetivo da otimização. Use "min" para minimizar ou "max" para maximizar.
#'
#' @return Invisivelmente, o caminho do arquivo gerado.
#' @export
exportar_excel_completo_ffd <- function(modelo, arquivo = NULL, alpha = 0.05, objetivo = c("min", "max")){

  objetivo <- match.arg(objetivo)

  if (is.null(arquivo)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    arquivo <- file.path(
      Sys.getenv("USERPROFILE"),
      "Desktop",
      paste0("relatorio_completo_ffd_", timestamp, ".xlsx")
    )
  }

  .exportar_excel_ffd_base(
    modelo = modelo,
    arquivo = arquivo,
    alpha = alpha,
    objetivo = objetivo,
    incluir_graficos = TRUE
  )
}



# ==============================================================================
# FUNÇÃO BASE (Faz todo o trabalho pesado)
# ==============================================================================
.exportar_excel_ffd_base <- function(modelo,
                                     arquivo,
                                     alpha,
                                     objetivo,
                                     incluir_graficos = FALSE) {

  # 🔥 FUNÇÃO DE FORMATAÇÃO (Corrigida para não dar Warning no console)
  formatar_num_seguro <- function(x) {
    suppressWarnings({
      num_x <- as.numeric(x)
      ifelse(
        !is.na(num_x),
        format(round(num_x, 4), decimal.mark = ","),
        as.character(x)
      )
    })
  }

  if (!inherits(modelo, "lm")) {
    stop("O objeto deve ser um modelo do tipo lm.")
  }

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Instale o pacote 'openxlsx' para usar esta função.")
  }

  if (!grepl("\\.xlsx$", arquivo, ignore.case = TRUE)) {
    arquivo <- paste0(arquivo, ".xlsx")
  }

  pasta_destino <- dirname(arquivo)

  if (!dir.exists(pasta_destino)) {
    dir.create(pasta_destino, recursive = TRUE)
  }

  fatores <- attr(modelo, "ffd_fatores")
  nome_resp <- attr(modelo, "ffd_resposta")

  if (is.null(fatores)) {
    stop("Não foi possível identificar os fatores do modelo.")
  }

  if (is.null(nome_resp) || !nzchar(nome_resp)) {
    nome_resp <- all.vars(stats::formula(modelo))[1]
  }

  wb <- openxlsx::createWorkbook()

  estilo_cabecalho <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    fgFill = "#D9EAF7"
  )

  estilo_corpo <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight"
  )

  estilo_significativo <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    fgFill = "#FFF2CC",
    fontColour = "#C00000",
    textDecoration = "bold"
  )

  aplicar_estilo_tabela <- function(nome_aba, df) {
    openxlsx::addStyle(
      wb = wb,
      sheet = nome_aba,
      style = estilo_cabecalho,
      rows = 1,
      cols = 1:ncol(df),
      gridExpand = TRUE,
      stack = TRUE
    )
    if (nrow(df) > 0) {
      openxlsx::addStyle(
        wb = wb,
        sheet = nome_aba,
        style = estilo_corpo,
        rows = 2:(nrow(df) + 1),
        cols = 1:ncol(df),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
    openxlsx::freezePane(wb, nome_aba, firstRow = TRUE)
    openxlsx::setColWidths(wb, nome_aba, cols = 1:ncol(df), widths = "auto")
  }

  formatar_termo <- function(x) {
    x <- as.character(x)
    x <- gsub(":", " × ", x, fixed = TRUE)
    x <- gsub("I\\(([^\\)]+)\\^2\\)", "\\1²", x)
    x <- gsub("Residuals", "Resíduos", x)
    x
  }

  dados_df <- modelo$model

  # reorganiza colunas
  cols_ordem <- c(
    fatores,
    grep("^I\\(", names(dados_df), value = TRUE),
    setdiff(names(dados_df), c(fatores, grep("^I\\(", names(dados_df), value = TRUE)))
  )
  dados_df <- dados_df[, cols_ordem, drop = FALSE]

  names(dados_df) <- gsub(
    "I\\(([^\\)]+)\\^2\\)",
    "\\1²",
    names(dados_df)
  )

  openxlsx::addWorksheet(wb, "Dados")
  openxlsx::writeData(wb, "Dados", dados_df)
  aplicar_estilo_tabela("Dados", dados_df)

  met_df <- metricas_ffd(modelo)
  openxlsx::addWorksheet(wb, "Métricas")
  openxlsx::writeData(wb, "Métricas", met_df)
  aplicar_estilo_tabela("Métricas", met_df)

  anova_df <- anova_ffd(modelo)
  names(anova_df)[names(anova_df) == "Df"] <- "GL"
  names(anova_df)[names(anova_df) == "Sum Sq"] <- "SQ"
  names(anova_df)[names(anova_df) == "Mean Sq"] <- "QM"
  names(anova_df)[names(anova_df) == "F value"] <- "F"
  names(anova_df)[names(anova_df) == "Pr(>F)"] <- "p_valor"
  anova_df$Termo <- formatar_termo(anova_df$Termo)

  openxlsx::addWorksheet(wb, "ANOVA")
  openxlsx::writeData(wb, "ANOVA", anova_df)
  aplicar_estilo_tabela("ANOVA", anova_df)

  coef_df <- coeficientes_ffd(modelo)
  coef_df$Termo <- formatar_termo(coef_df$Termo)
  openxlsx::addWorksheet(wb, "Coeficientes")
  openxlsx::writeData(wb, "Coeficientes", coef_df)
  aplicar_estilo_tabela("Coeficientes", coef_df)

  efeitos_df <- efeitos_ffd(modelo, alpha = alpha)
  openxlsx::addWorksheet(wb, "Efeitos")
  openxlsx::writeData(wb, "Efeitos", efeitos_df)
  aplicar_estilo_tabela("Efeitos", efeitos_df)

  if ("Significativo" %in% names(efeitos_df) && nrow(efeitos_df) > 0) {
    linhas_sig <- which(efeitos_df$Significativo == "Sim")
    if (length(linhas_sig) > 0) {
      openxlsx::addStyle(
        wb = wb,
        sheet = "Efeitos",
        style = estilo_significativo,
        rows = linhas_sig + 1,
        cols = 1:ncol(efeitos_df),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  ot <- tryCatch(
    otimo_ffd(modelo, objetivo = objetivo),
    error = function(e) NULL
  )

  if (!is.null(ot)) {

    tem_mensagem <- !is.null(ot$mensagem) &&
      length(ot$mensagem) > 0 &&
      !is.na(ot$mensagem) &&
      nzchar(ot$mensagem)

    otimo_df <- data.frame(
      Item = c(
        "Objetivo",
        names(ot$ponto),
        paste0("Resposta prevista (", ot$nome_resposta, ")"),
        "Convergência",
        "Valor otimizado",
        if (tem_mensagem) "Observação" else NULL
      ),
      Valor = c(
        ifelse(ot$objetivo == "min", "Minimizar", "Maximizar"),
        as.numeric(ot$ponto),
        as.numeric(ot$resposta),
        ifelse(ot$convergencia == 0, "Sucesso", "Falha"),
        as.numeric(ot$valor_otimizado),
        if (tem_mensagem) ot$mensagem else NULL
      ),
      check.names = FALSE
    )

    openxlsx::addWorksheet(wb, "Ótimo")
    openxlsx::writeData(wb, "Ótimo", otimo_df)
    aplicar_estilo_tabela("Ótimo", otimo_df)
  }
  pe <- tryCatch(
    ponto_estacionario_ffd(modelo),
    error = function(e) NULL
  )

  if (!is.null(pe)) {
    resumo_df <- data.frame(
      Item = c(
        "Classificação do ponto estacionário",
        "Observação",
        paste0("Resposta estimada (", pe$nome_resposta, ")"),
        "Status"
      ),
      Valor = c(
        pe$classificacao,
        pe$aviso,
        pe$resposta_estimada,
        ifelse(pe$convergencia == 0, "Sucesso", "Falha")
      ),
      check.names = FALSE
    )

    coord_df <- data.frame(
      Fator = names(pe$ponto),
      Valor = as.numeric(pe$ponto[1, ]),
      check.names = FALSE
    )

    autoval_df <- data.frame(
      Autovalor = paste0("λ", seq_along(pe$autovalores)),
      Valor = pe$autovalores,
      check.names = FALSE
    )

    # Formatação dos valores
    resumo_df$Valor <- formatar_num_seguro(resumo_df$Valor)
    coord_df$Valor <- formatar_num_seguro(coord_df$Valor)
    autoval_df$Valor <- formatar_num_seguro(autoval_df$Valor)

    openxlsx::addWorksheet(wb, "Ponto Estacionário")

    openxlsx::writeData(wb, "Ponto Estacionário", resumo_df, startRow = 2)
    aplicar_estilo_tabela("Ponto Estacionário", resumo_df)

    openxlsx::writeData(wb, "Ponto Estacionário", coord_df, startRow = 8)
    aplicar_estilo_tabela("Ponto Estacionário", coord_df)

    openxlsx::writeData(wb, "Ponto Estacionário", autoval_df, startRow = 13)
    aplicar_estilo_tabela("Ponto Estacionário", autoval_df)

    # 🔥 INTERPRETAÇÃO
    interpretacao <- if (pe$classificacao == "sela") {
      "Autovalores com sinais mistos: ponto de sela."
    } else if (pe$classificacao == "mínimo") {
      "Autovalores positivos: ponto de mínimo."
    } else if (pe$classificacao == "máximo") {
      "Autovalores negativos: ponto de máximo."
    } else {
      "Classificação não determinada."
    }

    openxlsx::writeData(
      wb,
      "Ponto Estacionário",
      data.frame(Interpretacao = interpretacao),
      startRow = 18
    )

    openxlsx::setColWidths(wb, "Ponto Estacionário", cols = 1:3, widths = "auto")
  }

  arquivos_tmp <- character(0)

  # ==============================================================================
  # GRÁFICOS (Agora com ambas orientações A x B e B x A)
  # ==============================================================================
  if (isTRUE(incluir_graficos)) {

    if (length(fatores) < 2) {
      stop("São necessários pelo menos dois fatores para exportar gráficos.")
    }

    tmp_pareto <- tempfile(fileext = ".png")
    arquivos_tmp <- c(arquivos_tmp, tmp_pareto)

    grDevices::png(tmp_pareto, width = 2200, height = 1400, res = 220)
    pareto_ffd(modelo, alpha = alpha)
    grDevices::dev.off()

    openxlsx::addWorksheet(wb, "Pareto")
    openxlsx::insertImage(
      wb, "Pareto", tmp_pareto,
      startRow = 2, startCol = 2,
      width = 11, height = 7, units = "in"
    )

    pares <- utils::combn(fatores, 2, simplify = FALSE)

    nome_aba_seguro <- function(prefixo, f1, f2) {
      nome <- paste(prefixo, f1, "x", f2)
      nome <- gsub("[\\\\/:*?\\[\\]]", "_", nome)
      if (nchar(nome) > 31) {
        nome <- substr(nome, 1, 31)
      }
      nome
    }

    for (par_fatores in pares) {

      f1 <- par_fatores[1]
      f2 <- par_fatores[2]

      # Cria a lista de orientações assim como no DCC
      orientacoes <- list(
        c(f1, f2),
        c(f2, f1)
      )

      for (ori in orientacoes) {
        x_plot <- ori[1]
        y_plot <- ori[2]

        # --- SUPERFÍCIE ---
        tmp_sup <- tempfile(fileext = ".png")
        arquivos_tmp <- c(arquivos_tmp, tmp_sup)

        grDevices::png(tmp_sup, width = 2200, height = 1400, res = 220)
        superficie_ffd(modelo, x1 = x_plot, x2 = y_plot)
        grDevices::dev.off()

        aba_sup <- nome_aba_seguro("Superf", x_plot, y_plot)
        openxlsx::addWorksheet(wb, aba_sup)
        openxlsx::insertImage(
          wb, aba_sup, tmp_sup,
          startRow = 2, startCol = 2,
          width = 11, height = 7, units = "in"
        )

        # --- CONTORNO ---
        tmp_cont <- tempfile(fileext = ".png")
        arquivos_tmp <- c(arquivos_tmp, tmp_cont)

        grDevices::png(tmp_cont, width = 2200, height = 1400, res = 220)
        contorno_ffd(
          modelo,
          x1 = x_plot,
          x2 = y_plot,
          mostrar_otimo = TRUE,
          mostrar_estacionario = TRUE,
          objetivo = objetivo
        )
        grDevices::dev.off()

        aba_cont <- nome_aba_seguro("Cont", x_plot, y_plot)
        openxlsx::addWorksheet(wb, aba_cont)
        openxlsx::insertImage(
          wb, aba_cont, tmp_cont,
          startRow = 2, startCol = 2,
          width = 11, height = 7, units = "in"
        )
      }
    }
  }

  openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)

  arquivos_tmp <- unique(arquivos_tmp)
  arquivos_tmp <- arquivos_tmp[file.exists(arquivos_tmp)]

  if (length(arquivos_tmp) > 0) {
    unlink(arquivos_tmp, force = TRUE)
  }

  caminho <- normalizePath(arquivo, winslash = "/", mustWork = FALSE)

  message("Arquivo Excel salvo em:\n", caminho)

  invisible(caminho)
}
