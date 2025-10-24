# modules/mod_caua.R
# Módulo Cauã – versão sem aba "Dados originais"

mod_caua_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Módulo Cauã"),
      checkboxInput(ns("somente_menores"), "Somente menores de 18", TRUE),
      sliderInput(ns("bins"), "Bins (histograma)", min = 5, max = 50, value = 20),
      selectInput(ns("col_hist"), "Variável numérica p/ histograma", choices = NULL),
      selectInput(ns("col_jud"),  "Coluna p/ Ações Judiciais", choices = NULL),
      selectInput(ns("col_grp_ts"), "Agrupar série temporal por", choices = NULL)
    ),
    mainPanel(
      tabsetPanel(
        # aba "Dados originais" foi removida
        tabPanel("Tabelas",
                 tableOutput(ns("tabela_horas")),
                 tableOutput(ns("tabela_esp"))
        ),
        tabPanel("Gráficos",
                 fluidRow(
                   column(6, plotOutput(ns("plot_hist"))),
                   column(6, plotOutput(ns("plot_ts")))
                 ),
                 fluidRow(
                   column(6, plotOutput(ns("plot_horas_reg"))),
                   column(6, plotOutput(ns("plot_situacao")))
                 )
        )
      )
    )
  )
}

mod_caua_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    
    # Preparar dados
    dados_proc <- reactive({
      req(dados())
      d <- dados()
      d <- preparar_dados(d)
      if (isTRUE(input$somente_menores) && "idade_anos" %in% names(d)) {
        d <- d[d$idade_anos < 18, , drop = FALSE]
      }
      if (!"ano_registro" %in% names(d)) {
        d$ano_registro <- lubridate::year(d$Competencia)
      }
      if (!"horas_por_registro" %in% names(d) && all(c("Duracao_sessao","Qtde_Itens") %in% names(d))) {
        d$duracao_min <- dplyr::case_when(
          d$Duracao_sessao == "30 a 40 minutos" ~ 35,
          d$Duracao_sessao == "50 minutos"      ~ 50,
          TRUE ~ NA_real_
        )
        d$horas_por_registro <- (d$duracao_min * d$Qtde_Itens) / 60
      }
      d
    })
    
    # Atualizar selects
    observeEvent(dados_proc(), {
      d <- dados_proc()
      cols <- names(d)
      num_cols <- cols[vapply(d, is.numeric, logical(1))]
      num_pref <- intersect(c("janela_dias", "janela_meses", "idade_anos", "horas_por_registro"), num_cols)
      hist_sel <- if (length(num_pref)) num_pref[[1]] else if (length(num_cols)) num_cols[[1]] else NULL
      cat_cols <- cols[vapply(d, function(x) is.character(x) || is.factor(x), logical(1))]
      jud_pref <- if ("Espec_Solic" %in% cat_cols) "Espec_Solic" else if (length(cat_cols)) cat_cols[[1]] else NULL
      grp_pref <- if ("Situacao" %in% cat_cols) "Situacao" else if (length(cat_cols)) cat_cols[[1]] else NULL
      updateSelectInput(session, "col_hist",   choices = num_cols, selected = hist_sel)
      updateSelectInput(session, "col_jud",    choices = cat_cols, selected = jud_pref)
      updateSelectInput(session, "col_grp_ts", choices = cat_cols, selected = grp_pref)
    }, ignoreInit = FALSE)
    
    # Tabelas
    output$tabela_horas <- renderTable({
      d <- dados_proc()
      req(all(c("Nome_Beneficiario","horas_por_registro") %in% names(d)))
      d |>
        dplyr::group_by(Nome_Beneficiario) |>
        dplyr::summarise(
          n_procedimentos = dplyr::n(),
          total_horas     = sum(horas_por_registro, na.rm = TRUE),
          .groups = "drop"
        ) |>
        head(20)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$tabela_esp <- renderTable({
      d <- dados_proc()
      req("Acao_Judicial" %in% names(d), input$col_jud)
      d |>
        dplyr::filter(Acao_Judicial == "SIM") |>
        dplyr::group_by(Nome_Beneficiario, .data[[input$col_jud]]) |>
        dplyr::summarise(`Total por especialista` = dplyr::n(), .groups = "drop") |>
        head(20)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Gráficos
    output$plot_hist <- renderPlot({
      d <- dados_proc()
      req(input$col_hist, is.numeric(d[[input$col_hist]]))
      ggplot2::ggplot(d, ggplot2::aes(x = .data[[input$col_hist]])) +
        ggplot2::geom_histogram(bins = input$bins, fill = "steelblue", color = "black") +
        ggplot2::labs(x = input$col_hist, y = "Frequência", title = "Distribuição (histograma)") +
        ggplot2::theme_bw()
    })
    
    output$plot_ts <- renderPlot({
      d <- dados_proc()
      req("ano_registro" %in% names(d), input$col_grp_ts)
      d_sum <- d |>
        dplyr::group_by(ano_registro, .data[[input$col_grp_ts]]) |>
        dplyr::summarise(total_sessoes = sum(horas_por_registro, na.rm = TRUE), .groups = "drop")
      ggplot2::ggplot(d_sum, ggplot2::aes(x = ano_registro, y = total_sessoes, color = .data[[input$col_grp_ts]])) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(title = "Evolução do total de horas por ano",
                      x = "Ano", y = "Total de horas", color = input$col_grp_ts) +
        ggplot2::theme_bw()
    })
    
    output$plot_horas_reg <- renderPlot({
      d <- dados_proc()
      req("horas_por_registro" %in% names(d))
      ggplot2::ggplot(d, ggplot2::aes(x = horas_por_registro)) +
        ggplot2::geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
        ggplot2::labs(title = "Distribuição de horas por registro",
                      x = "Horas por registro", y = "Frequência") +
        ggplot2::theme_bw()
    })
    
    output$plot_situacao <- renderPlot({
      d <- dados_proc()
      req(all(c("Nome_Beneficiario","Situacao","idade_anos","Competencia") %in% names(d)))
      d_recente <- d |>
        dplyr::group_by(Nome_Beneficiario) |>
        dplyr::arrange(dplyr::desc(idade_anos), dplyr::desc(Competencia)) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()
      ggplot2::ggplot(d_recente, ggplot2::aes(x = Situacao, fill = Situacao)) +
        ggplot2::geom_bar(color = "black", show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = c("steelblue", "#235F77", "#939598")) +
        ggplot2::labs(x = "Situação do plano", y = "Frequência",
                      title = "Situação mais recente do plano por beneficiário") +
        ggplot2::theme_bw()
    })
  })
}
