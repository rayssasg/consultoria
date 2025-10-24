# modules/mod_idade.R

mod_idade_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Idade"),
      sliderInput(ns("bins_ini"),  "Bins idade inicial", min = 1, max = 30, value = 10),
      sliderInput(ns("bins_fim"),  "Bins idade final",   min = 1, max = 30, value = 10),
      sliderInput(ns("bins_diff"), "Bins diferenças",     min = 1, max = 30, value = 10)
    ),
    mainPanel(
      h4("Estatísticas sumárias (primeiro e último atendimento)"),
      tableOutput(ns("tab_sumarias_idade")),
      hr(),
      fluidRow(
        column(6, plotOutput(ns("hist_ini"))),
        column(6, plotOutput(ns("hist_fim")))
      ),
      hr(),
      fluidRow(
        column(6, tableOutput(ns("tab_comp_ini_fim"))),
        column(6, tableOutput(ns("tab_registros_por_idade")))
      ),
      hr(),
      h4("Horas x Idade"),
      fluidRow(
        column(6, plotOutput(ns("linha_media_ponderada"))),
        column(6, plotOutput(ns("hist_diferencas_horas")))
      ),
      hr(),
      h4("Correlação (Spearman)"),
      verbatimTextOutput(ns("texto_spearman"))
    )
  )
}

mod_idade_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    d0 <- reactive({
      req(dados())
      preparar_dados(dados())
    })
    
    # Base por criança: idades inicial/final (para os hists e sumários)
    base_ini_fim <- reactive({
      d0() %>%
        dplyr::group_by(Nome_Beneficiario) %>%
        dplyr::summarise(
          idade_inicial = min(idade_anos, na.rm = TRUE),
          idade_final   = max(idade_anos, na.rm = TRUE),
          .groups = "drop"
        )
    })
    
    output$tab_sumarias_idade <- renderTable({
      # Tabela simples com summary
      s <- summary(base_ini_fim()[, c("idade_inicial","idade_final")])
      data.frame(estatistica = rownames(s), s, row.names = NULL, check.names = FALSE)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$hist_ini <- renderPlot({
      ggplot2::ggplot(base_ini_fim(), ggplot2::aes(x = idade_inicial)) +
        ggplot2::geom_histogram(bins = input$bins_ini, fill = "steelblue", color = "black") +
        ggplot2::theme_bw() + ggplot2::labs(x="Idade inicial", y="Frequência")
    })
    
    output$hist_fim <- renderPlot({
      ggplot2::ggplot(base_ini_fim(), ggplot2::aes(x = idade_final)) +
        ggplot2::geom_histogram(bins = input$bins_fim, fill = "steelblue", color = "black") +
        ggplot2::theme_bw() + ggplot2::labs(x="Idade final", y="Frequência")
    })
    
    output$tab_comp_ini_fim <- renderTable({
      base_ini_fim() %>%
        dplyr::mutate(diff = idade_final - idade_inicial) %>%
        head(50)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$tab_registros_por_idade <- renderTable({
      d0() %>%
        dplyr::count(idade_anos, name = "n_registros") %>%
        dplyr::arrange(idade_anos)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # ====== Nível "por criança e idade" (para diferenças e Spearman) ======
    # Equivalente à "etapa 2" da sua horas_normalizadas(): media_horas_mensal_paciente por criança-idade
    d_crianca_idade <- reactive({
      d <- d0()
      dados1 <- d %>%
        dplyr::group_by(Nome_Beneficiario, idade_anos) %>%
        dplyr::mutate(meses_participados_idade = dplyr::n_distinct(Competencia)) %>%
        dplyr::ungroup()
      
      media_p11 <- dados1 %>%
        dplyr::group_by(Nome_Beneficiario, Competencia, idade_anos) %>%
        dplyr::summarise(
          total_horas_mes = sum(horas_por_registro, na.rm = TRUE),
          peso = max(meses_participados_idade),
          .groups = "drop"
        )
      
      media_p22 <- media_p11 %>%
        dplyr::group_by(Nome_Beneficiario, idade_anos) %>%
        dplyr::summarise(
          media_horas_mensal_paciente = mean(total_horas_mes, na.rm = TRUE),
          peso = max(peso),
          .groups = "drop"
        )
      media_p22
    })
    
    # Linha: média (ponderada) por idade (usa suas funções existentes)
    output$linha_media_ponderada <- renderPlot({
      graficos <- graficos_evolucao_horas(horas_normalizadas(d0()))
      graficos$grafico_media_ponderada
    })
    
    # Histograma das diferenças de horas (primeira vs. recente), por criança
    output$hist_diferencas_horas <- renderPlot({
      d <- d_crianca_idade()
      req(all(c("Nome_Beneficiario","idade_anos","media_horas_mensal_paciente") %in% names(d)))
      diffs <- d %>%
        dplyr::group_by(Nome_Beneficiario) %>%
        dplyr::summarise(
          idade_primeira = min(idade_anos, na.rm = TRUE),
          horas_primeira = media_horas_mensal_paciente[which.min(idade_anos)],
          idade_recente  = max(idade_anos, na.rm = TRUE),
          horas_recente  = media_horas_mensal_paciente[which.max(idade_anos)],
          .groups = "drop"
        ) %>%
        dplyr::mutate(dif_horas = horas_recente - horas_primeira) %>%
        dplyr::filter(!is.na(dif_horas), dif_horas != 0)
      
      ggplot2::ggplot(diffs, ggplot2::aes(x = dif_horas)) +
        ggplot2::geom_histogram(bins = input$bins_diff, fill = "steelblue", color = "black") +
        ggplot2::labs(x = "Diferença de horas (Positivo = aumento com a idade)", y = "Nº de Crianças") +
        ggplot2::theme_bw()
    })
    
    # Correlação Spearman (idade_anos x média de horas por criança-idade)
    output$texto_spearman <- renderPrint({
      d <- d_crianca_idade()
      req(all(c("idade_anos","media_horas_mensal_paciente") %in% names(d)))
      with(d, stats::cor.test(idade_anos, media_horas_mensal_paciente, method = "spearman"))
    })
  })
}
