# módulo UI
mod_rayssa_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h3("Testando"),
    
    # Tabela de resumo
    tableOutput(ns("tabela_resumo")),
    
    # Controle de bins para histogramas
    sidebarLayout(
      sidebarPanel(
        sliderInput(ns("bins_janela_dias"), "Bins - Janela (dias)", min = 1, max = 20, value = 5),
        sliderInput(ns("bins_janela_meses"), "Bins - Janela (meses)", min = 1, max = 20, value = 5),
        sliderInput(ns("bins_inicial"), "Bins - Idade Inicial", min = 1, max = 20, value = 5),
        sliderInput(ns("bins_final"), "Bins - Idade Final", min = 1, max = 20, value = 5)
      ),
      mainPanel(
        fluidRow(
          column(6, plotOutput(ns("grafico_janela_dias"))),
          column(6, plotOutput(ns("grafico_janela_meses")))
        ),
        fluidRow(
          column(6, plotOutput(ns("hist_idade_inicial"))),
          column(6, plotOutput(ns("hist_idade_final")))
        )
      )
    )
  )
}

# módulo server
mod_rayssa_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    
    # Limpar e preparar dados
    dados_processados <- reactive({
      req(dados())
      d <- dados()
      d <- preparar_dados(d)
      d
    })
    
    # Tabela de estatísticas gerais
    output$tabela_resumo <- renderTable({
      req(dados_processados())
      
      df <- estatisticas_gerais(dados_processados())
      
      # definir os nomes bonitos
      colnames(df) <- c("Total de Procedimentos", "Crianças Distintas", "Cartões Distintos")
      
      df
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Histograma da janela em dias
    output$grafico_janela_dias <- renderPlot({
      req(dados_processados())
      ggplot(dados_processados(), aes(x = janela_dias)) +
        geom_histogram(bins = input$bins_janela_dias, fill = "steelblue", color = "black") +
        labs(x = "Janela de tempo (dias)", y = "Frequência", title = "Distribuição da janela de tempo (dias)") +
        theme_bw()
    })
    
    # Histograma da janela em meses
    output$grafico_janela_meses <- renderPlot({
      req(dados_processados())
      ggplot(dados_processados(), aes(x = janela_meses)) +
        geom_histogram(bins = input$bins_janela_meses, fill = "steelblue", color = "black") +
        labs(x = "Janela de tempo (meses)", y = "Frequência", title = "Distribuição da janela de tempo (meses)") +
        theme_bw()
    })
    
    # Histograma da idade inicial (bins ajustáveis)
    output$hist_idade_inicial <- renderPlot({
      req(dados_processados())
      idades_inicial <- dados_processados() %>%
        group_by(`Nome_Beneficiario`) %>%
        summarise(idade_inicial = min(idade_anos, na.rm = TRUE))
      
      ggplot(idades_inicial, aes(x = idade_inicial)) +
        geom_histogram(bins = input$bins_inicial, fill = "steelblue", color = "black") +
        labs(x = "Idade inicial (anos)", y = "Frequência") +
        theme_bw()
    })
    
    # Histograma da idade final (bins ajustáveis)
    output$hist_idade_final <- renderPlot({
      req(dados_processados())
      idades_final <- dados_processados() %>%
        group_by(`Nome_Beneficiario`) %>%
        summarise(idade_final = max(idade_anos, na.rm = TRUE))
      
      ggplot(idades_final, aes(x = idade_final)) +
        geom_histogram(bins = input$bins_final, fill = "steelblue", color = "black") +
        labs(x = "Idade final (anos)", y = "Frequência") +
        theme_bw()
    })
    
  })
}
