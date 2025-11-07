library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)

mod_idade_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header(
        class = "bg-primary",
        tags$h4("Análise por Idade", style = "margin: 0; color: white;")
      ),
      card_body(
        # Usar tabsetPanel tradicional que sempre funciona
        tabsetPanel(
          id = ns("abas"),
          type = "tabs",
          
          tabPanel(
            "Visão Geral",
            fluidRow(
              column(6,
                     card(
                       card_header("Distribuição de Idades"),
                       plotOutput(ns("plot_idade"), height = "500px")
                     )
              ),
              column(6,
                     card(
                       card_header("Estatísticas Descritivas"),
                       DT::DTOutput(ns("tab_estatisticas")))
              )
          )),
          tabPanel(
            "Análise Detalhada", 
            fluidRow(
              column(6,
                     card(
                       card_header("Boxplot de Idades por Ano"),
                       plotOutput(ns("plot_boxplot_idade"), height = "550px")
                     )
              ),
              column(6,
                     card(
                       card_header("Evolução da Idade Média por Ano"),
                       plotOutput(ns("plot_media_ano"), height = "500px")
                     )
              )
            )
          ),
          
          tabPanel(
            "Dados Completos",
            card(
              card_header("Tabela de Frequência de Idades"),
              DT::DTOutput(ns("tab_frequencia_idade"))
            )
          )
        )
      ),
      card_footer("Análise da distribuição etária dos beneficiários")
    )
  )
}

mod_idade_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    
    dados_prep <- reactive({
      req(dados())
      preparar_dados(dados())
    })
    
    # Gráfico de distribuição de idades
    output$plot_idade <- renderPlot({
      df <- dados_prep()
      ggplot(df, aes(x = idade_anos)) +
        geom_bar(bins = 20, fill = "#1E2A5E", alpha = 0.8, color = "white") +
        labs(
          x = "Idade (anos)", 
          y = "Frequência",
          title = "Distribuição das Idades dos Beneficiários"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    })
    
    # Evolução da idade média por ano
    output$plot_media_ano <- renderPlot({
      d <- dados_prep() %>%
        mutate(Ano = lubridate::year(Competencia)) %>%
        group_by(Ano) %>%
        summarise(IdadeMedia = mean(idade_anos, na.rm = TRUE))
      
      ggplot(d, aes(x = Ano, y = IdadeMedia)) +
        geom_line(color = "#1E2A5E", linewidth = 1.5) +
        geom_point(size = 3, color = "#1E2A5E") +
        labs(
          x = "Ano", 
          y = "Idade Média (anos)",
          title = "Evolução da Idade Média por Ano"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    })
    
    # Boxplot de idades por ano
    output$plot_boxplot_idade <- renderPlot({
      df <- dados_prep() %>%
        mutate(Ano = as.factor(lubridate::year(Competencia)))
      
      ggplot(df, aes(x = Ano, y = idade_anos)) +
        geom_boxplot(fill = "#5DADE2", alpha = 0.7) +
        labs(
          x = "Ano",
          y = "Idade (anos)",
          title = "Distribuição de Idades por Ano"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    })
    
    # Estatísticas descritivas
    output$tab_estatisticas <- DT::renderDT({
      df <- dados_prep()
      stats_df <- data.frame(
        Estatística = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Desvio Padrão"),
        Valor = c(
          min(df$idade_anos, na.rm = TRUE),
          quantile(df$idade_anos, 0.25, na.rm = TRUE),
          median(df$idade_anos, na.rm = TRUE),
          mean(df$idade_anos, na.rm = TRUE),
          quantile(df$idade_anos, 0.75, na.rm = TRUE),
          max(df$idade_anos, na.rm = TRUE),
          sd(df$idade_anos, na.rm = TRUE)
        )
      )
      
      DT::datatable(
        stats_df,
        rownames = FALSE,
        options = list(dom = 't', pageLength = 10)
      )
    })
    
    # Tabela de frequência completa
    output$tab_frequencia_idade <- DT::renderDT({
      df <- dados_prep() %>%
        count(idade_anos, name = "Frequência") %>%
        mutate(Percentual = round(Frequência / sum(Frequência) * 100, 2)) %>%
        arrange(idade_anos)
      
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 15, 
          scrollX = TRUE,
          scrollY = "500px"
        )
      )
    })
  })
}