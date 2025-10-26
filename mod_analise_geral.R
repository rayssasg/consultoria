library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)

mod_analise_geral_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    card(
      card_header(
        class = "bg-primary",
        tags$h4("Análise Geral", style = "margin: 0; color: white;")
      ),
      card_body(
        tabsetPanel(
          id = ns("abas"),
          type = "tabs",
          
          tabPanel(
            " Resumo Estatístico",
            card(
              card_header("Estatísticas Gerais do Dataset"),
              DT::DTOutput(ns("tabela_resumo"))
            ),
            fluidRow(
              column(6,
                     card(
                       card_header("Distribuição da Janela de Tempo (Dias)"),
                       plotOutput(ns("plot_janela_dias"), height = "400px")
                     )
              ),
              column(6,
                     card(
                       card_header("Distribuição da Janela de Tempo (Meses)"),
                       plotOutput(ns("plot_janela_meses"), height = "400px")
                     )
              )
            )
          ),
          
          tabPanel(
            "Visualizações",
            fluidRow(
              column(6,
                     card(
                       card_header("Distribuição de Meses Participados"),
                       plotOutput(ns("plot_meses_participados"), height = "450px")
                     )
              ),
              column(6,
                     card(
                       card_header("Distribuição do Número de Registros"),
                       plotOutput(ns("plot_numero_registros"), height = "450px")
                     )
              )
            )
          ),
          
          tabPanel(
            "Dados Completos",
            card(
              card_header("Estatísticas Descritivas Detalhadas"),
              DT::DTOutput(ns("tab_estatisticas_detalhadas"))
            )
          )
        )
      ),
      card_footer("Visão geral e estatísticas descritivas do dataset completo")
    )
  )
}

mod_analise_geral_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    dados_prep <- reactive({ 
      req(dados())
      preparar_dados(dados()) 
    })
    
    output$tabela_resumo <- DT::renderDT({
      df <- estatisticas_gerais(dados_prep())
      resumo_df <- data.frame(
        Métrica = c("Total de Procedimentos", "Crianças Distintas", "Cartões Distintos"),
        Valor = c(df$total_procedimentos, df$criancas_distintas, df$cartoes_distintos)
      )
      
      DT::datatable(
        resumo_df,
        rownames = FALSE,
        options = list(dom = 't')
      )
    })
    
    output$plot_janela_dias <- renderPlot({
      df <- dados_prep()
      ggplot(df, aes(x = janela_dias)) +
        geom_histogram(bins = 30, fill = "#1E2A5E", alpha = 0.8, color = "white") +
        labs(
          title = "Distribuição da Janela de Tempo (Dias)",
          x = "Janela de Tempo (dias)", 
          y = "Frequência"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    })
    
    output$plot_janela_meses <- renderPlot({
      df <- dados_prep()
      ggplot(df, aes(x = janela_meses)) +
        geom_histogram(bins = 30, fill = "#18BC9C", alpha = 0.8, color = "white") +
        labs(
          title = "Distribuição da Janela de Tempo (Meses)",
          x = "Janela de Tempo (meses)", 
          y = "Frequência"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    })
    
    output$plot_meses_participados <- renderPlot({
      df <- dados_prep()
      ggplot(df, aes(x = meses_participados)) +
        geom_histogram(bins = 20, fill = "#5DADE2", alpha = 0.8, color = "white") +
        labs(
          title = "Distribuição de Meses Participados",
          x = "Meses Participados", 
          y = "Frequência"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    })
    
    output$plot_numero_registros <- renderPlot({
      df <- dados_prep()
      ggplot(df, aes(x = numero_registros)) +
        geom_histogram(bins = 20, fill = "#1E2A5E", alpha = 0.8, color = "white") +
        labs(
          title = "Distribuição do Número de Registros",
          x = "Número de Registros", 
          y = "Frequência"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    })
    
    output$tab_estatisticas_detalhadas <- DT::renderDT({
      df <- dados_prep()
      stats_df <- data.frame(
        Variável = c("Janela (dias)", "Janela (meses)", "Meses Participados", "Número de Registros"),
        Mínimo = c(
          min(df$janela_dias, na.rm = TRUE),
          min(df$janela_meses, na.rm = TRUE),
          min(df$meses_participados, na.rm = TRUE),
          min(df$numero_registros, na.rm = TRUE)
        ),
        Média = c(
          mean(df$janela_dias, na.rm = TRUE),
          mean(df$janela_meses, na.rm = TRUE),
          mean(df$meses_participados, na.rm = TRUE),
          mean(df$numero_registros, na.rm = TRUE)
        ),
        Mediana = c(
          median(df$janela_dias, na.rm = TRUE),
          median(df$janela_meses, na.rm = TRUE),
          median(df$meses_participados, na.rm = TRUE),
          median(df$numero_registros, na.rm = TRUE)
        ),
        Máximo = c(
          max(df$janela_dias, na.rm = TRUE),
          max(df$janela_meses, na.rm = TRUE),
          max(df$meses_participados, na.rm = TRUE),
          max(df$numero_registros, na.rm = TRUE)
        )
      )
      
      DT::datatable(
        stats_df,
        rownames = FALSE,
        options = list(dom = 'tip', pageLength = 10)
      ) %>% 
        DT::formatRound(columns = c("Mínimo", "Média", "Mediana", "Máximo"), digits = 2)
    })
  })
}