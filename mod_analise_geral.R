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
            "Resumo Estatístico",
            fluidRow(
              card(
                card_header("Estatísticas Gerais do Dataset"),
                DT::DTOutput(ns("tabela_resumo"))
              )
            ),
            fluidRow(
              column(6,
                     card(
                       card_header("Estatísticas Descritivas Detalhadas"),
                       DT::DTOutput(ns("tab_estatisticas_detalhadas"))
                     )),
              )
            )
          )),
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