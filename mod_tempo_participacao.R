library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)

mod_tempo_participacao_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    card(
      card_header(
        class = "bg-primary",
        tags$h4("Tempo de Participação", style = "margin: 0; color: white;")
      ),
      card_body(
        tabsetPanel(
          id = ns("abas"),
          type = "tabs",
          
          tabPanel(
            " Distribuições",
            fluidRow(
              column(6,
                     card(
                       card_header("Controles"),
                       card_body(
                         sliderInput(ns("bins_dias"), "Número de Barras (Dias)", 
                                     min = 5, max = 50, value = 20),
                         sliderInput(ns("bins_meses"), "Número de Barras (Meses)", 
                                     min = 5, max = 50, value = 20)
                       )
                     )
              ),
              column(6,
                     card(
                       card_header("Instruções"),
                       card_body(
                         p("Use os sliders para ajustar o número de barras nos histogramas."),
                         p("Analise a distribuição do tempo de participação dos beneficiários."),
                         p("A janela de tempo refere-se à diferença entre o primeiro e o último atendimento de cada paciente
registrado no banco de dados."),
                         p("O número de meses participados refere-se a quantos meses
o paciente recebeu algum atendimento, calculado somando as ocorrências de atendimentos em
meses distintos")
                       )
                     )
              )
            ),
            fluidRow(
              column(6,
                     card(
                       card_header("Distribuição - Janela (Dias)"),
                       plotOutput(ns("hist_dias"), height = "400px")
                     )
              ),
              column(6,
                     card(
                       card_header("Distribuição - Janela (Meses)"),
                       plotOutput(ns("hist_meses"), height = "400px")
                     )
              )
            )
          ),
          
          tabPanel(
            " Análise Detalhada",
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
            "Estatísticas",
            fluidRow(
              column(6,
                     card(
                       card_header("Estatísticas - Meses Participados"),
                       DT::DTOutput(ns("tab_meses_participados"))
                     )
              ),
              column(6,
                     card(
                       card_header("Estatísticas - Número de Registros"),
                       DT::DTOutput(ns("tab_num_registros"))
                     )
              )
            ),
            fluidRow(
              column(12,
                     card(
                       card_header("Resumo Completo do Tempo de Participação"),
                       DT::DTOutput(ns("tab_resumo_completo"))
                     )
              )
            )
          )
        )
      ),
      card_footer("Análise do tempo de participação e engajamento dos beneficiários")
    )
  )
}

mod_tempo_participacao_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    d0 <- reactive({ 
      req(dados())
      preparar_dados(dados()) 
    })
    
    output$hist_dias <- renderPlot({
      ggplot(d0(), aes(x = janela_dias)) +
        geom_histogram(bins = input$bins_dias, fill = "#1E2A5E", alpha = 0.8, color = "white") +
        labs(
          title = "Tempo entre 1º e último atendimento (dias)",
          x = "Dias", 
          y = "Frequência"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    })
    
    output$hist_meses <- renderPlot({
      ggplot(d0(), aes(x = janela_meses)) +
        geom_histogram(bins = input$bins_meses, fill = "#18BC9C", alpha = 0.8, color = "white") +
        labs(
          title = "Janela de Participação (meses)",
          x = "Meses", 
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
      df <- d0()
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
      df <- d0()
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
    
    output$tab_meses_participados <- DT::renderDT({
      df <- d0()
      meses <- df$meses_participados
      
      stats_df <- data.frame(
        Estatística = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Desvio Padrão"),
        Valor = c(
          min(meses, na.rm = TRUE),
          quantile(meses, 0.25, na.rm = TRUE),
          median(meses, na.rm = TRUE),
          mean(meses, na.rm = TRUE),
          quantile(meses, 0.75, na.rm = TRUE),
          max(meses, na.rm = TRUE),
          sd(meses, na.rm = TRUE)
        )
      )
      
      DT::datatable(
        stats_df,
        rownames = FALSE,
        options = list(dom = 't')
      ) %>% 
        DT::formatRound(columns = "Valor", digits = 2)
    })
    
    output$tab_num_registros <- DT::renderDT({
      df <- d0()
      registros <- df$numero_registros
      
      stats_df <- data.frame(
        Estatística = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Desvio Padrão"),
        Valor = c(
          min(registros, na.rm = TRUE),
          quantile(registros, 0.25, na.rm = TRUE),
          median(registros, na.rm = TRUE),
          mean(registros, na.rm = TRUE),
          quantile(registros, 0.75, na.rm = TRUE),
          max(registros, na.rm = TRUE),
          sd(registros, na.rm = TRUE)
        )
      )
      
      DT::datatable(
        stats_df,
        rownames = FALSE,
        options = list(dom = 't')
      ) %>% 
        DT::formatRound(columns = "Valor", digits = 2)
    })
    
    output$tab_resumo_completo <- DT::renderDT({
      df <- d0()
      stats_df <- data.frame(
        Métrica = c("Janela (dias)", "Janela (meses)", "Meses Participados", "Número de Registros"),
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