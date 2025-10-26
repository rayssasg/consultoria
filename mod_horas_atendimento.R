library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)

mod_horas_atendimento_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    card(
      card_header(
        class = "bg-primary",
        tags$h4("Horas de Atendimento", style = "margin: 0; color: white;")
      ),
      card_body(
        tabsetPanel(
          id = ns("abas"),
          type = "tabs",
          
          tabPanel(
            " Resumo Estatístico",
            fluidRow(
              column(6,
                     card(
                       card_header("Estatísticas de Horas por Registro"),
                       DT::DTOutput(ns("tab_horas_registro"))
                     )
              ),
              column(6,
                     card(
                       card_header("Distribuição de Horas por Registro"),
                       plotOutput(ns("plot_dist_horas"), height = "400px")
                     )
              )
            )
          ),
          
          tabPanel(
            " Análise por Paciente",
            card(
              card_header("Número de Atendimentos e Total de Horas por Paciente"),
              DT::DTOutput(ns("tab_por_paciente")),
              style = "height: 600px; overflow-y: auto;"
            )
          ),
          
          tabPanel(
            "Evolução Temporal",
            card(
              card_header("Evolução de Horas de Atendimento por Mês"),
              plotOutput(ns("plot_evolucao_horas"), height = "500px")
            )
          )
        )
      ),
      card_footer("Análise de horas de atendimento e distribuição por paciente")
    )
  )
}

mod_horas_atendimento_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    d0 <- reactive({ 
      req(dados())
      preparar_dados(dados()) 
    })
    
    output$tab_horas_registro <- DT::renderDT({
      df <- d0()
      horas <- df$horas_por_registro
      
      # Criar estatísticas manualmente para evitar o erro
      stats_df <- data.frame(
        Estatística = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Desvio Padrão"),
        Valor = c(
          min(horas, na.rm = TRUE),
          quantile(horas, 0.25, na.rm = TRUE),
          median(horas, na.rm = TRUE),
          mean(horas, na.rm = TRUE),
          quantile(horas, 0.75, na.rm = TRUE),
          max(horas, na.rm = TRUE),
          sd(horas, na.rm = TRUE)
        )
      )
      
      DT::datatable(
        stats_df,
        rownames = FALSE,
        options = list(dom = 't')
      ) %>% 
        DT::formatRound(columns = "Valor", digits = 2)
    })
    
    output$plot_dist_horas <- renderPlot({
      df <- d0()
      ggplot(df, aes(x = horas_por_registro)) +
        geom_histogram(bins = 30, fill = "#1E2A5E", alpha = 0.8, color = "white") +
        labs(
          title = "Distribuição de Horas por Registro",
          x = "Horas por Registro", 
          y = "Frequência"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    })
    
    output$tab_por_paciente <- DT::renderDT({
      df <- d0() %>%
        dplyr::group_by(Nome_Beneficiario) %>%
        dplyr::summarise(
          n_atendimentos = dplyr::n(),
          total_horas = sum(horas_por_registro, na.rm = TRUE),
          .groups = "drop"
        ) %>% 
        arrange(desc(total_horas))
      
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
          )
        )
      ) %>% 
        DT::formatRound(columns = "total_horas", digits = 2)
    })
    
    output$plot_evolucao_horas <- renderPlot({
      df <- d0() %>%
        mutate(
          AnoMes = format(Competencia, "%Y-%m"),
          DataRef = as.Date(paste0(AnoMes, "-01"))
        ) %>%
        group_by(DataRef) %>%
        summarise(
          TotalHoras = sum(horas_por_registro, na.rm = TRUE),
          .groups = "drop"
        )
      
      ggplot(df, aes(x = DataRef, y = TotalHoras)) +
        geom_line(color = "#18BC9C", linewidth = 1.5) +
        geom_point(color = "#1E2A5E", size = 2) +
        geom_area(fill = "#18BC9C", alpha = 0.2) +
        labs(
          title = "Evolução Mensal de Horas de Atendimento",
          x = "Mês/Ano", 
          y = "Total de Horas"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 13),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    })
  })
}