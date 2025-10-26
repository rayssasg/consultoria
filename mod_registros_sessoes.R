library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)

mod_registros_sessoes_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    card(
      card_header(
        class = "bg-primary",
        tags$h4(" Registros e Sessões", style = "margin: 0; color: white;")
      ),
      card_body(
        tabsetPanel(
          id = ns("abas"),
          type = "tabs",
          
          tabPanel(
            "Visão Temporal",
            fluidRow(
              column(6,
                     card(
                       card_header("Registros por Ano"),
                       plotOutput(ns("plot_registros_ano"), height = "500px")
                     )
              ),
              column(6,
                     card(
                       card_header("Sessões por Ano"),
                       plotOutput(ns("plot_sessoes_ano"), height = "500px")
                     )
              )
            ),
            fluidRow(
              column(12,
                     card(
                       card_header("Evolução Mensal"),
                       plotOutput(ns("plot_evolucao_mensal"), height = "500px")
                     )
              )
            )
          ),
          
          tabPanel(
            "Distribuições",
            fluidRow(
              column(6,
                     card(
                       card_header("Distribuição de Sessões por Registro"),
                       plotOutput(ns("plot_dist_sessoes_registro"), height = "550px")
                     )
              ),
              column(6,
                     card(
                       card_header("Distribuição de Duração das Sessões"),
                       plotOutput(ns("plot_dist_duracao"), height = "550px")
                     )
              )
            )
          ),
          
          tabPanel(
            "Dados Detalhados",
            fluidRow(
              column(6,
                     card(
                       card_header("Registros por Ano"),
                       DT::DTOutput(ns("tab_registros_ano"))
                     )
              ),
              column(6,
                     card(
                       card_header("Estatísticas de Sessões"),
                       DT::DTOutput(ns("tab_estatisticas_sessoes"))
                     )
              )
            )
          )
        )
      ),
      card_footer("Análise de volume e distribuição de registros e sessões")
    )
  )
}

mod_registros_sessoes_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    
    dados_prep <- reactive({
      req(dados())
      d <- preparar_dados(dados())
      d <- criar_ano_registro(d, Competencia)
      d
    })
    
    # Gráfico de registros por ano
    output$plot_registros_ano <- renderPlot({
      d <- dados_prep()
      d_ano <- dplyr::count(d, ano_registro, name = "n") %>%
        mutate(ano_registro = as.numeric(ano_registro))
      
      ggplot(d_ano, aes(x = ano_registro, y = n)) +
        geom_col(fill = "#1E2A5E", alpha = 0.8) +
        geom_text(aes(label = n), vjust = -0.5, size = 5, fontface = "bold") +
        labs(
          title = "Quantidade de Registros por Ano",
          x = "Ano", 
          y = "Número de Registros"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        ) +
        scale_x_continuous(breaks = unique(d_ano$ano_registro))
    })
    
    # Gráfico de sessões por ano
    output$plot_sessoes_ano <- renderPlot({
      d <- dados_prep()
      dados_sum <- d %>%
        group_by(ano_registro) %>%
        summarise(total_sessoes = sum(horas_por_registro, na.rm = TRUE)) %>%
        mutate(ano_registro = as.numeric(ano_registro))
      
      ggplot(dados_sum, aes(x = ano_registro, y = total_sessoes)) +
        geom_line(color = "#18BC9C", linewidth = 1.5) +
        geom_point(color = "#1E2A5E", size = 3) +
        geom_area(fill = "#18BC9C", alpha = 0.2) +
        geom_text(aes(label = round(total_sessoes, 1)), vjust = -1, size = 4, fontface = "bold") +
        labs(
          title = "Evolução do Número de Sessões por Ano",
          x = "Ano do Registro", 
          y = "Total de Sessões"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    })
    
    # Evolução mensal
    output$plot_evolucao_mensal <- renderPlot({
      d <- dados_prep() %>%
        mutate(
          AnoMes = format(Competencia, "%Y-%m"),
          DataRef = as.Date(paste0(AnoMes, "-01"))
        ) %>%
        group_by(DataRef) %>%
        summarise(
          Registros = n(),
          Sessoes = sum(horas_por_registro, na.rm = TRUE),
          .groups = "drop"
        )
      
      ggplot(d, aes(x = DataRef)) +
        geom_line(aes(y = Registros, color = "Registros"), linewidth = 1) +
        geom_line(aes(y = Sessoes * max(Registros)/max(Sessoes), color = "Sessões"), linewidth = 1) +
        scale_y_continuous(
          name = "Registros",
          sec.axis = sec_axis(~ . * max(d$Sessoes)/max(d$Registros), name = "Sessões")
        ) +
        scale_color_manual(
          name = "Métrica",
          values = c("Registros" = "#1E2A5E", "Sessões" = "#18BC9C")
        ) +
        labs(
          title = "Evolução Mensal de Registros e Sessões",
          x = "Mês/Ano"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 13)
        )
    })
    
    # Distribuição de sessões por registro
    output$plot_dist_sessoes_registro <- renderPlot({
      d <- dados_prep()
      ggplot(d, aes(x = horas_por_registro)) +
        geom_histogram(binwidth = 0.5, fill = "#5DADE2", alpha = 0.8, color = "white") +
        ##geom_density(aes(y = after_stat(count) * 0.5), alpha = 0.3, fill = "#1E2A5E", color = NA) +
        labs(
          title = "Distribuição da Quantidade de Sessões por Registro",
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
    
    # Distribuição de duração
    output$plot_dist_duracao <- renderPlot({
      d <- dados_prep()
      ggplot(d, aes(x = Duracao_sessao, fill = Duracao_sessao)) +
        geom_bar(alpha = 0.8) +
        scale_fill_manual(values = c("#1E2A5E", "#18BC9C", "#5DADE2")) +
        labs(
          title = "Distribuição da Duração das Sessões",
          x = "Duração da Sessão", 
          y = "Frequência"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)
        )
    })
    
    # Tabela de registros por ano
    output$tab_registros_ano <- DT::renderDT({
      df <- tabela_registros_por_ano_df(dados_prep(), ano_registro) %>%
        rename(Ano = ano_registro, `Total de Registros` = total_registros)
      
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          dom = 'tip',
          pageLength = 10,
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
          )
        )
      )
    })
    
    # Estatísticas de sessões
    output$tab_estatisticas_sessoes <- DT::renderDT({
      d <- dados_prep()
      stats_df <- data.frame(
        Estatística = c(
          "Total de Sessões", 
          "Média por Registro", 
          "Mediana por Registro",
          "Máximo por Registro",
          "Desvio Padrão"
        ),
        Valor = c(
          sum(d$horas_por_registro, na.rm = TRUE),
          mean(d$horas_por_registro, na.rm = TRUE),
          median(d$horas_por_registro, na.rm = TRUE),
          max(d$horas_por_registro, na.rm = TRUE),
          sd(d$horas_por_registro, na.rm = TRUE)
        )
      )
      
      DT::datatable(
        stats_df,
        rownames = FALSE,
        options = list(
          dom = 't',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
          )
        )
      ) %>% 
        DT::formatRound(columns = "Valor", digits = 2)
    })
  })
}