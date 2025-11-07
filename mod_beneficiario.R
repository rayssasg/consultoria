library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(bslib)

mod_beneficiario_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    card(
      card_header(
        class = "bg-primary",
        tags$h4("Análise por Beneficiário", style = "margin: 0; color: white;")
      ),
      card_body(
        tabsetPanel(
          id = ns("abas"),
          type = "tabs",
          
          tabPanel(
            "Visão Geral",
            fluidRow(
              column(6,
                     card(
                       card_header("Situação do Plano"),
                       plotlyOutput(ns("plot_situacao"), height = "400px")
                     )
              ),
              column(6,
                     card(
                       card_header("Distribuição por Ação Judicial"),
                       plotlyOutput(ns("plot_acoes_judiciais"), height = "400px")
                     )
              )
            )
          ),
          
          tabPanel(
            "Especialidades & Procedimentos",
            fluidRow(
              column(6,
                     card(
                       card_header("Top 10 Especialidades"),
                       plotlyOutput(ns("plot_top_especialidades"), height = "500px")
                     )
              ),
              column(6,
                     card(
                       card_header("Top 10 Procedimentos"),
                       plotlyOutput(ns("plot_top_procedimentos"), height = "500px")
                     )
              )
            )
          ),
          
          tabPanel(
            "Mais Detalhes",
            fluidRow(
              column(4,
                     card(
                       card_header("Contagem por Ação Judicial"),
                       DT::DTOutput(ns("tab_acoes_contagem"))
                     )
              ),
              column(4,
                     card(
                       card_header("Especialidades Mais Comuns"),
                       DT::DTOutput(ns("tab_acoes_especialistas"))
                     )
              ),
              column(4,
                     card(
                       card_header("Procedimentos Mais Comuns"),
                       DT::DTOutput(ns("tab_acoes_procedimentos"))
                     )
              )
            )
          ),
          
          tabPanel(
            "Dados Completos",
            fluidRow(
              column(6,
                     card(
                       card_header("Especialidades por Registro"),
                       DT::DTOutput(ns("tab_especialistas_registro"))
                     )
              ),
              column(6,
                     card(
                       card_header("Procedimentos por Registro"),
                       DT::DTOutput(ns("tab_procedimentos_geral"))
                     )
              )
            )
          )
        )
      ),
      card_footer("Análise detalhada por beneficiário, especialidades e procedimentos")
    )
  )
}

mod_beneficiario_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    dados_prep <- reactive({
      req(dados())
      preparar_dados(dados())
    })
    
    # Gráfico de situação do plano - PLOTLY
    output$plot_situacao <- renderPlotly({
      dados_unicos <- dados_prep() %>%
        distinct(Nome_Beneficiario, .keep_all = TRUE)
      
      contagem <- dados_unicos %>%
        count(Situacao, name = "Frequencia")
      
      p <- ggplot(contagem, aes(x = Situacao, y = Frequencia, fill = Situacao,
                                text = paste("Situação:", Situacao, "<br>Pacientes:", Frequencia))) +
        geom_col(alpha = 0.8) +
        scale_fill_manual(values = c("#1E2A5E", "#18BC9C", "#5DADE2")) +
        labs(
          title = "Situação do Plano dos Beneficiários",
          x = "Situação do Plano",
          y = "Número de Beneficiários"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
          legend.position = "none",
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12)
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          autosize = TRUE,
          margin = list(l = 50, r = 50, b = 50, t = 50)
        )
    })
    
    # Gráfico de ações judiciais - PLOTLY
    output$plot_acoes_judiciais <- renderPlotly({
      contagem <- dados_prep() %>%
        distinct(Nome_Beneficiario, Acao_Judicial) %>%
        count(Acao_Judicial, name = "Frequencia")
      
      p <- ggplot(contagem, aes(x = Acao_Judicial, y = Frequencia, fill = Acao_Judicial,
                                text = paste("Ação Judicial:", Acao_Judicial, "<br>Pacientes:", Frequencia))) +
        geom_col(alpha = 0.8) +
        scale_fill_manual(values = c("#1E2A5E", "#18BC9C")) +
        labs(
          title = "Distribuição por Ação Judicial",
          x = "Ação Judicial",
          y = "Número de Beneficiários"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
          legend.position = "none",
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12)
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          autosize = TRUE,
          margin = list(l = 50, r = 50, b = 50, t = 50)
        )
    })
    
    # Top especialidades - PLOTLY
    output$plot_top_especialidades <- renderPlotly({
      top_espec <- dados_prep() %>%
        count(Espec_Solic, sort = TRUE) %>%
        head(10)
      
      p <- ggplot(top_espec, aes(x = reorder(Espec_Solic, n), y = n,
                                 text = paste("Especialidade:", Espec_Solic, "<br>Registros:", n),
                                 fill = n)) +
        geom_col(alpha = 0.8) +
        scale_fill_gradient(low = "#1E2A5E", high = "#3498DB") +
        coord_flip() +
        labs(
          title = "Top 10 Especialidades",
          x = "Especialidade",
          y = "Número de Registros"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          legend.position = "none"
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          autosize = TRUE,
          margin = list(l = 150, r = 50, b = 50, t = 50),  # Margem esquerda maior para labels
          yaxis = list(tickangle = 0)
        )
    })
    
    # Top procedimentos - PLOTLY
    output$plot_top_procedimentos <- renderPlotly({
      top_proc <- contar_procedimentos(dados_prep()) %>%
        head(10)
      
      p <- ggplot(top_proc, aes(x = reorder(Descricao_Item, n), y = n,
                                text = paste("Procedimento:", Descricao_Item, "<br>Registros:", n),
                                fill = n)) +
        geom_col(alpha = 0.8) +
        scale_fill_gradient(low = "#18BC9C", high = "#2ECC71") +
        coord_flip() +
        labs(
          title = "Top 10 Procedimentos",
          x = "Procedimento",
          y = "Número de Registros"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
          axis.text = element_text(size = 9),
          axis.title = element_text(size = 12),
          legend.position = "none"
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          autosize = TRUE,
          margin = list(l = 200, r = 50, b = 50, t = 50),  # Margem esquerda ainda maior
          yaxis = list(tickangle = 0)
        )
    })
    
    # Tabelas (mantidas iguais)
    output$tab_acoes_contagem <- DT::renderDT({
      d <- dados_prep()
      df <- d %>%
        distinct(Nome_Beneficiario, Acao_Judicial) %>%
        count(Acao_Judicial, name = "Número de Pacientes")
      DT::datatable(df, options = list(dom = 't'))
    })
    
    output$tab_acoes_especialistas <- DT::renderDT({
      d <- dados_prep() %>% filter(Acao_Judicial == "SIM")
      df <- d %>%
        distinct(Nome_Beneficiario, Espec_Solic) %>%
        count(Espec_Solic, sort = TRUE, name = "Número de Casos") %>%
        head(10)
      DT::datatable(df, options = list(pageLength = 5))
    })
    
    output$tab_acoes_procedimentos <- DT::renderDT({
      d <- dados_prep() %>% filter(Acao_Judicial == "SIM")
      df <- d %>%
        distinct(Nome_Beneficiario, Descricao_Item) %>%
        count(Descricao_Item, sort = TRUE, name = "Número de Casos") %>%
        head(10)
      DT::datatable(df, options = list(pageLength = 5))
    })
    
    output$tab_especialistas_registro <- DT::renderDT({
      df <- dados_prep() %>%
        count(Espec_Solic, sort = TRUE, name = "Total de Registros")
      DT::datatable(df, options = list(pageLength = 10))
    })
    
    output$tab_procedimentos_geral <- DT::renderDT({
      df <- contar_procedimentos(dados_prep())
      DT::datatable(df, options = list(pageLength = 10))
    })
  })
}