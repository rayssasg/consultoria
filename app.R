# DEVE SER A PRIMEIRA LINHA!
library(shiny)

library(bslib)
library(ggplot2)
library(fontawesome)
library(DT)
library(dplyr)
library(lubridate)

# Tema elegante
tema_consultoria <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1E2A5E",
  secondary = "#5DADE2",
  success = "#18BC9C",
  base_font = font_google("Nunito Sans"),
  heading_font = font_google("Poppins"),
  "border-radius" = "1rem",
  "box-shadow" = "0 0.25rem 0.75rem rgba(0,0,0,0.1)"
)
bs_add_rules(tema_consultoria, "body { background-color: #f9fafc; }")

# Carregar funções e módulos
source("funções.R")
source("mod_analise_geral.R")
source("mod_registros_sessoes.R")
source("mod_beneficiario.R")
source("mod_idade.R")
source("mod_tempo_participacao.R")
source("mod_horas_atendimento.R")
source("mod_questionario.R")

# ---- UI ----
ui <- page_navbar(
  theme = tema_consultoria,
  title = div(icon("chart-line"), "Consultoria - Análise de Dados"),
  nav_panel("Carregar Dados",
            layout_columns(
              col_widths = c(4, 8),
              card(
                card_header("Upload do Arquivo"),
                card_body(
                  fileInput("file", "Escolha um arquivo CSV", accept = c(".csv", ".xlsx", ".xls"), buttonLabel = "Procurar..."),
                  checkboxInput("header", "Cabeçalho", TRUE),
                  selectInput("sep", "Separador", choices = c("," = ",", ";" = ";")),
                  selectInput("quote", "Aspas", choices = c("Nenhuma" = "", "Dupla" = '"', "Simples" = "'")),
                  hr(),
                  actionButton("upload_btn", "Carregar", class = "btn-primary w-100")
                )
              ),
              card(
                card_header("📊 Pré-visualização dos Dados"),
                card_body(
                  tableOutput("preview")
                )
              )
            )
  ),
  nav_panel("Análise Geral", mod_analise_geral_ui("ag")),
  nav_panel(" Registros e Sessões", mod_registros_sessoes_ui("rs")),
  nav_panel(" Beneficiário", mod_beneficiario_ui("bf")),
  nav_panel(" Idade", mod_idade_ui("id")),
  nav_panel(" Tempo Participação", mod_tempo_participacao_ui("tp")),
  nav_panel("Horas Atendimento", mod_horas_atendimento_ui("ha")),
  nav_panel("Questionário Famílias", mod_questionario_ui("quest"))
)

# ---- Server ----
server <- function(input, output, session) {
  dados <- reactiveVal(NULL)
  
  observeEvent(input$upload_btn, {
    req(input$file)
    tryCatch({
      df <- read.csv(
        input$file$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        encoding = "UTF-8"
      )
      dados(df)
      showNotification("Dados carregados com sucesso!", type = "message")
    }, error = function(e) {
      showNotification(paste("Erro ao carregar dados:", e$message), type = "error")
    })
  })
  
  output$preview <- renderTable({
    req(dados())
    head(dados(), 5)
  })
  
  # Chamar módulos
  mod_analise_geral_server("ag", dados)
  mod_registros_sessoes_server("rs", dados)
  mod_beneficiario_server("bf", dados)
  mod_idade_server("id", dados)
  mod_tempo_participacao_server("tp", dados)
  mod_horas_atendimento_server("ha", dados)
  mod_questionario_server("quest")
}

shinyApp(ui, server)