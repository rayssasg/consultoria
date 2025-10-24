library(shiny)

# Carregar funções gerais
source("funções.R")

# Módulos temáticos (novos)
source("modules/mod_analise_geral.R")
source("modules/mod_registros_sessoes.R")
source("modules/mod_beneficiario.R")
source("modules/mod_idade.R")
source("modules/mod_tempo_participacao.R")
source("modules/mod_horas_atendimento.R")

# Carregar módulos
#source("modules/mod_bianca.R")
#source("modules/mod_cauã.R")
#source("modules/mod_gabrielle.R")
#source("modules/mod_lucas.R")
#source("modules/mod_rayssa.R")

ui = navbarPage(
  "Consultoria",
  
  # Painel inicial para upload do banco
  tabPanel("Carregar Dados",
           sidebarLayout(
             sidebarPanel(
               fileInput("file", "Escolha um arquivo CSV", accept = ".csv"),
               checkboxInput("header", "Cabeçalho", TRUE),
               selectInput("sep", "Separador", choices = c("," = ",", ";" = ";"), selected = ","),
               selectInput("quote", "Aspas", choices = c("Nenhuma" = "", "Dupla" = '"', "Simples" = "'"), selected = '"')
             ),
             mainPanel(
               tableOutput("preview")
             )
           )
  ),
  
  tabPanel("Análise geral",        mod_analise_geral_ui("ag")),
  tabPanel("Registros e Sessões",  mod_registros_sessoes_ui("rs")),
  tabPanel("Beneficiário",         mod_beneficiario_ui("bf")),
  tabPanel("Idade",                mod_idade_ui("id")),
  tabPanel("Tempo de participação",mod_tempo_participacao_ui("tp")),
  tabPanel("Horas de atendimento", mod_horas_atendimento_ui("ha")))
  
  
  # Abas dos módulos
  #tabPanel("Bianca", mod_bianca_ui("bianca")),
  #tabPanel("Cauã", mod_caua_ui("cauã")),
  #tabPanel("Gabrielle", mod_gabrielle_ui("gabrielle")),
  #tabPanel("Lucas", mod_lucas_ui("lucas")),
  #tabPanel("Rayssa", mod_rayssa_ui("rayssa")))

server = function(input, output, session) {
  
  # Reactive para armazenar o banco carregado
  dados = reactive({
    req(input$file)
    read.csv(input$file$datapath, 
             header = input$header, 
             sep = input$sep, 
             quote = input$quote)
  })
  
  # Preview do banco
  output$preview = renderTable({
    req(dados())
    head(dados())
  })
  
  mod_analise_geral_server("ag", dados)
  mod_registros_sessoes_server("rs", dados)
  mod_beneficiario_server("bf", dados)
  mod_idade_server("id", dados)
  mod_tempo_participacao_server("tp", dados)
  mod_horas_atendimento_server("ha", dados)
  
  # Chamar os servidores dos módulos
  #mod_bianca_server("bianca", dados)
  #mod_caua_server("cauã", dados)
  #mod_gabrielle_server("gabrielle", dados)
  #mod_lucas_server("lucas", dados)
  #mod_rayssa_server("rayssa", dados)
}

shinyApp(ui, server)
