library(shiny)

# Carregar funções gerais
source("funções.R")

# Carregar módulos
source("modules/mod_bianca.R")
source("modules/mod_caua.R")
source("modules/mod_gabrielle.R")
source("modules/mod_lucas.R")
source("modules/mod_rayssa.R")

ui <- navbarPage(
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
  
  # Abas dos módulos
  tabPanel("Bianca", mod_bianca_ui("bianca")),
  tabPanel("Cauã", mod_caua_ui("cauã")),
  tabPanel("Gabrielle", mod_gabrielle_ui("gabrielle")),
  tabPanel("Lucas", mod_lucas_ui("lucas")),
  tabPanel("Rayssa", mod_rayssa_ui("rayssa"))
)

server <- function(input, output, session) {
  
  # Reactive para armazenar o banco carregado
  dados <- reactive({
    req(input$file)
    read.csv(input$file$datapath, 
             header = input$header, 
             sep = input$sep, 
             quote = input$quote)
  })
  
  # Preview do banco
  output$preview <- renderTable({
    req(dados())
    head(dados())
  })
  
  # Chamar os servidores dos módulos
  mod_bianca_server("bianca", dados)
  mod_caua_server("cauã", dados)
  mod_gabrielle_server("gabrielle", dados)
  mod_lucas_server("lucas", dados)
  mod_rayssa_server("rayssa", dados)
}

shinyApp(ui, server)
