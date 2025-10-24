# modules/mod_beneficiario.R

mod_beneficiario_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Beneficiário")
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput(ns("plot_situacao"))),
        column(6,
               h4("Ações judiciais — contagem de pacientes"),
               tableOutput(ns("tab_acoes_contagem")))
      ),
      hr(),
      fluidRow(
        column(4,
               h4("Especialidades (apenas quem recorreu)"),
               tableOutput(ns("tab_acoes_especialistas"))),
        column(4,
               h4("Procedimentos (apenas quem recorreu)"),
               tableOutput(ns("tab_acoes_procedimentos"))),
        column(4,
               h4("Especialidades por registro (geral)"),
               tableOutput(ns("tab_especialistas_registro")))
      ),
      hr(),
      h4("Tipos de procedimentos (geral)"),
      tableOutput(ns("tab_procedimentos_geral"))
    )
  )
}

mod_beneficiario_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    dados_prep <- reactive({
      req(dados())
      preparar_dados(dados())
    })
    
    # Distribuição da situação do plano (já existente)
    output$plot_situacao <- renderPlot({
      hist_situacao_plano(dados_prep())
    })
    
    # ====== TABELAS EM DATA.FRAME (sem kable) ======
    
    # Ações judiciais — contagem de pacientes (SIM/NÃO)
    output$tab_acoes_contagem <- renderTable({
      d <- dados_prep()
      d %>%
        dplyr::distinct(Nome_Beneficiario, Acao_Judicial) %>%
        dplyr::count(Acao_Judicial, name = "n_pacientes")
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Especialidades para pacientes que recorreram
    output$tab_acoes_especialistas <- renderTable({
      d <- dados_prep() %>% dplyr::filter(Acao_Judicial == "SIM")
      d %>%
        dplyr::distinct(Nome_Beneficiario, Espec_Solic) %>%
        dplyr::count(Espec_Solic, sort = TRUE, name = "n_casos")
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Procedimentos para pacientes que recorreram
    output$tab_acoes_procedimentos <- renderTable({
      d <- dados_prep() %>% dplyr::filter(Acao_Judicial == "SIM")
      d %>%
        dplyr::distinct(Nome_Beneficiario, Descricao_Item) %>%
        dplyr::count(Descricao_Item, sort = TRUE, name = "n_casos")
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Especialidades por registro (geral)
    output$tab_especialistas_registro <- renderTable({
      dados_prep() %>%
        dplyr::count(Espec_Solic, sort = TRUE, name = "Total de Registros")
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Procedimentos (geral) — função já existente retorna tibble
    output$tab_procedimentos_geral <- renderTable({
      contar_procedimentos(dados_prep())
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}
