mod_analise_geral_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Análise geral")
    ),
    mainPanel(
      tableOutput(ns("tabela_resumo"))
    )
  )
}

mod_analise_geral_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    dados_prep <- reactive({ req(dados()); preparar_dados(dados()) })
    
    output$tabela_resumo <- renderTable({
      df <- estatisticas_gerais(dados_prep())
      colnames(df) <- c("Total de Procedimentos", "Crianças Distintas", "Cartões Distintos")
      df
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}
