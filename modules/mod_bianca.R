mod_bianca_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Módulo Bianca"),
    tableOutput(ns("tabela"))
  )
}

mod_bianca_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    output$tabela <- renderTable({
      req(dados())
      head(dados())
    })
  })
}
