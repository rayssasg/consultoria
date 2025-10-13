mod_gabrielle_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Módulo Gabrielle"),
    tableOutput(ns("tabela"))
  )
}

mod_gabrielle_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    output$tabela <- renderTable({
      req(dados())
      head(dados())
    })
  })
}
