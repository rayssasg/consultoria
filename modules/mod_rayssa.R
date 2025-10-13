mod_rayssa_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Módulo Rayssa"),
    tableOutput(ns("tabela"))
  )
}

mod_rayssa_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    output$tabela <- renderTable({
      req(dados())
      head(dados())
    })
  })
}
