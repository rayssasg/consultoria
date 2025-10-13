mod_lucas_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Módulo Lucas"),
    tableOutput(ns("tabela"))
  )
}

mod_lucas_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    output$tabela <- renderTable({
      req(dados())
      head(dados())
    })
  })
}
