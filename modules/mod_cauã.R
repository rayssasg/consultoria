mod_caua_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Módulo Cauã"),
    tableOutput(ns("tabela"))
  )
}

mod_caua_server <- function(id, dados){ 
  moduleServer(id, function(input, output, session){
    output$tabela <- renderTable({
      req(dados())
      head(dados())
    })
  })
}
