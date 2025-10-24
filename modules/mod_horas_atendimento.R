mod_horas_atendimento_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Horas de atendimento")
    ),
    mainPanel(
      h4("Tabela: horas por registro (resumo)"),
      tableOutput(ns("tab_horas_registro")),
      hr(),
      h4("Número de atendimentos e total de horas por paciente"),
      tableOutput(ns("tab_por_paciente"))
    )
  )
}

mod_horas_atendimento_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    d0 <- reactive({ req(dados()); preparar_dados(dados()) })
    
    output$tab_horas_registro <- renderTable({
      vec_summary_table(d0()$horas_por_registro)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    
    output$tab_por_paciente <- renderTable({
      # (novo, 2–3 linhas) usa horas_por_registro (já vem do preparar_dados)
      d0() %>%
        dplyr::group_by(Nome_Beneficiario) %>%
        dplyr::summarise(
          n_atendimentos = dplyr::n(),
          total_horas    = sum(horas_por_registro, na.rm = TRUE),
          .groups = "drop"
        ) %>% head(50)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}
