mod_tempo_participacao_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Tempo de participação"),
      sliderInput(ns("bins_dias"),   "Bins (dias)",   min = 1, max = 50, value = 20),
      sliderInput(ns("bins_meses"),  "Bins (meses)",  min = 1, max = 50, value = 20)
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput(ns("hist_dias"))),
        column(6, plotOutput(ns("hist_meses")))
      ),
      hr(),
      h4("Sumários"),
      fluidRow(
        column(6, tableOutput(ns("tab_meses_participados"))),   # (novo, 2 linhas)
        column(6, tableOutput(ns("tab_num_registros")))        # (novo, 2 linhas)
      )
    )
  )
}

mod_tempo_participacao_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    d0 <- reactive({ req(dados()); preparar_dados(dados()) })
    
    output$hist_dias <- renderPlot({
      ggplot(d0(), aes(x = janela_dias)) +
        geom_histogram(bins = input$bins_dias, fill = "steelblue", color = "black") +
        theme_bw() + labs(x = "Tempo entre 1º e último atendimento (dias)", y = "Frequência")
    })
    
    output$hist_meses <- renderPlot({
      ggplot(d0(), aes(x = janela_meses)) +
        geom_histogram(bins = input$bins_meses, fill = "steelblue", color = "black") +
        theme_bw() + labs(x = "Janela (meses)", y = "Frequência")
    })
    
    # (novo, 2 linhas) sumário meses_participados
    output$tab_meses_participados <- renderTable({
      vec_summary_table(d0()$meses_participados)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # (novo, 2 linhas) sumário numero_registros
    output$tab_num_registros <- renderTable({
      vec_summary_table(d0()$numero_registros)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}
