# modules/mod_registros_sessoes.R

mod_registros_sessoes_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Registros e Sessões")
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput(ns("plot_registros_ano"))),
        column(6, plotOutput(ns("plot_sessoes_ano")))
      ),
      fluidRow(
        column(12, plotOutput(ns("plot_dist_sessoes_registro")))
      ),
      hr(),
      h4("Tabela: Registros por ano"),
      tableOutput(ns("tab_registros_ano"))
    )
  )
}

mod_registros_sessoes_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    
    # dados preparados + coluna ano_registro (reuso de funções existentes)
    dados_prep <- reactive({
      req(dados())
      d <- preparar_dados(dados())
      d <- criar_ano_registro(d, Competencia)
      d
    })
    
    # Gráfico: quantidade de registros por ano (barras)
    output$plot_registros_ano <- renderPlot({
      d <- dados_prep()
      d_ano <- dplyr::count(d, ano_registro, name = "n")
      ggplot2::ggplot(d_ano, ggplot2::aes(x = as.numeric(ano_registro), y = n)) +
        ggplot2::geom_col(fill = "steelblue") +
        ggplot2::labs(title = "Quantidade de registros por ano", x = "Ano", y = "Registros") +
        ggplot2::theme_bw()
    })
    
    # Tabela: registros por ano (usa função já existente)
    output$tab_registros_ano <- renderTable({
      tabela_registros_por_ano_df(dados_prep(), ano_registro)   # <- usa a versão _df
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    
    # Gráfico: quantidade de sessões (itens/horas_por_registro) por ano (linha)
    output$plot_sessoes_ano <- renderPlot({
      plotar_sessoes_por_ano(dados_prep(), ano_registro, horas_por_registro)
    })
    
    # Distribuição: quantidade de sessões por registro (histograma/barra)
    output$plot_dist_sessoes_registro <- renderPlot({
      plotar_distribuicao_sessoes(dados_prep(), horas_por_registro)
    })
    
  })
}
