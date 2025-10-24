#UI

mod_gabrielle_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    h3("Painel de Análise Detalhada"),
    
    #criação de um conjunto de abas
    tabsetPanel(
      tabPanel("Visão Geral e Evolução", 
               fluidRow(
                 # Coluna para a tabela de procedimentos
                 column(width = 6,
                        h4("Procedimentos Mais Comuns"),
                        p("Contagem total de cada tipo de procedimento realizado."),
                        tableOutput(ns("tabela_procedimentos"))
                 ),
                 # Coluna para o gráfico de evolução
                 column(width = 6,
                        h4("Evolução do Número de Sessões por Ano"),
                        p("Total de horas de sessão registradas a cada ano."),
                        plotOutput(ns("plot_evolucao_sessoes"))
                 )
               )
      ),
      
      tabPanel("Análise de Intensidade por Idade",
               fluidRow(
                 # Coluna para o gráfico de correlação
                 column(width = 8,
                        h4("Idade vs. Média de Horas Mensais"),
                        p("Cada ponto é a média de horas de um paciente em uma determinada idade."),
                        plotOutput(ns("plot_correlacao_idade_horas"))
                 ),
                 # Coluna para o resultado do teste estatístico
                 column(width = 4,
                        h5("Teste de Correlação (Spearman)"),
                        verbatimTextOutput(ns("texto_correlacao")),
                        hr(),
                        h5("Evolução Média Geral"),
                        p("Média de horas (ponderada) para todas as crianças a cada idade."),
                        plotOutput(ns("plot_media_ponderada_idade"))
                 )
               )
      ),
      
      tabPanel("Análise Administrativa",
               fluidRow(
                 column(width = 6,
                        h4("Distribuição da Situação do Plano"),
                        plotOutput(ns("plot_situacao_plano"))
                 ),
                 column(width = 6,
                        h4("Contagem por Especialidade Solicitante"),
                        tableOutput(ns("tabela_especialistas"))
                 )
               ),
               hr()
               #h4("Painel de Ações Judiciais"),
               #fluidRow(
                 #column(width = 4, tableOutput(ns("tabela_acoes_contagem"))),
                 #column(width = 4, tableOutput(ns("tabela_acoes_especialistas"))),
                 #column(width = 4, tableOutput(ns("tabela_acoes_procedimentos")))
               #)
      )
    )
  )
}

#SERVER

mod_gabrielle_server <- function(id, dados){
  moduleServer(id, function(input, output, session){
    
    #Dados básicos processados
    dados_processados <- reactive({
      req(dados())
      d <- preparar_dados(dados())
      d <- criar_ano_registro(d, Competencia)
      return(d)
    })
    
    # Reactive 2: Dados com horas normalizadas, para análises de intensidade
    dados_horas_normalizadas <- reactive({
      req(dados_processados())
      horas_normalizadas(dados_processados())
    })
    
    #VISÃO GERAL E EVOLUÇÃO
    output$tabela_procedimentos <- renderTable({
      contar_procedimentos(dados_processados())
    }, striped = TRUE, hover = TRUE)
    
    output$plot_evolucao_sessoes <- renderPlot({
      plotar_sessoes_por_ano(dados_processados(), ano_registro, horas_por_registro)
    })
    
    #ANÁLISE DE INTENSIDADE POR IDADE
    analise_cor <- reactive({
      cor_idade_horas(dados_horas_normalizadas())
    })
    
    output$plot_correlacao_idade_horas <- renderPlot({
      analise_cor()$grafico
    })
    
    output$texto_correlacao <- renderPrint({
      analise_cor()$teste
    })
    
    graficos_evolucao <- reactive({
      graficos_evolucao_horas(dados_horas_normalizadas())
    })
    
    output$plot_media_ponderada_idade <- renderPlot({
      graficos_evolucao()$grafico_media_ponderada
    })
    
    #ANÁLISE ADMINISTRATIVA
    output$plot_situacao_plano <- renderPlot({
      hist_situacao_plano(dados_processados())
    })
    
    output$tabela_especialistas <- renderTable({
      dados_processados() %>%
        dplyr::count(Espec_Solic, sort = TRUE, name = "Total de Registros")
    }, striped = TRUE, hover = TRUE)
    
    #tabelas de Ações Judiciais
    #dados_com_acao <- reactive({
      #req(dados_processados())
      #dplyr::filter(dados_processados(), Acao_Judicial == "SIM")
    #})
    
    #output$tabela_acoes_contagem <- renderTable({
      #dados_processados() %>%
        #dplyr::distinct(Nome_Beneficiario, .keep_all = TRUE) %>%
        #dplyr::count(Acao_Judicial, name = "Nº de Pacientes")
   # }, caption = "Status Ação Judicial", caption.placement = "top")
    
    output$tabela_acoes_especialistas <- renderTable({
      dados_com_acao() %>%
        dplyr::count(Espec_Solic, sort = TRUE, name = "Nº de Casos")
    }, caption = "Especialidades em Ações", caption.placement = "top")
    
    output$tabela_acoes_procedimentos <- renderTable({
      dados_com_acao() %>%
        dplyr::count(Descricao_Item, sort = TRUE, name = "Nº de Casos")
    }, caption = "Procedimentos em Ações", caption.placement = "top")
    
  })
}