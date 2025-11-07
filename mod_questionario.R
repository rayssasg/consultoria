library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)
library(readr)
library(readxl)
library(tools)
library(forcats)
library(janitor)

# --- UI do Módulo Questionário ---
mod_questionario_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    # CSS para quebrar linha no menu suspenso
    tags$head(
      tags$style(HTML("
        .selectize-dropdown-content .option {
          white-space: normal !important; 
          word-wrap: break-word !important;
        }
      "))
    ),
    
    card(
      card_header(
        class = "bg-primary",
        tags$h4("Análise do Questionário das Famílias", style = "margin: 0; color: white;")
      ),
      card_body(
        sidebarLayout(
          sidebarPanel(
            width = 3,
            fileInput(ns("file_quest"), "Carregar Questionário (.csv ou .xlsx)", 
                      accept = c(".csv", ".xlsx", ".xls"),
                      buttonLabel = "Procurar...", 
                      placeholder = "Nenhum arquivo"),
            numericInput(ns("row_quest"), "Linha de início da leitura:", value = 1, min = 1, step = 1),
            hr(),
            
            selectInput(ns("var_cat"), "Selecione a pergunta:", choices = NULL),
            
            p("Selecione uma pergunta para ver sua tabela de frequência.")
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "Tabela de Frequência",
                br(),
                p("Esta tabela mostra a contagem e o percentual de cada resposta para a pergunta selecionada."),
                DTOutput(ns("tabela_frequencia"))
              ),
              tabPanel(
                "Dados Carregados",
                br(),
                p("Pré-visualização dos dados brutos carregados do arquivo do questionário."),
                DTOutput(ns("dados_quest_preview"))
              )
            )
          )
        )
      ),
      card_footer("Análise exploratória das respostas do questionário das famílias.")
    )
  )
}

# --- Server do Módulo Questionário ---
mod_questionario_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # --- 1. Ler os dados (Carrega todas as colunas) ---
    dados_questionario <- reactive({
      req(input$file_quest)
      infile <- input$file_quest
      ext <- tolower(tools::file_ext(infile$name))
      
      tryCatch({
        df_raw <- if (ext %in% c("csv")) {
          readr::read_csv(infile$datapath, skip = input$row_quest - 1, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
        } else if (ext %in% c("xls", "xlsx")) {
          readxl::read_excel(infile$datapath, skip = input$row_quest - 1)
        } else {
          stop("Formato de arquivo não suportado.")
        }
        
       
        nomes_originais_filtrados <- names(df_raw) 
        
        # Cria o dataframe com nomes limpos
        df_clean <- janitor::clean_names(df_raw)
        
        
        nomes_limpos <- names(df_clean)
        select_choices <- setNames(nomes_limpos, nomes_originais_filtrados)
        
        list(data = df_clean, choices = select_choices)
        
      }, error = function(e) {
        shiny::validate(need(FALSE, paste("Erro ao ler o arquivo do questionário:\n", e$message)))
      })
    })
    
    # Reactive separado apenas para os dados
    dados_quest_raw <- reactive({
      req(dados_questionario())
      dados_questionario()$data
    })
    
    # --- 2. Atualizar o seletor de variáveis ---
    observeEvent(dados_questionario(), {
      choices <- dados_questionario()$choices
      default_var <- choices[grepl("faixa de renda", names(choices), ignore.case = TRUE)][1] %||% choices[1]
      updateSelectInput(session, "var_cat", choices = choices, selected = default_var)
    })
    
    # --- 3. Tabela de Frequência  ---
    output$tabela_frequencia <- renderDT({
      req(dados_quest_raw(), input$var_cat)
      
      df_freq <- dados_quest_raw() %>%
        mutate(!!sym(input$var_cat) := as.character(!!sym(input$var_cat))) %>%
        count(!!sym(input$var_cat), sort = TRUE, name = "Frequencia") %>%
        mutate(Percentual = round(Frequencia / sum(Frequencia) * 100, 2)) %>%
        rename(Resposta = !!sym(input$var_cat))
      
      titulo_variavel <- names(dados_questionario()$choices)[dados_questionario()$choices == input$var_cat]
      
      datatable(
        df_freq,
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; color: #1E2A5E; font-size: 1.2em; font-weight: bold;',
          paste("Tabela de Frequência para:", titulo_variavel)
        ),
        options = list(pageLength = 10, scrollY = "400px", dom = 'tip',
                       language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'))
      )
    })
    
    # --- 4. Preview dos dados ---
    output$dados_quest_preview <- renderDT({
      req(dados_quest_raw())
      
      df_preview <- dados_quest_raw()
      
      datatable(df_preview, options = list(pageLength = 5, scrollX = TRUE))
    })
  })
}
