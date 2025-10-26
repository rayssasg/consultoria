library(dplyr)
library(lubridate)
library(ggplot2)
library(kableExtra)
library(tidyr)

# dados = read.csv("C:\\Users\\rayss\\Downloads\\0-3.csv", 
#                   header = TRUE, sep = ",", quote = '"')

carregar_pacotes = function() {
  pacotes = c("tidyverse", "kableExtra", "readxl", "googledrive", "lubridate")
  pacotes_nao_instalados = pacotes[!(pacotes %in% installed.packages()[,"Package"])]
  if(length(pacotes_nao_instalados)) install.packages(pacotes_nao_instalados)
  lapply(pacotes, library, character.only = TRUE)
  cat("Todos os pacotes necessários foram carregados!\n")
}

#carregar_pacotes()

renomear_colunas_padrao = function(dados) {
  novos_nomes = c(
    "Competencia", "Cartao", "Nome_Beneficiario", "Data_Nascto", "Idade", "Situacao",
    "Descricao_Item", "Qtde_Itens", "Duracao_sessao", "Nome_Solic", "Espec_Solic",
    "Dia_Atend", "Acao_Judicial"
  )
  
  # Verifica o número de colunas
  num_colunas_dados = ncol(dados)
  num_novos_nomes = length(novos_nomes)
  
  # Se os números forem diferentes, avisa no console em vez de parar o app
  if (num_colunas_dados != num_novos_nomes) {
    warning(paste("AVISO: O arquivo CSV tem", num_colunas_dados, 
                  "colunas, mas a função de renomear esperava", num_novos_nomes, 
                  "nomes. Verifique o arquivo de origem."))
  }
  
  # Renomeia apenas o número de colunas que existem
  # Isso evita o erro e permite que o app continue rodando para depuração
  nomes_para_usar <- novos_nomes[1:min(num_colunas_dados, num_novos_nomes)]
  names(dados)[1:length(nomes_para_usar)] <- nomes_para_usar
  
  return(dados)
}

# dados = renomear_colunas_padrao(dados)

# VERSÃO CORRIGIDA 
preparar_dados = function(dados) {
  dados = renomear_colunas_padrao(dados)
  dados %>%
    mutate(
      Competencia = ymd(Competencia),
      Data_Nascto = ymd(Data_Nascto)
    ) %>%
    mutate(
      idade_meses = (year(Competencia) - year(Data_Nascto)) * 12 +
        (month(Competencia) - month(Data_Nascto)) - (day(Competencia) < day(Data_Nascto)),
      idade_anos = idade_meses %/% 12
    ) %>%
    filter(idade_anos < 18) %>%
    group_by(Nome_Beneficiario) %>%
    mutate(
      janela_dias = as.numeric(difftime(max(Competencia), min(Competencia), units = "days")),
      janela_meses = interval(min(Competencia), max(Competencia)) %/% months(1),
      meses_participados = n_distinct(Competencia),
      numero_registros = n(),
      duracao_min = case_when(
        Duracao_sessao == "30 a 40 minutos" ~ 35,
        Duracao_sessao == "50 minutos" ~ 50,
        TRUE ~ NA_real_
      ),
      horas_por_registro = (duracao_min * Qtde_Itens)/60
    ) %>%
    ungroup()
}

# dados = preparar_dados(dados)

estatisticas_gerais = function(dados) {
  n_criancas = n_distinct(dados$Nome_Beneficiario)
  n_cartoes = n_distinct(dados$Cartao)
  total_proc = nrow(dados)
  cat("Procedimentos totais:", total_proc, "\n")
  cat("Crianças distintas:", n_criancas, "\n")
  cat("Cartões distintos:", n_cartoes, "\n\n")
  data.frame(
    total_procedimentos = total_proc,
    criancas_distintas = n_criancas,
    cartoes_distintos = n_cartoes
  )
}

# estatisticas_gerais(dados)

graficos_distribuicao = function(dados) {
  
  print(
    ggplot(dados, aes(x = janela_dias)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
      labs(x = "Janela de tempo (dias)", y = "Frequência", title = "Distribuição da janela de tempo (dias)") +
      theme_bw()
  )
  
  print(
    ggplot(dados, aes(x = janela_meses)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
      labs(x = "Janela de tempo (meses)", y = "Frequência", title = "Distribuição da janela de tempo (meses)") +
      theme_bw()
  )
  
  idades_inicial = dados %>%
    group_by(Nome_Beneficiario) %>%
    summarise(idade_inicial = min(idade_anos, na.rm = TRUE))
  
  print(
    ggplot(idades_inicial, aes(x = idade_inicial)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
      labs(x = "Idade inicial (anos)", y = "Frequência", title = "Distribuição da idade inicial") +
      theme_bw()
  )
  
  idades_final = dados %>%
    group_by(Nome_Beneficiario) %>%
    summarise(idade_final = max(idade_anos, na.rm = TRUE))
  
  print(
    ggplot(idades_final, aes(x = idade_final)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
      labs(x = "Idade final (anos)", y = "Frequência", title = "Distribuição da idade final") +
      theme_bw()
  )
  
}

# graficos_distribuicao(dados)

calcular_horas = function(dados) {
  dados %>%
    mutate(
      duracao_min = case_when(
        Duracao_sessao == "30 a 40 minutos" ~ 35,
        Duracao_sessao == "50 minutos" ~ 50,
        TRUE ~ NA_real_
      ),
      horas_por_registro = (duracao_min * Qtde_Itens)/60
    )
}

# dados = calcular_horas(dados)

calcular_horas_idade = function(dados) {
  if (!"horas" %in% names(dados)) {
    dados = dados %>%
      mutate(
        duracao_min = case_when(
          Duracao_sessao == "30 a 40 minutos" ~ 35,
          Duracao_sessao == "50 minutos" ~ 50,
          TRUE ~ NA_real_
        ),
        horas = (duracao_min * Qtde_Itens)/60
      )
  }
  return(dados)
}

# dados = calcular_horas_idade(dados)

criar_ano_registro = function(dados, coluna_data) {
  dados %>% mutate(ano_registro = format({{ coluna_data }}, "%Y"))
}

# criar_ano_registro(dados, Competencia)

tabela_registros_por_ano = function(dados, coluna_ano) {
  dados %>%
    count({{ coluna_ano }}, sort = TRUE, name = "total_registros") %>%
    kable(caption = "Quantidade de Registros por Ano.")
}

# tabela_registros_por_ano(dados, ano_registro)

calcular_sessoes_por_ano = function(dados, coluna_ano, coluna_sessoes) {
  dados %>%
    group_by({{ coluna_ano }}) %>%
    summarise(total_sessoes = sum({{ coluna_sessoes }}, na.rm = TRUE)) %>%
    arrange(desc(total_sessoes)) %>%
    ungroup()
}

# calcular_sessoes_por_ano(dados, ano_registro, horas_por_registro)

plotar_sessoes_por_ano = function(dados, coluna_ano, coluna_sessoes) {
  dados_sum = dados %>%
    group_by({{ coluna_ano }}) %>%
    summarise(total_sessoes = sum({{ coluna_sessoes }}, na.rm = TRUE))
  ggplot(dados_sum, aes(x = as.numeric({{ coluna_ano }}), y = total_sessoes)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_point(color = "steelblue", size = 3) +
    labs(title = "Evolução do Número de Sessões por Ano", x = "Ano do Registro", y = "Total de Sessões") +
    theme_bw()
}

# plotar_sessoes_por_ano(dados, ano_registro, horas_por_registro)

plotar_distribuicao_sessoes = function(dados, coluna_sessoes) {
  dados %>% ggplot(aes(x = {{ coluna_sessoes }})) +
    geom_bar(fill = "steelblue", color = "black") +
    labs(title = "Distribuição da Quantidade de Sessões por Registro", x = "Quantidade de Sessões", y = "Frequência (Contagem de Registros)") +
    theme_bw()
}

# plotar_distribuicao_sessoes(dados, horas_por_registro)

horas_normalizadas = function(dados) {
  dados1 = dados %>%
    group_by(Nome_Beneficiario, idade_anos) %>%
    mutate(meses_participados_idade = n_distinct(Competencia)) %>%
    ungroup()
  
  media_p11 = dados1 %>%
    group_by(Nome_Beneficiario, Competencia, idade_anos) %>%
    summarise(total_horas_mes = sum(horas_por_registro, na.rm = TRUE),
              peso = max(meses_participados_idade),
              .groups = "drop")
  
  media_p22 = media_p11 %>%
    group_by(Nome_Beneficiario, idade_anos) %>%
    summarise(media_horas_mensal_paciente = mean(total_horas_mes, na.rm = TRUE),
              peso = max(peso), .groups = "drop")
  
  media_p33 = media_p22 %>%
    group_by(idade_anos) %>%
    summarise(
      media_geral_horas_mensal_idade = mean(media_horas_mensal_paciente, na.rm = TRUE),
      media_ponderada_horas_mensal_idade = weighted.mean(media_horas_mensal_paciente, w = peso, na.rm = TRUE),
      .groups = "drop"
    )
  
  media_p33
}

# horas_normalizadas(dados)

resumo_por_crianca = function(dados) {
  proc_por_crianca = dados %>%
    group_by(Nome_Beneficiario) %>%
    summarise(
      n_procedimentos = n(),
      total_horas = sum(horas, na.rm = TRUE),
      .groups = "drop"
    )
  summary(proc_por_crianca)
}

# resumo_por_crianca(dados)

contar_procedimentos = function(dados) {
  dados %>%
    count(Descricao_Item, sort = TRUE)
}

# contar_procedimentos(dados)

hist_situacao_plano = function(dados) {
  dados_unicos_recente = dados %>%
    group_by(Nome_Beneficiario) %>%
    arrange(desc(idade_anos), desc(Competencia)) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  ggplot(dados_unicos_recente, aes(x = Situacao, fill = Situacao)) +
    geom_bar(color = "black", show.legend = FALSE) +
    scale_fill_manual(values = c("steelblue", "#235F77", "#939598")) +
    theme_bw() +
    labs(x = "Situação do plano", y = "Frequência", 
         title = "Distribuição da situação mais recente do plano de cada beneficiário")
}

# hist_situacao_plano(dados)

tabela_especialistas = function(dados) {
  dados %>%
    count(Espec_Solic, sort = TRUE) %>%
    kable(caption = "Contagem dos tipos de especialidades solicitantes por registro")
}

# tabela_especialistas(dados)

acoes_judiciais = function(dados) {
  tabela_contagem = dados %>%
    distinct(Nome_Beneficiario, `Acao_Judicial`) %>%
    count(Acao_Judicial) %>%
    kable(caption = "Nº de pacientes que recorreram a Ação Judicial")
  
  dados_com_acao = dados %>% filter(Acao_Judicial == "SIM")
  
  tabela_especialistas = dados_com_acao %>%
    distinct(Nome_Beneficiario, Espec_Solic) %>%
    count(Espec_Solic, sort = TRUE) %>%
    kable(caption = "Especialidades mais comuns em Ações Judiciais")
  
  tabela_procedimentos = dados_com_acao %>%
    distinct(Nome_Beneficiario, Descricao_Item) %>%
    count(Descricao_Item, sort = TRUE) %>%
    kable(caption = "Procedimentos mais comuns em Ações Judiciais")
  
  list(
    contagem = tabela_contagem,
    especialistas = tabela_especialistas,
    procedimentos = tabela_procedimentos
  )
}

# acoes_judiciais(dados)

estatisticas_gerais_final = function(dados) {
  n_criancas = n_distinct(dados$Nome_Beneficiario)
  n_cartoes = n_distinct(dados$Cartao)
  
  cat("Procedimentos totais:", nrow(dados), "\n")
  cat("Crianças distintas:", n_criancas, "\n")
  cat("Cartões distintos:", n_cartoes, "\n\n")
  
  cat("Resumo janela de dias:\n")
  print(summary(dados$janela_dias))
  
  cat("\nResumo janela de meses:\n")
  print(summary(dados$janela_meses))
  
  cat("\nResumo meses participados:\n")
  print(summary(dados$meses_participados))
  
  cat("\nResumo número de registros por criança:\n")
  print(summary(dados$numero_registros))
}

# estatisticas_gerais_final(dados)

tabela_horas_extremos = function(dados) {
  dados %>%
    distinct(Nome_Beneficiario, idade_anos, .keep_all = TRUE) %>%
    group_by(Nome_Beneficiario) %>%
    summarise(
      idade_primeira = min(idade_anos, na.rm = TRUE),
      horas_primeira = media_horas_mensal_paciente[which.min(idade_anos)],
      idade_recente = max(idade_anos, na.rm = TRUE),
      horas_recente = media_horas_mensal_paciente[which.max(idade_anos)],
      .groups = "drop"
    ) %>%
    mutate(dif_horas = horas_recente - horas_primeira) %>%
    filter(dif_horas != 0)
}

# tabela_horas_extremos(dados)

graficos_evolucao_horas = function(dados) {
  dados_grafico = dados %>%
    distinct(idade_anos, .keep_all = TRUE) %>%
    select(idade_anos, media_geral_horas_mensal_idade, media_ponderada_horas_mensal_idade)
  
  g_media = ggplot(dados_grafico, aes(x = idade_anos, y = media_geral_horas_mensal_idade)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_point(color = "steelblue") +
    labs(x = "Idade (anos)", y = "Média de horas mensais") +
    theme_bw()
  
  g_media_ponderada = ggplot(dados_grafico, aes(x = idade_anos, y = media_ponderada_horas_mensal_idade)) +
    geom_line(color = "darkred", size = 1.2) +
    geom_point(color = "darkred") +
    labs(x = "Idade (anos)", y = "Média ponderada de horas mensais") +
    theme_bw()
  
  list(
    grafico_media = g_media,
    grafico_media_ponderada = g_media_ponderada
  )
}

# graficos_evolucao_horas(dados)

grafico_diferenca_horas = function(dados) {
  dados %>%
    distinct(Nome_Beneficiario, idade_anos, .keep_all = TRUE) %>%
    group_by(Nome_Beneficiario) %>%
    summarise(
      idade_primeira = min(idade_anos, na.rm = TRUE),
      horas_primeira = media_horas_mensal_paciente[which.min(idade_anos)],
      idade_recente = max(idade_anos, na.rm = TRUE),
      horas_recente = media_horas_mensal_paciente[which.max(idade_anos)],
      .groups = "drop"
    ) %>%
    mutate(dif_horas = horas_recente - horas_primeira) %>%
    filter(dif_horas != 0) %>%
    ggplot(aes(x = dif_horas)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
    labs(x = "Diferença de horas (Positivo = Aumento com a idade)",
         y = "Nº de Crianças") +
    theme_bw()
}

# grafico_diferenca_horas(dados)

cor_idade_horas = function(dados) {
  grafico_dispersao = dados %>%
    ggplot(aes(x = idade_anos, y = media_horas_mensal_paciente)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    labs(title = "Idade x Média de horas mensais de atendimento (por criança)",
         x = "Idade (anos)", y = "Horas totais de atendimento") +
    theme_bw()
  
  teste_cor = cor.test(dados$idade_anos, dados$media_horas_mensal_paciente, method = "spearman")
  
  cat("--- Resultado do Teste de Correlação de Spearman ---\n")
  print(teste_cor)
  cat("--------------------------------------------------\n")
  
  list(
    grafico = grafico_dispersao,
    teste = teste_cor
  )
}

# cor_idade_horas(dados)

#NOVAS FUNÇÕES
#versões que retornam data.frame, não kable 

tabela_registros_por_ano_df = function(dados, coluna_ano) {
  dados %>%
    count({{ coluna_ano }}, sort = TRUE, name = "total_registros")
}

acoes_judiciais_dfs = function(dados) {
  tab_contagem = dados %>%
    distinct(Nome_Beneficiario, Acao_Judicial) %>%
    count(Acao_Judicial, name = "n_pacientes")
  
  dados_com_acao = dados %>% filter(Acao_Judicial == "SIM")
  
  tab_especialistas = dados_com_acao %>%
    distinct(Nome_Beneficiario, Espec_Solic) %>%
    count(Espec_Solic, sort = TRUE, name = "n_casos")
  
  tab_procedimentos = dados_com_acao %>%
    distinct(Nome_Beneficiario, Descricao_Item) %>%
    count(Descricao_Item, sort = TRUE, name = "n_casos")
  
  list(
    contagem = tab_contagem,
    especialistas = tab_especialistas,
    procedimentos = tab_procedimentos
  )
}

#sumario vetores
vec_summary_table = function(x) {
  s = summary(x)
  data.frame(
    estatistica = names(s),
    valor = unname(s),
    row.names = NULL,
    check.names = FALSE
  )
}

# nível "por criança e idade" para análises de idade
# esta é "a etapa 2" da horas_normalizadas() (o seu media_p22)
horas_normalizadas_nivel_crianca = function(dados) {
  dados1 = dados %>%
    group_by(Nome_Beneficiario, idade_anos) %>%
    mutate(meses_participados_idade = n_distinct(Competencia)) %>%
    ungroup()
  
  media_p11 = dados1 %>%
    group_by(Nome_Beneficiario, Competencia, idade_anos) %>%
    summarise(total_horas_mes = sum(horas_por_registro, na.rm = TRUE),
              peso = max(meses_participados_idade),
              .groups = "drop")
  
  media_p22 = media_p11 %>%
    group_by(Nome_Beneficiario, idade_anos) %>%
    summarise(media_horas_mensal_paciente = mean(total_horas_mes, na.rm = TRUE),
              peso = max(peso), .groups = "drop")
  
  media_p22
}