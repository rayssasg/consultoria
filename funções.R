# pacotes

carregar_pacotes <- function() {
  pacotes <- c("tidyverse", "kableExtra", "readxl", "googledrive", "lubridate")
  
  # instala pacotes que ainda não estão instalados
  pacotes_nao_instalados <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]
  if(length(pacotes_nao_instalados)) {
    install.packages(pacotes_nao_instalados)
  }
  
  # carrega os pacotes
  lapply(pacotes, library, character.only = TRUE)
  
  cat("Todos os pacotes necessários foram carregados!\n")
}

# manipulações

unir_limpar_dados <- function(df1, df2) {
  library(dplyr)
  library(lubridate)
  
  # renomear coluna do segundo dataframe
  df2 <- df2 %>% rename("Competência" = "Comp.")
  
  # união completa
  dados <- full_join(df1, df2)
  
  # calcular idade em meses e anos
  dados <- dados %>%
    mutate(
      idade_meses = (year(Competência) - year(`Data Nascto`)) * 12 +
        (month(Competência) - month(`Data Nascto`)) - (day(Competência) < day(`Data Nascto`)),
      idade_anos = idade_meses %/% 12
    ) %>% select(-Idade) %>%
    filter(idade_anos < 18) %>%  # filtra menores de 18
    group_by(`Nome Beneficiário`) %>%
    mutate(
      janela_dias = difftime(max(Competência), min(Competência), units = "days"),
      janela_meses = interval(min(Competência), max(Competência)) %/% months(1),
      meses_participados = n_distinct(Competência),
      numero_registros = n()
    ) %>%
    ungroup()
  
  return(dados)
}

# análise descritiva

# resumo

estatisticas_gerais <- function(dados) {
  n_criancas <- n_distinct(dados$`Nome Beneficiário`)
  n_cartoes <- n_distinct(dados$`Nº Cartão`)
  
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

# variável idade 

graficos_distribuicao <- function(dados) {
  
  # boxplot da janela em dias
  print(
    ggplot(dados, aes(y = as.numeric(janela_dias))) +
      geom_boxplot(fill = "steelblue", color = "black") +
      labs(y = "Janela de tempo (dias)", title = "Distribuição da janela de tempo entre 1º e último atendimento") +
      theme_bw()
  )
  
  # boxplot da janela em meses
  print(
    ggplot(dados, aes(y = janela_meses)) +
      geom_boxplot(fill = "steelblue", color = "black") +
      labs(y = "Janela de tempo (meses)") +
      theme_bw()
  )
  
  # histograma de idade inicial
  idades_inicial <- dados %>%
    group_by(`Nome Beneficiário`) %>%
    summarise(idade_inicial = min(idade_anos, na.rm = TRUE))
  
  print(
    ggplot(idades_inicial, aes(x = idade_inicial)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
      labs(x = "Idade inicial (anos)", y = "Frequência") +
      theme_bw()
  )
  
  # histograma de idade final
  idades_final <- dados %>%
    group_by(`Nome Beneficiário`) %>%
    summarise(idade_final = max(idade_anos, na.rm = TRUE))
  
  print(
    ggplot(idades_final, aes(x = idade_final)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
      labs(x = "Idade final (anos)", y = "Frequência") +
      theme_bw()
  )
}

idades_criancas <- function(dados) {
  idades <- dados %>%
    group_by(`Nome Beneficiário`) %>%
    summarise(
      idade_primeira = min(idade_anos, na.rm = TRUE),
      idade_final = max(idade_anos, na.rm = TRUE)
    )
  
  cat("Tabela de igualdade entre idade inicial e final:\n")
  print(table(idades$idade_primeira == idades$idade_final))
  
  summary(idades)
}

plotar_distribuicao_idade = function(df, coluna_idade) {
  
  ggplot(df, aes(x = {{ coluna_idade }})) +
    # O valor de 'binwidth' agora está fixo em 1
    geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
    labs(
      x = "Idade (anos)",
      y = "Frequência (Nº de Registros)"
    ) +
    theme_bw()
  
}

# tempo/qtd atendimentos

calcular_horas = function(dados) {
  dados = dados %>%
    mutate(
      duracao_min = case_when(
        `Duração por sessão` == "30 a 40 minutos" ~ 35,
        `Duração por sessão` == "50 minutos" ~ 50,
        TRUE ~ NA_real_
      ),
      horas = (duracao_min * `Qtde (Itens)`)/60
    )
  dados = dados %>% mutate(horas_por_registro = (duracao_min*`Qtde (Itens)`)/60)
  return(dados)
}

# Função para calcular horas_total por criança e idade
calcular_horas_idade = function(dados) {
  # 1. garante que já tem a coluna horas
  if (!"horas" %in% names(dados)) {
    dados = dados %>%
      mutate(
        duracao_min = case_when(
          `Duração por sessão` == "30 a 40 minutos" ~ 35,
          `Duração por sessão` == "50 minutos" ~ 50,
          TRUE ~ NA_real_
        ),
        horas = (duracao_min * `Qtde (Itens)`)/60
      )
  }
  
  ##  # 2. calcula horas_total por criança e idade
  ##  horas_por_idade = dados %>%
  ##    group_by(`Nome Beneficiário`, idade_anos) %>%
  ##    summarise(horas_total = sum(horas, na.rm = TRUE),
  ##              n_procs = n(),
  ##              .groups = "drop")
  ##
  ##  # 3. junta de volta no df original
  ##  dados = dados %>%
  ##    left_join(horas_por_idade,
  ##              by = c("Nome Beneficiário", "idade_anos"))
  
  return(dados)
}

### identificar_extremos = function(horas_por_idade) {
###   horas_por_idade %>%
###     group_by(`Nome Beneficiário`) %>%
###     summarise(
###       idade_primeira = min(idade_anos, na.rm = TRUE),
###       horas_primeira = horas_total[which.min(idade_anos)],
###       idade_recente = max(idade_anos, na.rm = TRUE),
###       horas_recente = horas_total[which.max(idade_anos)],
###       .groups = "drop"
###     ) %>%
###     mutate(dif_horas = horas_recente - horas_primeira) %>%
###     filter(dif_horas != 0)
### }

criar_ano_registro = function(df, coluna_data) {
  df %>%
    mutate(ano_registro = format({{ coluna_data }}, format = "%Y"))
}

tabela_registros_por_ano = function(df, coluna_ano) {
  df %>%
    count({{ coluna_ano }}, sort = TRUE, name = "total_registros") %>%
    kable(caption = "Quantidade de Registros por Ano.")
}

calcular_sessoes_por_ano = function(df, coluna_ano, coluna_sessoes) {
  df %>%
    group_by({{ coluna_ano }}) %>%
    summarise(total_sessoes = sum({{ coluna_sessoes }})) %>%
    arrange(desc(total_sessoes)) %>%
    ungroup()
}

plotar_sessoes_por_ano = function(df, coluna_ano, coluna_sessoes) {
  df_sumarizado = df %>%
    group_by({{ coluna_ano }}) %>%
    summarise(total_sessoes = sum({{ coluna_sessoes }}, na.rm = TRUE)) # Adicionado na.rm
  
  ggplot(df_sumarizado, aes(x = as.numeric({{ coluna_ano }}), y = total_sessoes)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_point(color = "steelblue", size = 3) + # Adicionei pontos para melhor visualização
    labs(
      title = "Evolução do Número de Sessões por Ano",
      x = "Ano do Registro",
      y = "Total de Sessões"
    ) +
    theme_bw()
}

plotar_distribuicao_sessoes = function(df, coluna_sessoes) {
  df %>%
    ggplot(aes(x = {{ coluna_sessoes }})) +
    geom_bar(fill = "steelblue", color = "black") +
    labs(
      title = "Distribuição da Quantidade de Sessões por Registro",
      x = "Quantidade de Sessões",
      y = "Frequência (Contagem de Registros)"
    ) +
    theme_bw()
}

horas_normalizadas = function(df) {
  
  # Etapa 1: Criar coluna com os meses participados por pessoa e idade
  dados1 = df %>%
    group_by(`Nome Beneficiário`, idade_anos) %>%
    mutate(meses_participados_idade = n_distinct(Competência)) %>%
    ungroup()
  
  # Etapa 2: Calcular total de horas mensais por pessoa e o peso (meses participados)
  media_p11 = dados1 %>%
    group_by(`Nome Beneficiário`, Competência, idade_anos) %>%
    summarise(
      total_horas_mes = sum(horas_por_registro, na.rm = TRUE),
      peso = max(meses_participados_idade),
      .groups = "drop"
    )
  
  # Etapa 3: Calcular a média mensal de horas por pessoa para cada idade
  media_p22 = media_p11 %>%
    group_by(`Nome Beneficiário`, idade_anos) %>%
    summarise(
      media_horas_mensal_paciente = mean(total_horas_mes, na.rm = TRUE),
      peso = max(peso),
      .groups = "drop"
    )
  
  # Etapa 4: Calcular a média e média ponderada geral para cada idade
  media_p33 = media_p22 %>%
    group_by(idade_anos) %>%
    summarise(
      media_geral_horas_mensal_idade = mean(media_horas_mensal_paciente, na.rm = TRUE),
      media_ponderada_horas_mensal_idade = weighted.mean(media_horas_mensal_paciente, peso, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Etapa 5: Juntar todas as novas métricas de volta ao dataframe principal
  dados_finais = dados1 %>%
    left_join(
      media_p11 %>% select(`Nome Beneficiário`, Competência, idade_anos, total_horas_mes),
      by = c("Nome Beneficiário", "Competência", "idade_anos")
    ) %>%
    left_join(
      media_p22 %>% select(`Nome Beneficiário`, idade_anos, media_horas_mensal_paciente),
      by = c("Nome Beneficiário", "idade_anos")
    ) %>%
    left_join(
      media_p33 %>% select(idade_anos, media_geral_horas_mensal_idade, media_ponderada_horas_mensal_idade),
      by = "idade_anos"
    )
  return(list(
    dados = dados_finais,
    media_p22 = media_p22
  ))
  
}

tabela_horas_extremos = function(df) {
  
  horas_extremos = df %>%
    # Usa distinct para garantir um valor por paciente/idade
    distinct(`Nome Beneficiário`, idade_anos, .keep_all = TRUE) %>%
    group_by(`Nome Beneficiário`) %>%
    summarise(
      idade_primeira = min(idade_anos, na.rm = TRUE),
      horas_primeira = media_horas_mensal_paciente[which.min(idade_anos)],
      idade_recente = max(idade_anos, na.rm = TRUE),
      horas_recente = media_horas_mensal_paciente[which.max(idade_anos)],
      .groups = "drop"
    ) %>%
    mutate(dif_horas = horas_recente - horas_primeira) %>%
    filter(dif_horas != 0) # Filtra casos sem variação
  
  return(horas_extremos)
}

graficos_evolucao_horas = function(df) {
  
  # Prepara os dados para o gráfico (um valor por idade)
  dados_grafico = df %>%
    distinct(idade_anos, .keep_all = TRUE) %>%
    select(idade_anos, media_geral_horas_mensal_idade, media_ponderada_horas_mensal_idade)
  
  # Gráfico da média simples
  g_media = ggplot(dados_grafico, aes(x = idade_anos, y = media_geral_horas_mensal_idade)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_point(color = "steelblue") +
    labs(x = "Idade (anos)", y = "Média de horas mensais") +
    theme_bw()
  
  # Gráfico da média ponderada
  g_media_ponderada = ggplot(dados_grafico, aes(x = idade_anos, y = media_ponderada_horas_mensal_idade)) +
    geom_line(color = "darkred", size = 1.2) +
    geom_point(color = "darkred") +
    labs(x = "Idade (anos)", y = "Média ponderada de horas mensais") +
    theme_bw()
  
  return(list(
    grafico_media = g_media,
    grafico_media_ponderada = g_media_ponderada
  ))
}

grafico_diferenca_horas = function(df) {
  
  horas_extremos = df %>%
    # Usa distinct para garantir um valor por paciente/idade
    distinct(`Nome Beneficiário`, idade_anos, .keep_all = TRUE) %>%
    group_by(`Nome Beneficiário`) %>%
    summarise(
      idade_primeira = min(idade_anos, na.rm = TRUE),
      horas_primeira = media_horas_mensal_paciente[which.min(idade_anos)],
      idade_recente = max(idade_anos, na.rm = TRUE),
      horas_recente = media_horas_mensal_paciente[which.max(idade_anos)],
      .groups = "drop"
    ) %>%
    mutate(dif_horas = horas_recente - horas_primeira) %>%
    filter(dif_horas != 0)
  
  grafico = horas_extremos %>%
    ggplot(aes(x = dif_horas)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
    labs(
      x = "Diferença de horas (Positivo = Aumento com a idade)",
      y = "Nº de Crianças"
    ) +
    theme_bw()
  
  return(grafico)
}

cor_idade_horas = function(df) {
  
  # 1. Gerar o gráfico de dispersão
  grafico_dispersao = df %>%
    ggplot(aes(x=idade_anos, y= media_horas_mensal_paciente)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    labs(title = "Idade x Média de horas mensais de atendimento (por criança)",
         x = "Idade (anos)",
         y = "Horas totais de atendimento") +
    theme_bw()
  
  # 2. Realizar o teste de correlação
  teste_cor = cor.test(
    df$idade_anos,
    df$media_horas_mensal_paciente,
    method = "spearman"
  )
  
  # Imprime o resultado do teste no console para visualização imediata
  cat("--- Resultado do Teste de Correlação de Spearman ---\n")
  print(teste_cor)
  cat("--------------------------------------------------\n")
  
  # 3. Retornar uma lista com o gráfico e o resultado do teste
  return(list(
    grafico = grafico_dispersao,
    teste = teste_cor
  ))
}

# procedimentos

resumo_por_crianca = function(dados) {
  proc_por_crianca = dados %>%
    group_by(`Nome Beneficiário`) %>%
    summarise(
      n_procedimentos = n(),
      total_horas = sum(horas, na.rm = TRUE),
      .groups = "drop"
    )
  summary(proc_por_crianca)
}

contar_procedimentos = function(dados) {
  dados %>%
    count(`Descrição Item`, sort = TRUE)
}

# situação do plano

hist_situacao_plano = function(df) {
  
  # Filtra para pegar apenas o registro mais recente de cada beneficiário
  dados_unicos_recente = df %>%
    group_by(`Nome Beneficiário`) %>%
    arrange(desc(idade_anos), desc(Competência)) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  grafico = ggplot(dados_unicos_recente, aes(x = Situação, fill = Situação)) +
    geom_bar(color = "black", show.legend = FALSE) +
    scale_fill_manual(values = c("steelblue", "#235F77", "#939598")) +
    theme_bw() +
    labs(
      x = "Situação do plano",
      y = "Frequência",
      title = "Distribuição da situação mais recente do plano de cada beneficiário"
    )
  
  return(grafico)
}

# especialidades

tabela_especialistas = function(df) {
  
  tabela = df %>%
    count(`Espec. (Solic)`, sort = TRUE) %>%
    kable(caption = "Contagem dos tipos de especialidades solicitantes por registro")
  
  return(tabela)
}

# ação judicial

acoes_judiciais = function(df) {
  
  # Tabela 1: Contagem de pacientes com e sem ação judicial
  tabela_contagem = df %>%
    distinct(`Nome Beneficiário`, `Ação Judicial`) %>%
    count(`Ação Judicial`) %>%
    kable(caption = "Nº de pacientes que recorreram a Ação Judicial")
  
  # Filtra dados apenas de quem teve ação judicial
  df_com_acao = df %>%
    filter(`Ação Judicial` == "SIM")
  
  # Tabela 2: Especialistas mais comuns em casos com ação judicial
  tabela_especialistas = df_com_acao %>%
    distinct(`Nome Beneficiário`, `Espec. (Solic)`) %>%
    count(`Espec. (Solic)`, sort = TRUE) %>%
    kable(caption = "Especialidades mais comuns em Ações Judiciais")
  
  # Tabela 3: Procedimentos mais comuns em casos com ação judicial
  tabela_procedimentos = df_com_acao %>%
    distinct(`Nome Beneficiário`, `Descrição Item`) %>%
    count(`Descrição Item`, sort = TRUE) %>%
    kable(caption = "Procedimentos mais comuns em Ações Judiciais")
  
  return(list(
    contagem = tabela_contagem,
    especialistas = tabela_especialistas,
    procedimentos = tabela_procedimentos
  ))
}