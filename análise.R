# ==============================================================================
# TRABALHO PRÁTICO 1 - ESTATÍSTICA PARA ENGENHARIA (TI0111)
# Script R: Análise Descritiva e Bivariada (Questões 1, 2 e 3)
# ==============================================================================

# ------------------------------------------------------------------------------
# QUESTÃO 1 E 2: LEITURA DOS DADOS E AMOSTRA DO GRUPO
# ------------------------------------------------------------------------------

# Leitura do dataset no diretório correto
if (file.exists("dados/HW1_bike_sharing.csv")) {
  dados_completos <- read.csv("dados/HW1_bike_sharing.csv")
} else if (file.exists("HW1_bike_sharing.csv")) {
  dados_completos <- read.csv("HW1_bike_sharing.csv")
} else {
  stop("O arquivo HW1_bike_sharing.csv nao foi encontrado na pasta atual!")
}

# Maior matricula do grupo: M = 565324
# Deslocamento inicial: r = 1 + (565324 %% 100) = 25
r <- 25
n_obs <- 300

# Selecionando o trecho da amostra do grupo (linhas 25 a 324)
data_group <- dados_completos[r:(r + n_obs - 1), ]

# Criando a variavel total_user (soma de casual + registered)
data_group$total_user <- data_group$casual + data_group$registered

cat("========================================================\n")
cat(" AMOSTRA GERADA COM SUCESSO (300 OBSERVAÇÕES)\n")
cat("========================================================\n\n")

# Reforçando o recorte da amostra conforme instrução
M <- 565324
r <- 1 + (M %% 100) # r = 25
data_group <- dados_completos[r:(r + 299), ]

# Garantindo a variável total_user na base ajustada
data_group$total_user <- data_group$casual + data_group$registered

# Função auxiliar para calcular a Moda
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculando as estatisticas do Item 2
media_tu   <- mean(data_group$total_user)
mediana_tu <- median(data_group$total_user)
moda_tu    <- get_mode(data_group$total_user)

# Mostrando os resultados do Item 2 no console
cat("=== RESULTADOS DO ITEM 2 ===\n")
cat("Média:  ", round(media_tu, 2), "\n")
cat("Mediana:", round(mediana_tu, 2), "\n")
cat("Moda:   ", moda_tu, "\n\n")

# ------------------------------------------------------------------------------
# QUESTÃO 2 - ITEM 3: QUARTIS E OUTLIERS
# ------------------------------------------------------------------------------

# Cálculo dos Quartis e Amplitude Interquartil
Q1 <- quantile(data_group$total_user, 0.25)
Q2 <- quantile(data_group$total_user, 0.50) # Mediana
Q3 <- quantile(data_group$total_user, 0.75)
IQR_tu <- IQR(data_group$total_user)

# Definição dos limites para identificar outliers
limite_inferior <- Q1 - 1.5 * IQR_tu
limite_superior <- Q3 + 1.5 * IQR_tu

# Filtrando os pontos fora dos limites
outliers <- subset(data_group, total_user < limite_inferior | total_user > limite_superior)
qtd_outliers <- nrow(outliers)

# Exibindo os resultados das separatrizes
cat("=== RESULTADOS DO ITEM 3 ===\n")
cat("Q1 (1º Quartil):", Q1, "\n")
cat("Q2 (Mediana):", Q2, "\n")
cat("Q3 (3º Quartil):", Q3, "\n")
cat("IQR:", IQR_tu, "\n")
cat("Limite Inferior:", limite_inferior, "\n")
cat("Limite Superior:", limite_superior, "\n")
cat("Quantidade de Outliers:", qtd_outliers, "\n\n")

if (qtd_outliers > 0) {
  cat("Datas e valores dos outliers encontrados:\n")
  print(outliers[, c("dteday", "total_user")])
} else {
  cat("Não foram encontrados outliers com este critério.\n\n")
}

# ------------------------------------------------------------------------------
# QUESTÃO 2 - ITEM 4: EXPORTAÇÃO DO HISTOGRAMA E BOXPLOT SEPARADOS
# ------------------------------------------------------------------------------

media_val   <- mean(data_group$total_user)
mediana_val <- median(data_group$total_user)

# 1. Gerando o Histograma
png("histograma.png", width = 1800, height = 1500, res = 300)
par(mar = c(4.5, 4.5, 2.5, 1))

hist(data_group$total_user,
     main = "Histograma de total_user",
     xlab = "Total de Usuários por Dia",
     ylab = "Frequência Absoluta",
     col = "lightblue",
     border = "black",
     breaks = 12,
     ylim = c(0, 80),
     las = 1)

# Adicionando as linhas verticais para Media e Mediana
abline(v = media_val, col = "red", lwd = 2, lty = 2)
abline(v = mediana_val, col = "blue", lwd = 2, lty = 1)

legend("topleft", 
       legend = c(sprintf("Média (%.2f)", media_val), 
                  sprintf("Mediana (%.2f)", mediana_val)),
       col = c("red", "blue"), 
       lty = c(2, 1), 
       lwd = 2, 
       bty = "o",
       bg = "white",
       cex = 0.85)

dev.off()

# 2. Gerando o Boxplot
png("box_plot.png", width = 1800, height = 1500, res = 300)
par(mar = c(4.5, 4.5, 2.5, 1))

boxplot(data_group$total_user,
        main = "Boxplot de total_user",
        ylab = "Número Total de Usuários",
        col = "lightgreen",
        border = "black",
        las = 1)

dev.off()

# ------------------------------------------------------------------------------
# QUESTÃO 2 - ITEM 5: CRIAÇÃO DA VARIÁVEL LOW_USAGE
# ------------------------------------------------------------------------------

# Resgatando Q1 para categorizar os dias de baixo uso
Q1 <- quantile(data_group$total_user, 0.25)

# 1 se for abaixo de Q1, 0 caso contrario
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

qtd_low_usage  <- sum(data_group$low_usage)
total_obs      <- nrow(data_group)
prop_low_usage <- qtd_low_usage / total_obs
pct_low_usage  <- prop_low_usage * 100

cat("=== RESULTADOS DO ITEM 5 ===\n")
cat("Valor do 1º Quartil (Q1):", Q1, "\n")
cat("Quantidade de dias com low_usage = 1:", qtd_low_usage, "\n")
cat("Proporção de dias (frequência relativa):", round(prop_low_usage, 4), "\n")
cat("Porcentagem de dias com baixa utilização:", round(pct_low_usage, 2), "%\n\n")

# ------------------------------------------------------------------------------
# QUESTÃO 3 - ITEM 1: ANÁLISE POR ESTAÇÃO (SEASON)
# ------------------------------------------------------------------------------

media_est   <- tapply(data_group$total_user, data_group$season, mean)
mediana_est <- tapply(data_group$total_user, data_group$season, median)
sd_est      <- tapply(data_group$total_user, data_group$season, sd)

prop_low_est <- tapply(data_group$low_usage, data_group$season, mean)

tabela_estacoes <- data.frame(
  Estacao = c("1: Primavera", "2: Verão", "3: Outono", "4: Inverno"),
  Media = round(media_est, 2),
  Mediana = round(mediana_est, 2),
  Desvio_Padrao = round(sd_est, 2),
  Prop_Low_Usage = round(prop_low_est, 4),
  Pct_Low_Usage = paste0(round(prop_low_est * 100, 2), "%")
)

cat("=== RESULTADOS DA QUESTÃO 3 - ITEM 1 ===\n")
print(tabela_estacoes)
cat("\n")

# Boxplot comparando as estacoes do ano
boxplot(total_user ~ season, data = data_group,
        main = "Total de Usuários por Estação do Ano",
        xlab = "Estação do Ano",
        ylab = "Total de Usuários Diários",
        col = c("lightgreen", "khaki", "orange", "lightblue"),
        names = c("Primavera", "Verão", "Outono", "Inverno"))

# ------------------------------------------------------------------------------
# QUESTÃO 3 - ITEM 2: CONDIÇÕES METEOROLÓGICAS (WEATHERSIT)
# ------------------------------------------------------------------------------

media_clima    <- tapply(data_group$total_user, data_group$weathersit, mean)
sd_clima       <- tapply(data_group$total_user, data_group$weathersit, sd)
prop_low_clima <- tapply(data_group$low_usage, data_group$weathersit, mean)

tabela_clima <- data.frame(
  Condicao_Meteorologica = names(media_clima),
  Media = round(media_clima, 2),
  Desvio_Padrao = round(sd_clima, 2),
  Prop_Low_Usage = round(prop_low_clima, 4),
  Pct_Low_Usage = paste0(round(prop_low_clima * 100, 2), "%")
)

cat("=== RESULTADOS DA QUESTÃO 3 - ITEM 2 ===\n")
print(tabela_clima)
cat("\n")

# Boxplot por condicao do clima
boxplot(total_user ~ weathersit, data = data_group,
        main = "Total de Usuários por Condição Meteorológica",
        xlab = "Condição Meteorológica (1: Céu Limpo | 2: Nublado | 3: Chuva Fraca)",
        ylab = "Total de Usuários Diários",
        col = c("skyblue", "lightgray", "coral"))

# ------------------------------------------------------------------------------
# QUESTÃO 3 - ITEM 3: TEMPERATURA E CORRELAÇÃO
# ------------------------------------------------------------------------------

cor_temp <- cor(data_group$temp, data_group$total_user)

media_temp_low <- tapply(data_group$temp, data_group$low_usage, mean)
sd_temp_low    <- tapply(data_group$temp, data_group$low_usage, sd)

cat("=== RESULTADOS DA QUESTÃO 3 - ITEM 3 ===\n")
cat("Correlação de Pearson (temp vs total_user):", round(cor_temp, 4), "\n\n")

cat("Temperatura Média (normalizada):\n")
cat(" - Dias NORMAIS (low_usage = 0):", round(media_temp_low["0"], 4), "\n")
cat(" - Dias de BAIXA UTILIZAÇÃO (low_usage = 1):", round(media_temp_low["1"], 4), "\n\n")

cat("Desvio Padrão da Temperatura:\n")
cat(" - Dias NORMAIS (low_usage = 0):", round(sd_temp_low["0"], 4), "\n")
cat(" - Dias de BAIXA UTILIZAÇÃO (low_usage = 1):", round(sd_temp_low["1"], 4), "\n\n")

# Grafico de dispersao (temp vs total_user)
plot(data_group$temp, data_group$total_user,
     main = "Relação entre Temperatura e Total de Usuários",
     xlab = "Temperatura (Normalizada)",
     ylab = "Total de Usuários Diários",
     pch = 19, 
     col = ifelse(data_group$low_usage == 1, "red", "steelblue"))

# Reta de tendencia linear
abline(lm(total_user ~ temp, data = data_group), col = "darkred", lwd = 2, lty = 2)

legend("topleft", 
       legend = c("Uso Normal (low_usage = 0)", "Baixa Utilização (low_usage = 1)", "Tendência Linear"),
       col = c("steelblue", "red", "darkred"), 
       pch = c(19, 19, NA), 
       lty = c(NA, NA, 2), 
       lwd = c(NA, NA, 2),
       cex = 0.8)

# ------------------------------------------------------------------------------
# QUESTÃO 3 - ITEM 4: TABELAS CRUZADAS
# ------------------------------------------------------------------------------

cat("=== RESULTADOS DA QUESTÃO 3 - ITEM 4 ===\n")
cat("Tabela Cruzada: Estação vs Low Usage\n")
print(table(data_group$season, data_group$low_usage))

cat("\nTabela Cruzada: Clima vs Low Usage\n")
print(table(data_group$weathersit, data_group$low_usage))