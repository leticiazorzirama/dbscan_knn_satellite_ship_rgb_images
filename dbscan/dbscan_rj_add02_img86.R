# TODO
# Cabeçalho

# Carregar pacotes
library(imager)
library(dplyr)
library(tidyr)
library(dbscan)

# Definir diretório de trabalho, se necessário
setwd("dbscan")

# Importação das imagens
# Carregar a imagem
img <- "RJ_add02_img86"
rj_add02_img86 <- load.image(paste0(img, ".jpg"))

# Visualizar a imagem
plot(rj_add02_img86)

# Transformação das imagens para matrizes numéricas baseadas no RGB por pixel
rj_add02_img86_matriz <- as.data.frame(rj_add02_img86, wide = "c")

# colunas: x, y, c.1 (R), c.2 (G), c.3 (B)
names(rj_add02_img86_matriz)[3:5] <- c("R", "G", "B")

# Estrutura da matriz
str(rj_add02_img86_matriz)
# x = coordenadas x do pixel = integer
# y = coordenadas y do pixel = integer
# R, G, B = valor do pixel para cada canal

# Dimensões da matriz
dim(rj_add02_img86_matriz)

# Resumo estatístico
summary(rj_add02_img86_matriz)

# Subamostragem para redução de dimensionalidade 
# a partir da implementação de uma Amostragem Aleatória Simples de pixels sem reposição
# com análise de sensibilidade para a definição do N amostral

seed <- 42
set.seed(seed)

# Setar porcentagem amostra
prop <- 0.1
rj <- rj_add02_img86_matriz %>% slice_sample(prop = prop)
dim(rj)
summary(rj)

# Normalização dos dados de RGB com base no score-Z, 
# ou seja, distribuição normal padrão com média 0 e desvio padrão 1;
rj_norm_rgb <- rj %>% mutate(across(c(R, G, B), ~ as.numeric(scale(.x))))

# Aplicação do método de elbow para otimização e estimação do número de grupos (clusters) 
# baseado na densidade de pixels vizinhos definidos por proximidade e similaridade utilizando 
# kNN (definição do parâmetro epsilon do modelo);

# Ester et al., 1996 recomendam 4 como minPts e Eps conforme o 4-dist graph
# Hahsler et al., 2019 recomendam minPts = dim + 1
# testes com outros minPts foram feitos
dim(rj_norm_rgb)
minPts <- dim(rj_norm_rgb)[2] + 1

# Eps (epsilon) com base no método de elbow
kNNdistplot(rj_norm_rgb, minPts = minPts) 
eps <- 5.7
abline(h = eps, lty = 2)
title(main = paste0(img, " - Definição do eps para o DBSCAN com base no 'elbow' com minPts = ", minPts))

# Montar identificador para salvar os arquivos 
nome_base   <- tools::file_path_sans_ext(basename(img))
id_teste    <- paste0(nome_base, "_prop", prop, "_eps", eps, "_minPts", minPts)
timestamp   <- format(Sys.time(), "%Y%m%d")
id_teste    <- paste0(timestamp, "_", id_teste)

cat("ID do teste:", id_teste, "\n")

# Salvar manualmente kNNdistplot usando a string abaixo
knndistplot <- paste0(id_teste,"_knndistplot")
knndistplot

# Treinar o dbscan com mensuração do tempo cronológico e uso de processador
tempo_treino <- system.time({
rj_norm_rgb_dbscan_model <- dbscan(rj_norm_rgb, eps = eps, minPts = minPts)
})

# Predição com o dbscan com a matriz original com mensuração do tempo cronológico e uso de processador
tempo_predicao <- system.time({ 
rj_norm_rgb_dbscan_predict <- predict(rj_norm_rgb_dbscan_model, rj_add02_img86_matriz, rj_norm_rgb)
})

# Resultados
# Tabela dos clusters
table(rj_norm_rgb_dbscan_predict)

# Visualização dos clusters
plot(
  rj_norm_rgb_dbscan_predict, 
  col=rj_norm_rgb_dbscan_predict, 
  main = paste0(
    img, " - Agrupamentos DBSCAN treinado com amostra de ", prop*100, "% eps = ", eps, " minPts = ", minPts))

# Salvar manualmente Agrupamentos DBSCAN usando a string abaixo
agrupamentos <- paste0(id_teste,"_agrupamentos")
agrupamentos

# Anexar o cluster predito à matriz completa
rj_add02_img86_matriz$cluster <- rj_norm_rgb_dbscan_predict

# Calcular RGB médio por cluster (excluindo ruído = cluster 0)
media_rgb_cluster <- rj_add02_img86_matriz %>%
  filter(cluster != 0) %>%
  group_by(cluster) %>%
  summarise(
    R_mean = mean(R),
    G_mean = mean(G),
    B_mean = mean(B),
    .groups = "drop"
  )

# Substituir RGB de cada pixel pelo RGB médio do seu cluster
rj_add02_img86_dbscan_matriz <- rj_add02_img86_matriz %>%
  left_join(media_rgb_cluster, by = "cluster") %>%
  mutate(
    R = ifelse(cluster == 0, R, R_mean),  # ruído mantém cor original
    G = ifelse(cluster == 0, G, G_mean),
    B = ifelse(cluster == 0, B, B_mean)
  ) %>%
  select(x, y, R, G, B)

# Garantir que os valores estão em [0, 1] (imager usa essa escala)
rj_add02_img86_dbscan_matriz <- rj_add02_img86_dbscan_matriz %>%
  mutate(across(c(R, G, B), ~ pmin(pmax(.x, 0), 1)))  # clamp, não rescale

# Reconstruir o objeto imagem do imager
# imager precisa do formato longer: x, y, cc (canal), value
rj_add02_img86_dbscan_matriz_longer <- rj_add02_img86_dbscan_matriz %>%
  pivot_longer(cols = c(R, G, B),
               names_to  = "canal",
               values_to = "value") %>%
  mutate(
    cc = case_when(
      canal == "R" ~ 1L,
      canal == "G" ~ 2L,
      canal == "B" ~ 3L
    ),
    z = 1L  # imagem estática, sem frames
  ) %>%
  select(x, y, z, cc, value) %>%
  arrange(cc, y, x)

# Converter matriz para objeto cimg
width  <- max(rj_add02_img86_matriz$x)
height <- max(rj_add02_img86_matriz$y)

rj_add02_img86_dbscan <- as.cimg(
  rj_add02_img86_dbscan_matriz_longer$value,
  x = width,
  y = height,
  z = 1,
  cc = 3
)

# Visualizar imagem resultante
plot(rj_add02_img86_dbscan, main = id_teste)

# Salvar imagem resultante em .jpg
img_jpg <- paste0(id_teste, ".jpg")
save.image(rj_add02_img86_dbscan, img_jpg)
# ou em .png (sem compressão):
img_png <- paste0(id_teste, ".png")
save.image(rj_add02_img86_dbscan, img_png)

# Imprimir e exportar estatísticas, parâmetros, mensurações e resultados do teste

# Estatísticas
estatisticas_completa <- summary(rj_add02_img86_matriz)
estatisticas_amostra   <- summary(rj)

# Parâmetros do teste
parametros <- list(
  imagem            = img,
  n_pixels_total    = nrow(rj_add02_img86_matriz),
  seed              = seed,
  prop_amostra      = prop,
  n_pixels_amostra  = nrow(rj),
  minPts            = minPts,
  knndistplot       = paste0(knndistplot,".png"),
  eps               = eps
)

# Mensurações
mensuracoes <- list(
  tempo_treino_seg   = as.numeric(tempo_treino["elapsed"]),
  tempo_predicao_seg = as.numeric(tempo_predicao["elapsed"]),
  tempo_total_seg    = as.numeric(tempo_treino["elapsed"] + tempo_predicao["elapsed"])
)

# Resultados
tabela_clusters <- table(rj_norm_rgb_dbscan_predict)
tabela_clusters <- as.data.frame(tabela_clusters)
tabela_media_rgb_cluster <- as.data.frame(media_rgb_cluster)
n_clusters      <- length(unique(rj_norm_rgb_dbscan_predict[rj_norm_rgb_dbscan_predict != 0]))
prop_ruido      <- mean(rj_norm_rgb_dbscan_predict == 0)

resultados <- list(
  tabela_clusters = tabela_clusters,
  n_clusters      = n_clusters,
  prop_ruido      = prop_ruido,
  cluster_means   = tabela_media_rgb_cluster,
  clusters_plot   = paste0(agrupamentos, ".png"),
  img_jpg         = img_jpg,
  img_png         = img_png
)

# Salvar objeto completo em .rds (para comparação programática posterior)
teste_completo <- list(
  id              = id_teste,
  parametros      = parametros,
  mensuracoes     = mensuracoes,
  resultados      = resultados,
  estatisticas    = list(
    completa = estatisticas_completa,
    amostra  = estatisticas_amostra
  )
)
?dbscan()
saveRDS(teste_completo, paste0(id_teste, "_resultado.rds"))

# Salvar relatório em texto plano .txt

sink(paste0(id_teste, "_relatorio.txt"))

cat("============================================================\n")
cat("TESTE DBSCAN - SEGMENTAÇÃO DE IMAGEM\n")
cat("============================================================\n")
cat("ID do teste:", id_teste, "\n")
cat("Imagem:", img, "\n")
cat("Data/hora:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("--- ESTATÍSTICAS: IMAGEM COMPLETA ---\n")
print(estatisticas_completa)
cat("\n")

cat("--- ESTATÍSTICAS: AMOSTRA ---\n")
print(estatisticas_amostra)
cat("\n")

cat("--- PARÂMETROS ---\n")
cat("Proporção da amostra:", parametros$prop_amostra, "\n")
cat("N pixels (amostra):", parametros$n_pixels_amostra, "\n")
cat("N pixels (total):", parametros$n_pixels_total, "\n")
cat("minPts:", parametros$minPts, "\n")
cat("'Elbow' para definição do epsilon visível em:", parametros$knndistplot,"\n")
cat("eps:", parametros$eps, "\n")
cat("seed:", parametros$seed, "\n\n")

cat("--- MENSURAÇÕES (tempo) ---\n")
cat("Tempo de treino (s):", round(mensuracoes$tempo_treino_seg, 3), "\n")
cat("Tempo de predição (s):", round(mensuracoes$tempo_predicao_seg, 3), "\n")
cat("Tempo total (s):", round(mensuracoes$tempo_total_seg, 3), "\n\n")

cat("--- RESULTADOS ---\n")
cat("Número de clusters encontrados:", resultados$n_clusters, "\n")
cat("Proporção de ruído (cluster 0):", round(resultados$prop_ruido, 4), "\n\n")

cat("Distribuição de pixels por cluster:\n")
print(resultados$tabela_clusters)
cat("\n")

cat("RGB médio por cluster:\n")
print(resultados$cluster_means)
cat("\n")

cat("Agrupamentos visíveis em:\n")
cat(resultados$clusters_plot)
cat("\n\n")

cat("Imagem resultante em .jpg e em .png:\n")
cat(resultados$img_jpg,"\n")
cat(resultados$img_png,"\n")
cat("\n")

sink()

# Salvar resumo em .csv (uma linha por teste, ideal para comparar muitos testes)

resumo_linha <- data.frame(
  id_teste            = id_teste,
  imagem              = img,
  timestamp           = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  prop_amostra        = parametros$prop_amostra,
  n_pixels_amostra    = parametros$n_pixels_amostra,
  n_pixels_total      = parametros$n_pixels_total,
  minPts              = parametros$minPts,
  eps                 = parametros$eps,
  tempo_treino_seg    = mensuracoes$tempo_treino_seg,
  tempo_predicao_seg  = mensuracoes$tempo_predicao_seg,
  tempo_total_seg     = mensuracoes$tempo_total_seg,
  n_clusters          = resultados$n_clusters,
  prop_ruido          = resultados$prop_ruido
)

arquivo_csv <- "comparativo_testes_dbscan.csv"

# Se o arquivo já existe, append (para acumular múltiplos testes)
if (file.exists(arquivo_csv)) {
  write.table(resumo_linha, arquivo_csv,
              sep = ",", row.names = FALSE, col.names = FALSE,
              append = TRUE)
} else {
  write.table(resumo_linha, arquivo_csv,
              sep = ",", row.names = FALSE, col.names = TRUE,
              append = FALSE)
}

cat("Arquivos salvos:\n")
cat("-", paste0(id_teste, "_resultado.rds"), "(objeto completo)\n")
cat("-", paste0(id_teste, "_relatorio.txt"), "(relatório legível)\n")
cat("-", arquivo_csv, "(linha adicionada ao comparativo)\n")

# TODO adicionar coluna descrevendo condição experimentada de cada teste

#1. Amostra mínima, minPts = dim + 1 e eps = eps elbow
#2. Amostra um pouco maior que a primeira, , minPts = dim + 1 e eps no elbow
#3. Amostra um pouco maior que a segunda, , minPts = dim + 1 e eps no elbow
#4. Amostra mínima, minPts = dim + 1 e eps = eps elbow + 1 
#5. Amostra mínima, minPts = dim + 1 e eps = eps elbow + 2
#6. Amostra mínima, minPts = dim + 2 e eps = eps elbow
#7. Amostra mínima, minPts = dim + 1 e eps = eps < eps elbow
#8. Amostra mínima, minPts = dim + 1 e eps = eps < eps elbow
#pensar para os próximos, não só apertar botão
#como interpretar os gráficos knndistplot e agrupamentos?

# PERGUNTAS
# Será que o modelo funciona para imagens similares?

# Importação das imagens
# Carregar a imagem
img <- "RJ_add02_img56"
rj_add02_img56 <- load.image(paste0(img, ".jpg"))

# Visualizar a imagem
plot(rj_add02_img56)

# Transformação das imagens para matrizes numéricas baseadas no RGB por pixel
rj_add02_img56_matriz <- as.data.frame(rj_add02_img56, wide = "c")

# colunas: x, y, c.1 (R), c.2 (G), c.3 (B)
names(rj_add02_img56_matriz)[3:5] <- c("R", "G", "B")

# Estrutura da matriz
str(rj_add02_img56_matriz)
# x = coordenadas x do pixel = integer
# y = coordenadas y do pixel = integer
# R, G, B = valor do pixel para cada canal

# Dimensões da matriz
dim(rj_add02_img56_matriz)

# Resumo estatístico
summary(rj_add02_img56_matriz)

# Predição com o dbscan
rj_add02_img56_dbscan_predict <- predict(rj_norm_rgb_dbscan_model, rj_add02_img56_matriz, rj_norm_rgb)

# Resultados
# Tabela dos clusters
table(rj_add02_img56_dbscan_predict)

# Visualização dos clusters
# 

# Anexar o cluster predito à matriz completa
rj_add02_img56_matriz$cluster <- rj_add02_img56_dbscan_predict

# Calcular RGB médio por cluster (excluindo ruído = cluster 0)
rj_add02_img56_media_rgb_cluster <- rj_add02_img56_matriz %>%
  filter(cluster != 0) %>%
  group_by(cluster) %>%
  summarise(
    R_mean = mean(R),
    G_mean = mean(G),
    B_mean = mean(B),
    .groups = "drop"
  )

# Substituir RGB de cada pixel pelo RGB médio do seu cluster
rj_add02_img56_dbscan_matriz <- rj_add02_img56_matriz %>%
  left_join(rj_add02_img56_media_rgb_cluster, by = "cluster") %>%
  mutate(
    R = ifelse(cluster == 0, R, R_mean),  # ruído mantém cor original
    G = ifelse(cluster == 0, G, G_mean),
    B = ifelse(cluster == 0, B, B_mean)
  ) %>%
  select(x, y, R, G, B)

# Garantir que os valores estão em [0, 1] (imager usa essa escala)
rj_add02_img56_dbscan_matriz <- rj_add02_img56_dbscan_matriz %>%
  mutate(across(c(R, G, B), ~ pmin(pmax(.x, 0), 1)))  # clamp, não rescale

# Reconstruir o objeto imagem do imager
# imager precisa do formato longer: x, y, cc (canal), value
rj_add02_img56_dbscan_matriz_longer <- rj_add02_img56_dbscan_matriz %>%
  pivot_longer(cols = c(R, G, B),
               names_to  = "canal",
               values_to = "value") %>%
  mutate(
    cc = case_when(
      canal == "R" ~ 1L,
      canal == "G" ~ 2L,
      canal == "B" ~ 3L
    ),
    z = 1L  # imagem estática, sem frames
  ) %>%
  select(x, y, z, cc, value) %>%
  arrange(cc, y, x)

# Converter matriz para objeto cimg
width  <- max(rj_add02_img56_matriz$x)
height <- max(rj_add02_img56_matriz$y)

rj_add02_img56_dbscan <- as.cimg(
  rj_add02_img56_dbscan_matriz_longer$value,
  x = width,
  y = height,
  z = 1,
  cc = 3
)

# Visualizar imagem resultante
plot(rj_add02_img56_dbscan, main = id_teste)

# Salvar imagem resultante em .jpg
save.image(rj_add02_img56_dbscan,"rj_add02_img56_dbscan.jpg")

# PERGUNTAS
# O resultado mostrou a influência do DBSCAN ter sido treinado com a localização dos pixels da imagem de treino
#a pensar, experimentar treinar só com valores RGB? Mas o kNNdisplot fica estranho