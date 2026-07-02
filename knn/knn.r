# TODO
# Cabeçalho
# Refatorar para casos original, DBSCAN, com e sem blurring

# Fonte: https://asgr.github.io/imager/foreground_background.html#gradient-based-algorithm
# The first approach is similar to the SIOX algorithm implemented in the Gimp. 
# It assumes that foreground and background have different colours, and models 
# the segmentation task as a (supervised) classification problem, where the user 
# has provided examples of foreground pixels, examples of background pixels, and 
# we need to classify the rest of the pixels according to colour. Since we do not 
# have a parametric model in mind, a simple and robust approach is to use 
# k-nearest neighbour classification.

# There are many implementations of the kNN algorithm available for R but the fastest 
# one I’ve found is in the nabor package. The following function implements (binary) 
# knn classification:

# Carregar pacotes
library(imager)

# Definir diretório de trabalho, se necessário
getwd()
setwd("/media/ramaleticia/3202be5f-767f-4a29-8b6f-7301fe013db6/mca/aprendizado_maquina_COMPLETAR_REPOSITORIO_GITHUB_SALVAR_MATERIAIS_AULA_ANITA/vers/atividade_avaliativa/dbscan_knn_satellite_ship_rgb_images/knn")

## X = dados de treino
## Xp = dados de teste
## cl = rótulos das linhas
## k = número de vizinhos mais próximos
## Retorna o valor média dos k vizinhos mais próximos
fknn <- function(X, Xp, cl, k = 1)
{
    out <- nabor::knn(X, Xp, k = k)
    cl[as.vector(out$nn.idx)] %>% matrix(dim(out$nn.idx)) %>% rowMeans
}

# Carregar imagens
# Imagem original
id_img <- "RJ_add02_img86"
img <- load.image(paste0(id_img, ".jpg"))
plot(img)

# Melhor resultado do dbscan até o momento
# img <- load.image("20260623_RJ_add02_img86_prop0.1_eps5.5_minPts6.jpg")
# plot(img)

# Imagem com denoising
img <- isoblur(img, 5)
plot(img)

# Rodar kNN para imagem original e imagem dbscan

# Criar dados de treinamento 

# Amostrar regiões de background (não barco) e foreground (barco)
dim(img)
area_img <- nrow(img) * ncol(img)  # 3951002 px²
prop <- 0.0001 # amostras da imagem de 0.01%
amostra <- sqrt(area_img * prop) # amostras terão área de 62.85x62.85 pixels

grabPoint(img)
# Coordenadas x, y inicializam o quadrilátero no canto superior esquerdo
# Coordenada do canto inferior direito = coordenada canto superior esquerdo + amostra
# para se obter a área amostrada
bg <- c(3786, 2228, 3786 + amostra, 2228 + amostra) 
fg <- c(3613, 2336, 3613 + amostra, 2336 + amostra) 
 
# Filtrar pixels conforme as coordenadas de background e foreground
# Xc() e Yc() retornam coordenadas dos pixels de uma imagem como pixel sets de valores booleanos
# Par c(1,3) são (x0, x1) e par c(2,4) são c(y0, y1)
px.bg <- ((Xc(img) %inr% bg[c(1,3)]) & (Yc(img) %inr% bg[c(2,4)])) # pixel set background
plot(px.bg)
px.fg <- ((Xc(img) %inr% fg[c(1,3)]) & (Yc(img) %inr% fg[c(2,4)])) # pixel set foreground
plot(px.fg)

# Verificar regiões de background e foreground na imagem 
plot(img)
title(
    main = paste0("Dados de treinamento do kNN: amostras de ", id_img, " processada com DBSCAN"),
    sub = paste0("Tamanho da amostra: ", round(amostra, 2), " x ", round(amostra, 2), " pixels (", prop * 100, "% da imagem).\n",
    "Caixa azul: superfície do mar (background) - ", "Caixa vermelha: barco (foreground)"),
)
highlight(px.bg, col="blue")
highlight(px.fg, col="red")

# Dados são trios de valores R, G, B e são convertidos para o espaço de cores CIELAB
im.lab <- sRGBtoLab(img)

# Dados da imagem são redimensionados para matriz com 3 colunas
# Função para redimensionar
cvt.mat <- function(px) matrix(im.lab[px], sum(px)/3, 3)

# Aplicar a função no pixels sets do background e foreground
mat.bg <- cvt.mat(px.bg)
mat.fg <- cvt.mat(px.fg)

# Criar rótulos 
# 0 = foreground
# 1 = background
labels <- c(rep(1,nrow(mat.bg)), rep(0, nrow(mat.fg)))

# Dados de teste
# Toda a matriz
test.mat <- cvt.mat(px.all(img))

# Treinar fkNN 

# A saída é a proporção de pixels do foreground pixels entre k vizinhos mais próximos
# funciona como uma medida de confiança

# # Teste com 6-nn
# k <- 6
# knn6 <- fknn(rbind(mat.fg, mat.bg), test.mat, cl = labels, k = k)
# mask6 <- as.cimg(rep(knn6, 3), dim = dim(img))
# plot(mask6)
# title(main = paste0("Segmentação binária de ", k, "-NN para ", id_img, " processada com DBSCAN"))

# Teste com 6-nn com denoising original
k <- 6
knn6 <- fknn(rbind(mat.fg, mat.bg), test.mat, cl = labels, k = k)
mask6 <- as.cimg(rep(knn6, 3), dim = dim(img))
plot(mask6)
id_img <- "RJ_add02_img86"
title(main = paste0("Segmentação binária de ", k, "-NN para ", id_img, " original e com aplicação de denoising = 5"))

# Teste com 6-nn com denoising DBSCAN
# k <- 6
# knn6 <- fknn(rbind(mat.fg, mat.bg), test.mat, cl = labels, k = k)
# mask6 <- as.cimg(rep(knn6, 3), dim = dim(img))
# plot(mask6)
# id_img <- "RJ_add02_img86"
# title(main = paste0("Segmentação binária de ", k, "-NN para ", id_img, " processada com DBSCAN e com aplicação de denoising = 5"))

# # Teste com 5-nn
# k <- 5
# knn5 <- fknn(rbind(mat.fg, mat.bg), test.mat, cl = labels, k = k)
# mask5 <- as.cimg(rep(knn5, 3), dim = dim(img))
# plot(mask5)
# title(main = paste0("Segmentação binária de ", k, "-NN para ", id_img, " processada com DBSCAN"))

# # Teste com 4-nn
# k <- 4
# knn4 <- fknn(rbind(mat.fg, mat.bg), test.mat, cl = labels, k = k)
# mask4 <- as.cimg(rep(knn4, 3), dim = dim(img))
# plot(mask4)
# title(main = paste0("Segmentação binária de ", k, "-NN para ", id_img, " processada com DBSCAN"))

# # Teste com 3-nn
# k <- 3
# knn3 <- fknn(rbind(mat.fg, mat.bg), test.mat, cl = labels, k = k)
# mask3 <- as.cimg(rep(knn3, 3), dim = dim(img))
# plot(mask3)
# title(main = paste0("Segmentação binária de ", k, "-NN para ", id_img, " processada com DBSCAN"))

# # Teste com 2-nn
# k <- 2
# knn2 <- fknn(rbind(mat.fg, mat.bg), test.mat, cl = labels, k = k)
# mask2 <- as.cimg(rep(knn2, 3), dim = dim(img))
# plot(mask2)
# title(main = paste0("Segmentação binária de ", k, "-NN para ", id_img, " processada com DBSCAN"))

# TODO
# Entender melhor o código

# PERGUNTAS
# Serve de feature extraction para outros algoritmos de aprendizado supervisionado?
# Como avaliar o modelo? Pela área de cada rótulo na imagem original e na imagem segmentada?
 
