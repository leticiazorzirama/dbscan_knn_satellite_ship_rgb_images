# TODO
# Cabeçalho

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
#setwd()

## X = dados de treino
## Xp = dados de teste
## cl = rótulos das linhas
## k = número de vizinhos mais próximos
## Retorna o valor média dos k vizinhos mais próximos
fknn <- function(X, Xp, cl, k = 1)
{
    out <- nabor::knn(X, Xp, k=k)
    cl[as.vector(out$nn.idx)] %>% matrix(dim(out$nn.idx)) %>% rowMeans
}

# Carregar imagens
# Imagem original
# img <- load.image("knn/RJ_add02_img86.jpg")
# plot(img)

# Melhor resultado do dbscan até o momento
# img <- load.image("knn/20260623_RJ_add02_img86_prop0.1_eps5.5_minPts6.jpg")
# plot(img)

# Rodar kNN para imagem original e imagem dbscan

# Criar dados de treinamento 

# Amostrar regiões de background (não barco) e foreground (barco)

# Retângulo com coordenadas x0, y0, x1, y1
# grabRect(img) permite desenhar um quadrilátero na imagem e obter tais coordenadas
grabRect(img)
bg <- c(332, 358, 613, 463) # coordenadas background
fg <- c(3610, 2354, 3716, 2423 ) # coordenadas foreground

# Filtrar pixels conforme as coordenadas de background e foreground
# Xc() e Yc() retornam coordenadas dos pixels de uma imagem como pixel sets

# Par c(1,3) são (x0, x1) e par c(2,4) são c(y0, y1)
px.bg <- ((Xc(img) %inr% bg[c(1,3)]) & (Yc(img) %inr% bg[c(2,4)])) # pixel set background
plot(px.bg)
px.fg <- ((Xc(img) %inr% fg[c(1,3)]) & (Yc(img) %inr% fg[c(2,4)])) # pixel set foreground
plot(px.fg)

# Verificar regiões de background e foreground na imagem 
plot(img)
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

# A saída é a proporção de pixels do foreground pixels entre k-nearest neighbours
# funciona como uma medida de confiança

# Teste com 6-nn
knn.6 <- fknn(rbind(mat.fg, mat.bg), test.mat, cl = labels, k = 6)
mask.6 <- as.cimg(rep(knn.6, 3), dim = dim(img))
plot(mask.6)

# Teste com 5-nn
knn.5 <- fknn(rbind(mat.fg, mat.bg), test.mat, cl = labels, k = 5)
mask.5 <- as.cimg(rep(knn.5, 3), dim = dim(img))
plot(mask.5)

# Teste com 4-nn
knn.4 <- fknn(rbind(mat.fg, mat.bg), test.mat, cl = labels, k = 4)
mask.4 <- as.cimg(rep(knn.4, 3), dim = dim(img))
plot(mask.4)

# Teste com 3-nn
knn.3 <- fknn(rbind(mat.fg, mat.bg), test.mat, cl = labels, k = 3)
mask.3 <- as.cimg(rep(knn.3, 3), dim = dim(img))
plot(mask.3)

# Teste com 2-nn
knn.2 <- fknn(rbind(mat.fg, mat.bg), test.mat, cl = labels, k = 2)
mask.2 <- as.cimg(rep(knn.2, 3), dim = dim(img))
plot(mask.2)

# TODO
# Entender melhor o código

# PERGUNTAS
# Serve de feature extraction para outros algoritmos de aprendizado supervisionado?
# Como avaliar o modelo? Pela área de cada rótulo na imagem original e na imagem segmentada?
 
