library(imager)
library(dplyr)
library(ggplot2)
library(dbscan)

# Carregar as imagens
rj_add02_img86 <- load.image("RJ_add02_img86.jpg")
guaru02_img02 <- load.image("GUARU02_img17.jpg")
flor10_img02 <- load.image("FLOR10_img02.jpg")

# Visualizar as imagens
layout(t(1:3))
plot(rj_add02_img86)
plot(guaru02_img02)
plot(flor10_img02)

# Visualizar na escala de cinza
rj_add02_img86_cinza <- grayscale(rj_add02_img86)
guaru02_img02_cinza <- grayscale(guaru02_img02)
flor10_img02_cinza <- grayscale(flor10_img02)

layout(t(1:3))
plot(rj_add02_img86_cinza)
plot(guaru02_img02_cinza)
plot(flor10_img02_cinza)

# RJ_add02_img86

# Características básicas da imagem
class(rj_add02_img86) 
str(rj_add02_img86)
dim(rj_add02_img86) 
sum(rj_add02_img86)
range(rj_add02_img86) 
summary(rj_add02_img86)

# Decomposição R, G, B
rj_add02_img86_red <- R(rj_add02_img86)
rj_add02_img86_green <- G(rj_add02_img86)
rj_add02_img86_blue <- B(rj_add02_img86)

# Plotar histogramas da imagem inteira, escala de cinza e camadas R, G, B
layout(t(1:5))
rj_add02_img86 %>% hist(main="RJ_add02_img86")
rj_add02_img86_cinza %>% hist(main="Escala de cinza")
rj_add02_img86_red %>% hist(main="R")
rj_add02_img86_green %>% hist(main="G")
rj_add02_img86_blue %>% hist(main="B")
# Não ficou bom

layout(t(1:1))

# Reflectâncias para a imagem inteira
rj_add02_img86_reflec2_int <- rj_add02_img86 > 0.2
plot(rj_add02_img86_reflec_int)

rj_add02_img86_reflec3_int <- rj_add02_img86 > 0.3
plot(rj_add02_img86_reflec_int)

rj_add02_img86_reflec4_int <- rj_add02_img86 > 0.4
plot(rj_add02_img86_reflec_int)

rj_add02_img86_reflec5_int <- rj_add02_img86 > 0.5
plot(rj_add02_img86_reflec_int)

# Reflectância para escala de cinza
rj_add02_img86_reflec2_cinza <- rj_add02_img86_cinza > 0.2
plot(rj_add02_img86_reflec_cinza)

rj_add02_img86_reflec3_cinza <- rj_add02_img86_cinza > 0.3
plot(rj_add02_img86_reflec_cinza)

rj_add02_img86_reflec4_cinza <- rj_add02_img86_cinza > 0.4
plot(rj_add02_img86_reflec_cinza)

rj_add02_img86_reflec5_cinza <- rj_add02_img86_cinza > 0.5
plot(rj_add02_img86_reflec_cinza)

# Reflectância com o terceiro quartil de cada canal como threshold
rj_add02_img86_reflec_red <- rj_add02_img86_red > quantile(rj_add02_img86_red, 0.99)
plot(rj_add02_img86_reflec_red)
rj_add02_img86_reflec_green <- rj_add02_img86_green > quantile(rj_add02_img86_green, 0.99)
plot(rj_add02_img86_reflec_green)
rj_add02_img86_reflec_blue <- rj_add02_img86_blue > quantile(rj_add02_img86_blue, 0.99)
plot(rj_add02_img86_reflec_blue)

# cimg com pixset servindo de máscara (pixels não selecionados viram 0)
rj_add02_img86_mask4 <- rj_add02_img86 * rj_add02_img86_reflec4_int
rj_add02_img86_mask4
plot(rj_add02_img86_mask4)

# Filtrar apenas pixels com valor maior que zero
rj_add02_img86_matriz <- as.data.frame(rj_add02_img86, wide = "c")
rj_add02_img86_mask4_matriz <- as.data.frame(rj_add02_img86_mask4, wide = "c")
rj_add02_img86_mask4_matriz <- rj_add02_img86_mask4_matriz %>% filter(c.1 > 0, c.2 > 0, c.3 > 0)

# DBSCAN
# Predefinir parâmetros
# minPts = dim + 1
dim(rj_add02_img86_mask4_matriz)
# [1] 14365     5
# 5 + 1 = 6 como minPts

# Checar o minPts com o k-NN
kNNdistplot(rj_add02_img86_mask4_matriz, minPts=6)
# Resultado estranho

# Treinar o dbscan com rj_add02_img86_mask4_matriz
rj_add02_img86_mask4_dbscan <- dbscan(rj_add02_img86_mask4_matriz, eps=100, minPts=6)

# Predizer com o dbscan com a matriz original
rj_add02_img86_dbscan <- predict(clust_object, rj_add02_img86_matriz, rj_add02_img86_mask4_matriz)

plot(rj_add02_img86_dbscan)
