# Laboratorio 2 - Modelos Lineales
# Autor: Gonzalo Mardones Baeza

library(readr)
library(tidyverse)
library(ggplot2)
nidos <- read_delim("~/Desktop/MAGISTER/modelos_lineales/nidos.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

head(nidos)

label_altitud <- "Altitud del sector"
label_l_nums <- "Número promedio de nidos"

# Ejercicio 1 -------------------------------------------------------------
#Obtenga un grafico de dispersión del número promedio de nidos por árbol (escala logarítmica) en cada
# sector versus su altitud. Comente sobre la asociacion observada entre las variables

ggplot(nidos, aes(x=altitud, y=l_numero)) + 
  geom_point() + theme_light()


# Ejercicio 2 -------------------------------------------------------------
# Ajuste un modelo de regresion lineal simple a los nidos. Verifique que el modelo sea significativo,
# utilizando significancia 5 %. Agregue la recta ajustada a la figura y comente.

modelo = lm(l_numero ~ altitud,data = nidos)
summary(modelo)

ggplot(nidos, aes(x=l_numero, y=altitud)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  theme_light()

# Ejercicio 3 -------------------------------------------------------------
# Determine si existen observaciones anomalas u outliers. En caso de existir, identifique a que sectores 
# corresponden.

#############
## OUTLIERS ##
##############

t<-ls.diag(modelo)$stud.res

### Residuos studentizados

n = length(nidos$l_numero)
p = length(coef(modelo))

b = max(abs(t),
        qt(0.975, (n - p - 1))
)

plot(fitted(modelo),
     t,
     ylim = c(-b,b),
     xlab = "Valores ajustados",
     ylab="Residuos studentizados (t)",
     cex.lab =
       0.8,
     cex.axis = 0.8,
     pch = 19,
     cex = 0.5
     
)
abline(h = c(-qt(0.975, (n - p - 1)),
             qt(0.975,  (n - p - 1))
)
)

i = seq(1,n,1)
indices = i[abs(t)>=qt(0.975,(n - p - 1))]

## NOTA:  Observaciones 24 y 25 son identificadas como outliers

plot(nidos$altitud,
     nidos$l_numero,
     xlab = label_altitud,
     ylab = label_l_nums,
     cex.lab = 0.8,
     cex.axis = 0.8,
     pch = 19,
     cex = 0.5
)

points(nidos$altitud[indices],
       nidos$l_numero[indices],
       cex = 2.5,
       col = "red"
)

# Ejercicio 4 -------------------------------------------------------------
# Determine si existen observaciones influyentes. En caso de existir, identifique a que sectores corresponden.

# Ejercicio 1.R
### Palanca

hii = ls.diag(modelo)$hat
i[hii >= 2*p/n]

### 9,14,18,21

### Distancias de Cooks

Di = ls.diag(modelo)$cooks
i[Di >= 4/(n - p - 1)]

### 9,14,18,21

Dfit = ls.diag(modelo)$dfits
i[abs(Dfit) >= 2*sqrt(p/n)]

### 8,12

influyentes = c(8, 12)

plot(nidos$altitud,
     nidos$l_numero,
     xlab = label_altitud,
     ylab = label_l_nums,
     cex.lab = 0.8,
     cex.axis = 0.8,
     pch = 19,
     cex = 0.5
)

points(nidos$altitud[influyentes],
       nidos$l_numero[influyentes],
       cex = 2.5,
       col = "green"
)

text(nidos$altitud[8],
     nidos$l_numero[12],
     "8"
)


# Ejercicio 5 -------------------------------------------------------------
# Muestre en la figura las observaciones identificadas, diferenciando, con colores o sımbolos, aquellas
# que corresponden unicamente a outliers, aquellas que son unicamente influyentes, y aquellas que cumplen
# con ambas condiciones. No olvide incluir una leyenda que permita identificar a cual de los tres grupos
# corresponde cada observacion

plot(nidos$altitud,
     nidos$l_numero,
     xlab = label_altitud,
     ylab = label_l_nums,
     cex.lab = 0.8,
     cex.axis = 0.8,
     pch = 19,
     cex = 0.5
)

#OUTLIER
plot(nidos$altitud,
     nidos$l_numero,
     xlab = label_altitud,
     ylab = label_l_nums,
     cex.lab = 0.8,
     cex.axis = 0.8,
     pch = 19,
     cex = 0.5
)

points(nidos$altitud[indices],
       nidos$l_numero[indices],
       cex = 2.5,
       col = "red"
)

# INFLUYENTES
points(nidos$altitud[influyentes],
       nidos$l_numero[influyentes],
       cex = 2.5,
       col = "green"
)

text(nidos$altitud[8],
     nidos$l_numero[12],
     "8"
)

# Ejercicio 6 -------------------------------------------------------------
# Para cada observacion identificada, explique que caracterıstica en la figura sugiere la clasificacion que
# se le ha dado.



# Ejercicio 7 -------------------------------------------------------------
# ¿Sugeriría usted eliminar alguna(s) observaciones antes de ajustar el modelo? Justifique. Si lo considera
# adecuado, ajuste el modelo sin dichas observaciones y construya una figura que muestre las observaciones
# con ambas rectas ajustadas. No olvide agregar una leyenda. Comente.



# Ejercicio 8 -------------------------------------------------------------
# Compare los coeficientes ajustados con y sin las observaciones eliminadas. Comente.



# Ejercicio 9 -------------------------------------------------------------
# Interprete, en terminos del problema, el coeficiente ajustado segun el modelo que usted considere más
# adecuado



