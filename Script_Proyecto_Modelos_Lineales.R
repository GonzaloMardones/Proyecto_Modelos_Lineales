library(readxl)
library(tidyverse)


# Lectura y cambio de estructura de la BBDD

BBDD_proyecto <- read_excel("BBDD_FINAL_Mod_Lin.xlsx")
#View(BBDD_proyecto)
names(BBDD_proyecto)
glimpse(BBDD_proyecto)

BBDD_proyecto <- BBDD_proyecto %>% mutate_at(c("SEXO","ESTADO_CIVIL",
                              "SISTEMA_SALUD","NIVEL",
                              "TIPO_ESTABLECIMIENTO",
                              "CALIDAD_DESEMPEÑO",
                              "COMUNA_ESTABLECIMIENTO",
                              "ESTAMENTO","TRAMO_DOCENTE"),factor)

BBDD_proyecto$EVALUACION_DOCENTE <- factor(BBDD_proyecto$EVALUACION_DOCENTE,
                                           levels = c("SIN EVALUACIÓN",
                                                      "DESTACADO",
                                                      "BASICO",
                                                      "INSATISFACTORIO",
                                                      "COMPETENTE"))

levels(BBDD_proyecto$EVALUACION_DOCENTE)
contrasts(BBDD_proyecto$EVALUACION_DOCENTE)

BBDD_proyecto_sin_ceros <- BBDD_proyecto %>% filter(PROMEDIO_DIAS_LM != 0)


unique(BBDD_proyecto_sin_ceros$EVALUACION_DOCENTE)
levels(BBDD_proyecto_sin_ceros$EVALUACION_DOCENTE)
mod <- lm(PROMEDIO_DIAS_LM ~ EVALUACION_DOCENTE, data = BBDD_proyecto_sin_ceros)
summary(mod)



ggplot(BBDD_proyecto_sin_ceros, aes(x = PROMEDIO_DIAS_LM)) +
  geom_histogram(aes(y =..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.6 , fill = "#FF6666", lwd = 1,colour = 2)+
  geom_vline(aes(xintercept = mean(PROMEDIO_DIAS_LM)),
             color = "blue", linetype = "dashed", size = 1)+
  theme_minimal()

ggplot(BBDD_proyecto_sin_ceros, aes(x = PROMEDIO_DIAS_LM))+
  stat_boxplot(geom = "errorbar",width = 0.15) +
  geom_boxplot(fill="#FF6666")+
  theme_minimal()

summary(BBDD_proyecto_sin_ceros)
boxplot(BBDD_proyecto_sin_ceros$PROMEDIO_DIAS_LM)
hist(BBDD_proyecto_sin_ceros$PROMEDIO_DIAS_LM,freq = FALSE)

# ggplot(BBDD_proyecto_sin_ceros, aes(sample = PROMEDIO_DIAS_LM)) + 
#   stat_qq() + 
#   stat_qq_line()

modelo1 <- lm(PROMEDIO_DIAS_LM ~ SEXO + ESTADO_CIVIL + EDAD + RENTA_PROMEDIO +
     SISTEMA_SALUD + JORNADA + NIVEL + CALIDAD_DESEMPEÑO + 
     ESTAMENTO + TRAMO_DOCENTE + EVALUACION_DOCENTE + TRASLADO_COMUNA, 
   data = BBDD_proyecto_sin_ceros)

summary(modelo1)
library(MASS)
empty.model <- lm(PROMEDIO_DIAS_LM ~ 1, data = BBDD_proyecto_sin_ceros)

modform <- stepAIC(empty.model, direction = "forward", trace = FALSE, scope = modelo1)

summary(modform)

## forward

modelo0 = lm(PROMEDIO_DIAS_LM ~ 1, data = BBDD_proyecto_sin_ceros)
add1(modelo0, ~ .  + SEXO + ESTADO_CIVIL + EDAD + RENTA_PROMEDIO +
       SISTEMA_SALUD + JORNADA + NIVEL + CALIDAD_DESEMPEÑO +
       ESTAMENTO + EVALUACION_DOCENTE + TRAMO_DOCENTE + TRASLADO_COMUNA,
     test="F") # El menor p-valor(F)= < 2.2e-16 *** es STATE


modelo0 = lm(PROMEDIO_DIAS_LM ~ EVALUACION_DOCENTE, data = BBDD_proyecto_sin_ceros)
add1(modelo0, ~ .  + SEXO + ESTADO_CIVIL + EDAD + RENTA_PROMEDIO +
       SISTEMA_SALUD + JORNADA + NIVEL + CALIDAD_DESEMPEÑO +
       ESTAMENTO + EVALUACION_DOCENTE + TRAMO_DOCENTE + TRASLADO_COMUNA,
     test="F") # El menor p-valor(F)= < 2.2e-16 *** es STATE


modelo0 = lm(PROMEDIO_DIAS_LM ~ EVALUACION_DOCENTE + CALIDAD_DESEMPEÑO, data = BBDD_proyecto_sin_ceros)
add1(modelo0, ~ .  + SEXO + ESTADO_CIVIL + EDAD + RENTA_PROMEDIO +
       SISTEMA_SALUD + JORNADA + NIVEL + CALIDAD_DESEMPEÑO +
       ESTAMENTO + EVALUACION_DOCENTE + TRAMO_DOCENTE + TRASLADO_COMUNA,
     test="F") # El menor p-valor(F)= < 2.2e-16 *** es STATE


modelo0 = lm(PROMEDIO_DIAS_LM ~ EVALUACION_DOCENTE + CALIDAD_DESEMPEÑO +NIVEL, data = BBDD_proyecto_sin_ceros)
add1(modelo0, ~ .  + SEXO + ESTADO_CIVIL + EDAD + RENTA_PROMEDIO +
       SISTEMA_SALUD + JORNADA + NIVEL + CALIDAD_DESEMPEÑO +
       ESTAMENTO + EVALUACION_DOCENTE + TRAMO_DOCENTE + TRASLADO_COMUNA,
     test="F") # El menor p-valor(F)= < 2.2e-16 *** es STATE


modelo0 = lm(PROMEDIO_DIAS_LM ~ EVALUACION_DOCENTE + CALIDAD_DESEMPEÑO +NIVEL + EDAD, data = BBDD_proyecto_sin_ceros)
add1(modelo0, ~ .  + SEXO + ESTADO_CIVIL + EDAD + RENTA_PROMEDIO +
       SISTEMA_SALUD + JORNADA + NIVEL + CALIDAD_DESEMPEÑO +
       ESTAMENTO + EVALUACION_DOCENTE + TRAMO_DOCENTE + TRASLADO_COMUNA,
     test="F") # El menor p-valor(F)= < 2.2e-16 *** es STATE


modelo0 = lm(PROMEDIO_DIAS_LM ~ EVALUACION_DOCENTE + CALIDAD_DESEMPEÑO +NIVEL + EDAD + TRAMO_DOCENTE, data = BBDD_proyecto_sin_ceros)
add1(modelo0, ~ .  + SEXO + ESTADO_CIVIL + EDAD + RENTA_PROMEDIO +
       SISTEMA_SALUD + JORNADA + NIVEL + CALIDAD_DESEMPEÑO +
       ESTAMENTO + EVALUACION_DOCENTE + TRAMO_DOCENTE + TRASLADO_COMUNA,
     test="F") # El menor p-valor(F)= < 2.2e-16 *** es STATE

summary(modelo0)
contrasts(BBDD_proyecto_sin_ceros$EVALUACION_DOCENTE)
