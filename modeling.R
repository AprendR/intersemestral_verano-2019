
# Modeling ----------------------------------------------------------------
#+ setup, message = FALSE, warning = FALSE, error = FALSE


# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(foreign)
library(jtools)


# Descargar datos ---------------------------------------------------------

datos <- read.spss("Datos/GSS2018.sav", 
                   to.data.frame = TRUE,
                   max.value.labels = 5) 


# tbl_df ------------------------------------------------------------------

datos <- tbl_df(datos)



# Modelo lineal -----------------------------------------------------------

(datos_lm <- datos %>% 
   select(edad = AGE, 
          sexo = SEX, 
          peso = WEIGHT,
          estatura = HEIGHT,
          tv = TVHOURS) %>% 
   mutate(peso_kg = peso * 0.453592,
          estatura_m = estatura * 0.0254,
          imc = peso_kg / estatura_m ^ 2) %>% 
   select(-(peso:estatura))) %>% 
  filter(!is.na(peso_kg), !is.na(tv))


# Ajusta el modelo --------------------------------------------------------

fit <- lm(peso_kg ~ tv, data = datos_lm)


# Resume ------------------------------------------------------------------

summ(fit, confint = T)


# Visualiza ---------------------------------------------------------------

effect_plot(fit, pred = tv, plot.points = T, interval = T)



# Modelo lineal generalizado ----------------------------------------------

(datos_glm <- datos %>% 
   select(edad = AGE, sexo = SEX, infiel = EVSTRAY) %>% 
   filter(!is.na(sexo), 
          !is.na(infiel),
          infiel %in% c("YES", "NO")) %>%
   mutate(infiel = fct_drop(infiel)))



# Ajusta el modelo --------------------------------------------------------

fit2 <- glm(infiel == "YES" ~ sexo, 
            data = datos_glm, 
            family = binomial("logit"))


# Resume ------------------------------------------------------------------

summ(fit2, confint = T)


# Visualiza ---------------------------------------------------------------

effect_plot(fit2, 
            pred = sexo, 
            interval = T, 
            data = datos_glm)





