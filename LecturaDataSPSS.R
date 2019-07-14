library(tidyverse)
library(datos)
library(gapminder)
library(Lahman)
library(nycflights13)
library(remotes)
library(foreign)


ReadSPSSData <- function() {
  ruta.de.archivo <- file.choose()
  dataset_GSS <- read.spss(ruta.de.archivo, to.data.frame = TRUE)
  tb_datos_GSS <- as_tibble(dataset_GSS)
  return(tb_datos_GSS)
}
