# Librerias ----
pacman::p_load(tidyverse, purrr)

# Simulación modelo LP ----
mod_lp <- function(longitud, n, a, b, error, semilla){
  dt <- tibble::tibble(
    Talla = sample(seq(longitud[1],longitud[2],1), size = n, replace = TRUE),
    Peso_medio = a*Talla^b,
    error = rnorm(n = n, sd = error),
    Peso = Peso_medio*exp(error)
  )
  return(dt)
}