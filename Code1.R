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

# Ejemplo datos simulados con diferentes errores ----
dt_lp_error <- expand_grid(
  n = 200,
  a = 0.0001,
  b = 2.9,
  error = seq(0.01,1,0.1),
  Talla = c(10, 120)) %>% 
  group_by(n, a, b, error) %>% 
  nest(.key = 'Tallas') %>% 
  mutate(
    datos_lp = purrr::map(Tallas, ~mod_lp(longitud = .$Talla, error = error, 
                                          n = n, a = a, b = b))
  )
