# Funcion px
p_x <- function(Edad, Tabla_mortalidad) {
qx <- Tabla_mortalidad[which(Tabla_mortalidad$x==Edad), "qx"]
    px <- 1-qx
    return(px)
}

t_p_x <- function(Edad, Tiempo, Tabla_mortalidad) {
    # Pre-asignar vector con longitud conocida
p_vec <- numeric(Tiempo-1)  # Vector de ceros
for(i in 1:n) {
    qx <- Tabla_mortalidad[which(Tabla_mortalidad$x== (Edad+i)), "qx"]
    p_vec[i] <- 1- qx
}
  t_q_x <- prod(p_vec)
    return(t_q_x)
}
