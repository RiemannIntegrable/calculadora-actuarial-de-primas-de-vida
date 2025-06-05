D <- function(tabla_mortalidad, x, i){

    v <- 1/(1+i) 
    lx <- tabla_mortalidad[tabla_mortalidad$x == x, "lx"]
    Dx <- (v**x) * lx
    
    return(Dx)
}