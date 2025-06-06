px <- function(x, tabla_mortalidad) {
    ### Validacion de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    
    omega <- max(tabla_mortalidad$x)
    if (x >= omega) {
        return(0)  # px = 0 porque qx = 1
    }
    
    qx <- tabla_mortalidad[tabla_mortalidad$x == x, "qx"]
    
    if (length(qx) == 0) {
        print(paste("Edad", x, "no encontrada en tabla"))
        return(NA)
    }

    ### Calculos ###
    px <- 1 - qx
    return(px)
}

tpx <- function(t, x, tabla_mortalidad) {
    ### Validacion de datos ###
    if (!is.numeric(t) || t %% 1 != 0 || t < 0) {
        print("t debe ser un entero no negativo")
        return(NA)
    }
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    
    omega <- max(tabla_mortalidad$x)
    if (x >= omega || x + t - 1 >= omega) {
        return(0)  # No puede sobrevivir más allá de omega
    }
    
    if (t == 0) {
        return(1)
    }
    
    ### Calculos ###
    rango <- x:(x + t - 1)
    tpx <- prod(sapply(rango, function(k) px(k, tabla_mortalidad)))
    return(tpx)
}

lx <- function(x, tabla_mortalidad, l0 = 100000) {
    ### Validacion de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    if (!is.numeric(l0) || l0 <= 0) {
        print("l0 debe ser un número positivo")
        return(NA)
    }
    
    omega <- max(tabla_mortalidad$x)
    if (x > omega) {
        return(0)  # No hay sobrevivientes después de omega
    }
    
    if (x == 0) {
        return(l0)
    }
    
    ### Calculos ###
    prob_supervivencia <- tpx(x, 0, tabla_mortalidad)
    if (is.na(prob_supervivencia)) return(NA)
    lx <- l0 * prob_supervivencia
    return(lx)
}

ndx <- function(n, x, tabla_mortalidad, l0 = 100000) {
    ### Validacion de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.numeric(n) || n %% 1 != 0 || n < 0) {
        print("n debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    
    omega <- max(tabla_mortalidad$x)
    if (x > omega) {
        return(0)  # No hay muertes después de omega
    }

    ### Calculos ###
    ndx <- lx(x, tabla_mortalidad, l0) - lx(x + n, tabla_mortalidad, l0)
    return(ndx)
}

S0 <- function(t, tabla_mortalidad) {
    ### Validacion de datos ###
    if (!is.numeric(t) || t %% 1 != 0 || t < 0) {
        print("t debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    
    omega <- max(tabla_mortalidad$x)
    if (t > omega) {
        return(0)  # Función de supervivencia es 0 después de omega
    }

    ### Calculos ###
    S0t <- lx(t, tabla_mortalidad) / lx(0, tabla_mortalidad)
    return(S0t)
}

f0 <- function(t, tabla_mortalidad) {
    ### Validacion de datos ###
    if (!is.numeric(t) || t %% 1 != 0 || t < 0) {
        print("t debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    
    omega <- max(tabla_mortalidad$x)
    if (t >= omega) {
        return(0)  # Densidad es 0 en omega y después
    }

    ### Calculos ###
    f0t <- S0(t, tabla_mortalidad) - S0(t + 1, tabla_mortalidad)
    return(f0t)
}

mu <- function(x, tabla_mortalidad) {
    ### Validacion de datos ###
    if (!is.numeric(x) || x %% 1 != 0 || x < 0) {
        print("x debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    
    omega <- max(tabla_mortalidad$x)
    if (x >= omega) {
        return(Inf)  # Fuerza de mortalidad infinita en omega
    }
    
    ### Calculos ###
    S0_x <- S0(x, tabla_mortalidad)
    if (S0_x == 0) {
        return(Inf)  # División por cero implica mortalidad infinita
    }
    
    mux <- f0(x, tabla_mortalidad) / S0_x
    return(mux)
}