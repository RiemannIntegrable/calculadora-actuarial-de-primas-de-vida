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

    x_min <- min(tabla_mortalidad$x)
    omega <- max(tabla_mortalidad$x)
    if (x < x_min) {
        return(NA)
    }
    if (x >= omega) {
        return(0)  # px = 0 porque qx = 1
    }

    ### Calculos ###
    qx <- tabla_mortalidad[tabla_mortalidad$x == x, "qx"]
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
    
    x_min <- min(tabla_mortalidad$x)
    omega <- max(tabla_mortalidad$x)
    if (x < x_min) {
        return(NA)
    }
    if (x >= omega || x + t - 1 >= omega) {
        return(0)
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
    ### Validación de datos ###
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
    
    # Obtener rango de edades de la tabla
    x_min <- min(tabla_mortalidad$x)
    omega <- max(tabla_mortalidad$x)
    
    # Casos especiales
    if (x < x_min) {
        return(NA)  # No hay datos antes del inicio de la tabla
    }
    if (x > omega) {
        return(0)  # No hay sobrevivientes después de omega
    }
    if (x == x_min) {
        return(l0)  # Base en la primera edad disponible
    }
    
    ### Cálculos ###
    # Calcular supervivencia desde x_min hasta x
    prob_supervivencia <- tpx(x - x_min, x_min, tabla_mortalidad)
    if (is.na(prob_supervivencia)) return(NA)
    
    lx_resultado <- l0 * prob_supervivencia
    return(lx_resultado)
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
    
    x_min <- min(tabla_mortalidad$x)
    omega <- max(tabla_mortalidad$x)
    if (x > omega) {
        return(0)
    }
    if (x < x_min) {
        return(NA)
    }

    ### Calculos ###
    ndx <- lx(x, tabla_mortalidad, l0) - lx(x + n, tabla_mortalidad, l0)
    return(ndx)
}

S <- function(t, tabla_mortalidad) {
    ### Validación de datos ###
    if (!is.numeric(t) || t %% 1 != 0 || t < 0) {
        print("t debe ser un entero no negativo")
        return(NA)
    }
    if (!is.data.frame(tabla_mortalidad)) {
        print("tabla_mortalidad debe ser un data frame")
        return(NA)
    }
    
    # Obtener rango de edades
    x_min <- min(tabla_mortalidad$x)
    omega <- max(tabla_mortalidad$x)

    if (t > omega) {
        return(0)
    }

    ### Cálculos ###
    St <- tpx(t, x_min, tabla_mortalidad)
    return(S0t)
}

f <- function(t, tabla_mortalidad) {
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
        return(0)
    }

    ### Calculos ###
    ft <- S(t, tabla_mortalidad) - S(t + 1, tabla_mortalidad) # \Delta = 1
    return(ft)
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
    
    x_min <- min(tabla_mortalidad$x)
    omega <- max(tabla_mortalidad$x)
    if (x >= omega) {
        return(Inf)  # Fuerza de mortalidad infinita en omega
    }
    if (x < x_min) {
        return(NA)  # No hay datos antes del inicio de la tabla
    }
    
    ### Calculos ###
    Sx <- S(x, tabla_mortalidad)
    if (Sx == 0) {
        return(Inf)  # División por cero implica mortalidad infinita
    }
    
    mux <- f(x, tabla_mortalidad) / Sx
    return(mux)
}