

# carga el paquetes
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
install.packages("Rcpp")
install.packages("dplyr")
install.packages("xlsx", dep = T)   
install.packages("JavaGD")
install.packages("rJava")
library(sets)
library(MASS)
library(readxl)
library(dplyr)
library(knitr)
library(kableExtra)
library(magrittr)
library(neuralnet)

# ----------------------  Levantar DataFrame  ----------------------
# Levantamos el archivo que nos brindara informacion
encuestas <- read_excel("se.xls")
encuestas$X <- NULL
str(encuestas)
summary(encuestas)
kable(head(encuestas))

# Guardamos el max y min salario neto por hora
maxSal = max(encuestas$SalarioNetoPorHora)
minSal = min(encuestas$SalarioNetoPorHora)

# Output
Output <- encuestas$SalarioNetoPorHora

# Output
Output <- encuestas$SalarioNetoPorHora
encuestas$SalarioNetoPorHora <- NULL

set.seed(5)
indices <- sample(1:nrow(encuestas),size=round(0.3*nrow(encuestas)))

entrenamiento_input <- encuestas[-indices,]
entrenamiento_output = Output[-indices]
entrenamiento <- entrenamiento_input
entrenamiento$Output <- entrenamiento_output

test_input <- encuestas[indices,]
test_output <- Output[indices]

# Creación y entrenamiento de la red neuronal
# create model

CambioPorMejorSalario  <- c(rep(0, 7), 1)
CambioPorMejorAmbiente <- c(0, rep(1, 7))
#binary_data <- data.frame(expand.grid(c(0, 1), c(0, 1), c(0, 1)), CambioPorMejorSalario, CambioPorMejorAmbiente)
binary_data <- data.frame(expand.grid(c(0, 1), c(0, 1), c(0, 1)), CambioPorMejorSalario, CambioPorMejorAmbiente)
mod <- neuralnet(CambioPorMejorSalario+CambioPorMejorAmbiente ~ Var1 + Var2 + Var3, binary_data,
              hidden = c(5, 4, 2), rep = 2, linear.output = TRUE)
# plotnet
par(mar = numeric(4), family = 'serif')
plot(mod)




# Configurar el universo para trabajar inicialmente un sistema difuso
sets_options("universe", seq(1, 100, 0.1))
# Definimos las variables del sistema difuso
variables <- set(
  presionLaboral = fuzzy_partition(varnames = c(excesoPresion = 20, moderadaPresion = 55,noPresion=80),sd = 9.0),
  desarrolloLaboral = fuzzy_partition(varnames = c(estancamientoProfesional = 20, crecimientoProfesionalIndividual = 55, 
                                                 CrecimientoProfesionalOrganizacional=80),sd = 8.25),
  nivelRemunerativo = fuzzy_partition(varnames = c(bajaRemuneracion= 25, remuneracionPromedio = 50, buenaRemuneracion = 100), 
                                                 sd = 9.5),
  climaOrganizacional = fuzzy_partition(varnames = c(malo = 20, aceptable = 55, Excelente = 80),FUN = fuzzy_cone, radius = 25)
)
# Definimos las reglas difusas del sistema
reglas <- set(
  fuzzy_rule(presionLaboral %is% noPresion && desarrolloLaboral %is% CrecimientoProfesionalOrganizacional && 
            nivelRemunerativo %is% buenaRemuneracion, climaOrganizacional %is% Excelente),
  fuzzy_rule(presionLaboral %is% excesoPresion && desarrolloLaboral %is% estancamientoLaboral 
            && nivelRemunerativo %is% bajaRemuneracion, climaOrganizacional %is% malo),
  fuzzy_rule(presionLaboral %is% excesoPresion, climaOrganizacional %is% malo),
  fuzzy_rule(presionLaboral %is% aceptablePresion || desarrolloLaboral %is% CrecimientoProfesionalOrganizacional ||nivelRemunerativo %is% 
            bajaRemuneracion, climaOrganizacional %is% aceptable),
  fuzzy_rule(presionLaboral %is% moderadaPresion && nivelRemunerativo %is% remuneracionPromedio,climaOrganizacional %is% aceptable),
  fuzzy_rule(presionLaboral %is% excesoPresion && desarrolloLaboral %is% CrecimientoProfesional && nivelRemunerativo 
             %is% bajaRemuneracion, climaOrganizacional %is% malo)
)

# construccion del sistemael sistema 
# Convierte las entradas a valores difusos
modelo <- fuzzy_system(variables, reglas)

# Mostramos las variables y las reglas del sistema
print(modelo)

# Mostramos el gráfico (trama) del sistema
plot(modelo)

## Probamos el sistema difuso ##
# Estos son algunos ejemplos para probar el sistema

# fuzzy_inference: procesa la salida en función de las entradas
cuadro_negativo <- fuzzy_inference(modelo, list(presionLaboral = 30, desarrolloLaboral =50, nivelRemunerativo =35))

# Ahora, defuzzificamos el ejemplo para transformar los parámetros en un número real
gset_defuzzify(ejemplo.1, "centroid")

# Mostramos el gráfico
plot(ejemplo.1)

# Interpretando: De acuerdo con el sistema, el clima es 0.6 aceptable (ver la gráfica del clima)


CuadrantePositivo <- fuzzy_inference(modelo, list(presionLaboral = 90, desarrolloLaboral =90 , nivelRemunerativo=100))
plot(ejemplo.2)

# Interpretando: Al bajar la temperatura, el modelo reduce la cantidad de clima "aceptable"
# a alrededor de 0.4 y crea un nuevo "pulso" con maximos globales en 40, lo que significa que
# clima es 1.0 "malo"

# Resetear el universo
sets_options("universe", NULL)
# ----------------------  imprimir resumen de campos  ----------------------

