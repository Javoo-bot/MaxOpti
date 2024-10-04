#10 septiembre 

# Define a custom function
f <- function(x) (-1 < x & x < 4) *exp(-1 / abs(x - 1))+ 10

# Plot the function
plot(f, from = -5, to = 5)

# ize the function in different intervals
optimize(f,c(-5,5))
optimize(f,c(-50,59))

# Plot the function over a wider range
plot(f, -50, 59, n=1001)

#Optimizacion trigonometrica
# Define function to convert degrees to radians
deg2rad <- function(deg) {
  return(deg * pi / 180)
}

# Define objective function (sine in radians)
f <- function(x) {
  return(sin(x))
}

# Optimization
result <- optimize(f, 
                   interval = c(deg2rad(10), deg2rad(100)), 
                   maximum = TRUE)

# Convert result back to degrees
max_x_deg <- result$maximum * 180 / pi
max_value <- result$objective

# Print results
cat("El máximo ocurre en x =", round(max_x_deg, 2), "grados\n")
cat("El valor máximo es", round(max_value, 4), "\n")

#Function Exploration with Plots
# Define a new function with conditional output
f <- function(x) ifelse (10 < x & x < 100, sin(x),0)

# Plot the function over different ranges
plot(f,10,100)
plot(f,-500,500)
plot(f,-500,500, lwd=3)

# Plot with more detail
plot(f,-50,150,n=1001)

# Convert degrees to radians within the function
f <- function(x) ifelse (10 < x & x < 100, sin(x* pi / 180),0)
plot(f, -50, 150,n=1001)

# Define interval and optimize
intervalo <- c(10, 100)
result <- optimize(f, interval = intervalo, maximum = TRUE); print(result)

# Add vertical line at maximum
abline(v=90)

# Alternative optimization method
f.neg <- function(x) -f(x)
optimize(f.neg, intervalo)

##  Robust Optimization Methods
# Define a simple quadratic function
f <- function(x) (x - 3)^2 + 5

# Perform optimization using Nelder-Mead method
result <- optim(par = 100, fn = f, method = "Nelder-Mead")
cat("Minimum x:", result$par, "\n")
cat("Minimum value:", result$value, "\n")

# Define Rosenbrock function
fr <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}

# Visualize Rosenbrock function (3D plot code omitted for brevity)

# Optimize Rosenbrock function with different starting points and methods
optim(c(0,0), fr)
optim(c(-5,-5), fr)
optim(c(10,7), fr)
optim(c(-5,-5), fr, method="Nelder-Mead")
optim(c(-5,-5), fr, method="CG")
optim(c(-5,-5), fr, method="BFGS")
optim(c(10,7), fr, method="BFGS")

##5: Gamma Distribution and Maximum Likelihood Estimation
# Define the sample
x <- c(90, 10, 60, 186, 61, 49, 14, 24, 56, 20, 79, 84, 44, 59, 29, 118,
       25, 156, 310, 76, 26, 44, 23, 62, 130, 208, 70, 101, 208)

# Gamma density function
f <- function(x,b,m) (b/m*(b*x/m)^(b-1)*exp(-b*x/m))/gamma(b)

# Example of density calculation
f(30,1,1/30) 

# Likelihood function
v <- function(X,b,m) prod(f(X,b,m))

# Examples of likelihood calculations
print(f(30, 1, 25)) # density at x=30, b=1, m=25
print(f(30:33, 1, 25)) # densities for x=30, 31, 32, 33
print(v(30:33, 1, 25)) # likelihood (product of densities) for x=30, 31, 32, 33

# Log-likelihood functions
logf <- function(x,b,m) log(b/m*(b*x/m)^(b-1)*exp(-b*x/m))/gamma(b)
logv <- function(X,b,m) sum(logf(X,b,m))

# Example log-likelihood calculations
logf(30:33,1,25)
logv(30:33,1,25)

# Function to optimize (negative log-likelihood)
optimizable <- function (par) -logv(x, par[1], par[2])
optim(c(1,1), optimizable)

# Checking results
logv(x, 1.6, 83)
logv(x,1.6,84)

#6: Linear Programming Examples

# Install and load lpSolve package
install.packages("lpSolve")
library("lpSolve")

# Simple linear programming example
m <- lp ("max",
         c(0.6, 0.5),
         rbind(c(1,2),c(3,1)),
         "<=",
         c(1,2))
print(m)

# More complex example (pill production)
resultado <- lp("max",
                c(2, 1), # objective function coefficients
                matrix(c(40,1,-2, 30,0,1), 3), # constraint coefficients
                c("<", ">", ">"),              # constraint directions
                c(600, 3, 0))                  # right-hand side of constraints
resultado$solution


#7: Gamma Distribution Maximum Likelihood (Continued)
muestra <- c(3,5,2,4,5,4,2,4)
p <- 1

dgamma(muestra,p)
prod(dgamma(muestra,p))

vero <- function(p) prod(dgamma(muestra,p))
vero(1)
vero(2)
vero(22)

optim(2,vero)

# For maximization (negative of the function)
vero <- function(p) -prod(dgamma(muestra,p))
optim(2,vero)

# Plotting the function
plot(Vectorize(vero), 0,10) 

#8: Complex Linear Programming Example (Mine Problem)

library(lpSolve)

# Define the problem using lp()
s <- lp("min", 
        c(2, 11, 12, 24, 13, 18),  # Objective function
        rbind(c(1,1,0,0,0,0),      # Constraints
              c(0,0,1,1,0,0),
              c(0,0,0,0,1,1),
              c(1,0,1,0,1,0),
              c(0,1,0,1,0,1)),
        c(rep("<=", 3), rep(">=", 2)),  # Constraint directions
        c(40, 40, 20, 40, 60))          # Right-hand side of constraints

# Print the solution
print(s)
s$solution
#la produccion se va a central + cara  
#[0,40] central a da a mina e

#[40,0] mina b da a mina e

#[0,20] mina c da a mina e

#problema de 8 reinas

## xij = 1 si hay reina en fila "i" y columna "j
## xij = 0 si no
## objetivo: max suma xij para i=1..n j=1..n

## restricciones:
 # para i = 1..n(suma xij para j=1..n) <=1 (una reina en cada fila)
 # para j = 1..n(suma xij para i=1..n) <=1 (una reina en cada columna)
 # para k = 2..16 (suma xij tales que i+k=k) <=1 (una reina en diagonal principal)
 # para k = -7..7 (suma xij tales que i+k=k) <=1 (una reina en diagonal secundaria)

# Function to solve N-Queens problem
reinas <- function (k, col, diag45, diag135, nReinas=8)
{
  if (k==nReinas) stop (paste (col, collapse="-"))
  else for (j in 1:nReinas)
    if (!(j%in%col) && !((j-k)%in%diag45) && !((j+k)%in%diag135))
    {
      reinas (k+1,
              c (col, j),
              c (diag45, j-k),
              c (diag135, j+k),
              nReinas)
    }
}

# Solve for 26 queens
reinas (0, integer(0), integer(0), integer(0), 26)

# Measure execution time
ptm <- proc.time()
try(reinas(0, integer(0), integer(0), integer(0), 26))
elapsed_time <- proc.time() - ptm
print(elapsed_time)

#10: Simulated Annealing Visualization
#cuando temperatura es alta la particula se mueve sin dificultad
#cuando se enfria le cuesta salir del minimo

options (OutDec = ",")
f <- function (x) sin(x*10) + x^2
xmin <- -2; xmax <- 2; niter <- 1000
T <- 10; sigma <- 0.5; I <- 10
x <- seq (xmin, xmax, 0.01)
plot (x, f(x), type="l")
x0 <- runif (1, xmin, xmax); y0 <- f(x0)
for (n in 1:niter) {
  T <- T * 0.995 #la temperatura va bajando en cada interaccion
  text (0, 4, signif(T,2))
  koloro <- rgb (1 - n/niter, 0, n/niter)
  points (x0, y0, col=koloro, pch=19)
  x1 <- max (xmin, min (xmax, rnorm (1, x0, sigma))) #generamos un punto x1 con una cierta dispersion con respecto al punto xo
  y1 <- f(x1)
  if (y1 <= y0) alfa <- 1 else
    alfa <- exp ((y0-y1)/T) #si el punto esta por debajo del inicial entonces elegimos el punto de debajo
  if (runif(1) < alfa){
    for (i in 0:I) {
      xi <- x0 + (i/I)*(x1-x0)
      yi <- f(xi)
      points (xi, yi, col=koloro, pch=19)
      points (xi, yi, col="white", pch=19)
    }
    lines (x, f(x))
    x0 <- x1; y0 <- y1
  }
  points (x0, y0, col=koloro, pch=19)
  text (0, 4, signif(T,2), col="white")
}
text (0, 4, signif(T,2))

#11: Optimizing a "Wild" function
fw <- function(x) {
  10 * sin(0.3 * x) * sin(1.3 * x^2) + 0.00001 * x^4 + 0.2 * x + 80
}

# Plot the function
plot(fw, -50, 50, n = 1000, main = "optim() minimising 'wild function'")

# Use optim to find the minimum
res <- optim(50, fw, method = "SANN",
             control = list(maxit = 20000, temp = 20, parscale = 20))
print(res)

# Find minima using optimize
min1 <- optimize(fw, c(-50, 50))
min2 <- optimize(fw, c(-100, 100))

# Add points to the graph
points(min1$minimum, min1$objective, col = 2, pch = 16, cex = 3)
points(min2$minimum, min2$objective, col = 2, pch = 16, cex = 3)

# Additional optimization attempts
optim(40, fw, method="SANN", control=list(maxit=100000))
optim(40, fw, method="SANN", control=list(maxit=1000000, temp=100))
points(-15.81, 67.47, col=3, pch=16, cex=3)
optim(40, fw, method="SANN", control=list(maxit=1e6, temp=75))

#interacciones permite enfriarse durante mas tiempo
#temperatura = energia inicial de la particula
#T es temperatura

#recocido basico: espacio de soluciones, puntos vecinos y temperatura
#parte del codigo para explicar mejor
#proceso iterativo que permite "saltar" a soluciones peores 
if (y1 <= y0) alfa <- 1 else
  alfa <- exp((y0 - y1) / T)
#Decision de aceptación
if (runif(1) < alfa) {
  x0 <- x1; y0 <- y1}

#explicacion
exp ((y0-y1)/T)
#a mayor temperatura es mas probable el salto
#cuanto mas altura (diferencia entre yo y y1) hay mas probabilidad del salto
#cuando vamos enfriando menos probabilidad de saltar en pozos lejanos

#12: N-Queens Problem (Simulated Annealing Approach)

# Function to generate a neighbor solution
vecino <- function(p, n=length(p)) {
  ij <- sample(n,2)
  p[ij] <- p[rev(ij)]
  p
}

# Function to count collisions
colisiones <- function(p, n=length(p)) {
  con <- 0
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      if(abs(p[i] - p[j]) == abs(i - j)) {
        con <- con + 1
      }
    }
  }
  return(con)
}

# Optimize using simulated annealing
optim(sample(8), colisiones, gr = vecino, method = "SANN")

# Larger problem
optim(sample(20), colisiones, vecino, method = "SANN", control = list(temp=5))

# Measure time for a very large problem
system.time({
  result <- optim(sample(300), colisiones, vecino, method = "SANN", 
                  control = list(maxit = 10000, temp = 5))
  print(result)
})


#13: Custom Simulated Annealing Implementation

# Custom implementation of Simulated Annealing for N-Queens
system.time({
  f <- colisiones
  niter <- 10000
  T <- 10
  x0 <- sample(100)  # Generate a random initial solution
  y0 <- f(x0)  # Evaluate the quality of the initial solution
  
  for (iter in 1:niter) {
    # Generate a new neighbor solution
    x1 <- vecino(x0)
    y1 <- f(x1)
    
    # Calculate acceptance probability
    if (y1 <= y0) {
      alfa <- 1
    } else {
      alfa <- exp((y0 - y1) / T)
    }
    
    # Accept or reject the new solution
    if (runif(1) < alfa) {
      x0 <- x1
      y0 <- y1
    }
    
    # Reduce temperature
    T <- T * 0.995
  }
  
  # Return results
  print(list(permu = x0, colis = y0))
})

# Compare with built-in optim function
system.time({
  result <- optim(sample(50), colisiones, vecino, method = "SANN", 
                  control = list(maxit = 10000, temp = 5))
  print(result)
})

#14: Parallel Processing Exploration
      
Sys.sleep(10)
system.time(Sys.sleep(10))

pausa <- function() Sys.sleep(10)

system.time(pausa())

system.time({pausa(); pausa()})

system.time(replicate(2, pausa())) #takes 40 seconds

pausa <- function(x) Sys.sleep(10)
system.time(lapply(1:2, pausa))
system.time(parallel::mclapply(1:2, pausa)) #the two pauses execute one after another

library(parallel)
system.time(parallel::mclapply(1:2, pausa))
#these are two simultaneous pauses, so they're two pauses on each processor
detectCores()

#15: Linear Regression and lapply Example

# Simple linear regression
regre <- lm(mpg ~ wt, mtcars)
regre$coefficients
summary(regre)$r.squared

list(R2 = summary(regre)$r.squared, coef = regre$coefficients)

# Using lapply for multiple regressions
lista <- lapply(1:ncol(mtcars), function(i) {
  regre <- lm(mtcars$mpg ~ mtcars[, i])
  list(R2 = summary(regre)$r.squared, coef = coef(regre))
})
lista

names(lista) <- names(mtcars)[-1]

# Parallel processing example
library(parallel)
system.time(parallel::mclapply(1:2, pausa))


#24 Septiembre

nReinas <- function (n=8, maxiter=10000) # nu'mero de reinas / taman~o del tablero
{
  ## cada solucio'n se representa como una permutacio'n, p
  colisiones <- function (p)
  {
    colisiones <- 0
    for (i in 1:(n-1))
      for (j in (i+1):n)
        colisiones <- colisiones + (j-i == abs(p[j]-p[i]))
    colisiones
  }
  ## solucio'n vecina de la actual por trasposicio'n
  otra <- function (p)
  {
    ij    <- sample (n, 2)
    p[ij] <- p[rev(ij)]
    p
  }
  ## recocido simulado
  temp <- n
  iter <- 0
  sol0 <- sample (n) #
  val0 <- colisiones (sol0)
  while (iter<maxiter && val0>0)
  {
    sol1 <- otra (sol0)
    val1 <- colisiones (sol1)
    if (val1 <= val0) alfa <- 1 else alfa <- exp ((val0-val1)/temp)
    if (rbinom(1,1,alfa)) {sol0 <- sol1; val0 <- val1}
    iter <- iter + 1
    temp <- temp * 0.995
  }
  cat ("Mejor solucio'n:", sol0, ". Mejor resultado:", val0, "\n")
  return(list(colisiones=val0, posiciones = sol0))  #le añadimos esta lista en vez de return(sol0)
}

nReinas(100, 1e4) #en examen fijarse en parametros y solo usar esos 
#salen dos resultados porque el segundo es interactivo y el primero es raw
nReinas(100, 1e4) -> resultado; print(resultado) #esto nada 

#resultado: vector de soluciones que es util

#Ej:usar lappy con diferentes tamaños de nreinas
?lapply

#prueba en clase
lista <- lapply(1:5, function(i) {
  result <- nReinas(i, 1e4)
  return(result)
}) #mi error
  
resultados <- lapply(c(8:12), nReinas); resultados #primera opcion

#resultados2 <- lapply(rep(20,5) nReinas); resultados2 #error sintaxis

#lo mismo pero cambiando el numero de iteraciones en lugar del numeros de reinas
resultados <- lapply(c(100,1000,10000), nReinas) #si queremos hacer el calculo con el segundo argumento

envoltorio <- function(x) nReinas(8,x)
resultados <- lapply(c(100,1000,10000), envoltorio)

#definir una función que combine los dos pasos anteriores
#entrada: número de recocidos que ejecutar
#salida: la lista creada por lapply

resultados <- recocidos(2) #hacer dos recocidos de tamaño 50

gran_recocidos <- function(k) lapply(rep(150,k), nReinas)
gran_recocidos(150) #en realidad solo hay una salida pq hay colisiones

base_recocidos <- function(k) lapply (rep(50,k), nReinas)

m_recocidos <- function(k) mcapply (rep(50,k), nReinas)

system.time(resultados <- base_recocidos(2))

library(parallel)
system.time(resultados <- m_recocidos(2)) #mcapply no funciona en windows

#probamos con mas valores (en windows mcapply no funciona)
library(parallel)
m_recocidos <- function(k) {
  cl <- makeCluster(getOption("cl.cores", 11))
  resultados <- parLapply(cl, rep(50, k), nReinas)
  stopCluster(cl)
  
  return(resultados)
}

system.time(resultados <- m_recocidos(1))

#filtrar resultados para quedarnos con soluciones con 0 colisiones:

resultados0 <- base_recocidos(2)
str(resultados0)
class(resultados0)

Filter(function(x) x$colisiones ==0, resultados0)

resultados0[lapply(resultados0, function(x)x$colisiones)] #da error corregir pq estara mal copiado

sapply(resultados0, function(x) x$colisiones == 0)

resultados0[lapply(resultados0, function(x) x$colisiones) ==0] -> resultados.buenos

resultados.buenos <- lapply(resultados.buenos, function(x) x$posiciones)

#02 octubre
#Preguntas repaso
#Recocido
#Que es el recocido
#Cual es la formula y que significa cada elemento
#Que es la decision verdadero o falso cuando aumenta T
#Porque recocido es un algoritmo minimizacion
#Si vamos hacia arriba en la formula el exponente queda entre 0 y 1 entonces... 
#porque hablamos de probabilidad y decimos "probabilidad de salto"?


#N-reinas: como representamos el vecino 
#Que son las permutaciones
#¿Vecino = permutacion? ¿Que tiene que ver? 
#R tiene una funcion optim que permite ejecutar el recocido similado, que argumentos hay que usar?
#Que argumentos tiene la funcion de optim en un recocido
#Que dos opciones hicimos para resolver el problema de las n reinas
#Que representan la 'ij' y cual es la distancia de las parejas


#Que es la paralelizacion
#Cual es la utilidad de lapply y ¿que es lo que hace?
#Si añadimos mcapply se reduce el tiempo a la mitad, ¿porque?
#Que significa el "mc" en "mclapply"
#Que pasa si hacemos dos recocidos en paralelo¿?

vecino <- function(p, n=length(p)) {ij <- sample(n,2); p[ij] <-p[rev(ij)];p}
vecino(1:8) #si lo hago otra vez cambia el resultado

#Posibles parejas 
combn(8,2)

#lo que hace con los indices es comprobar que no esten en las diagonales
#misma diagonal si i_1 - j_1 = i_2 - j_2 o i_1 + j_1 = i_2 + j_2
colisiones <- function(p, n = length(p))
                    sum(apply(combn(n,2),
                    2, 
                    function(ij) {
                      i <- ij[1]
                      j <- ij[2]
                     (j-i) == abs(p[ij][1]-p[ij][2])}))
vecino <- function(p, n = length(p)) {ij <- sample(n,2); p[ij] <- p[rev(ij)];p}
sol <- optim(sample(5), colisiones, vecino, method = "SANN")
colisiones(sample(5))
c(col=sol$value, permu=sol$par)
names(sol) #par es la combinacion ganadora 

#cambiar parametros: temperatura 
sol <- optim(sample(5), colisiones, vecino, method = "SANN", control=c(temp=1))
c(col=sol$value, permu=sol$par)

system.time({
  sol1 <- optim(sample(30), colisiones, vecino, method = "SANN", control = list(temp = 1))
  sol2 <- optim(sample(30), colisiones, vecino, method = "SANN", control = list(temp = 1))
})

c(col1 = sol1$value, col2 = sol2$value)

#dos funciones de recocido en paralelo 
recocido <- function(x)
   optim(sample(30), colisiones, vecino, method="SANN",control=c(temp=1))
system.time( sol <- lapply(1:2, recocido))
c(col1 = sol1$value, col2 = sol2$value)

system.time( sol <- lapply(1:2, recocido))
c(col1 = sol[[1]]$value, col2 = sol[[2]]$value)




#nos saltamos la limitacion de mclapply
recocido <- function(x)
{
  colisiones <- function(p, n = length(p))
    sum(apply(combn(n,2),
              2, 
              function(ij) {
                i <- ij[1]
                j <- ij[2]
                (j-i) == abs(p[ij][1]-p[ij][2])}))
  vecino <- function(p, n = length(p)) {ij <- sample(n,2); p[ij] <- p[rev(ij)];p}
  optim(sample(30), colisiones, vecino, method="SANN",control=c(temp=1))
  
}  
library(parallel)
cl  <- makeCluster (detectCores() - 1)
system.time(sol <- parLapply(cl, 1:2, recocido))
stopCluster (cl)

detectCores()

#problema del viajante
?optim
library(stats)
eurodist

eurodistmat <- as.matrix(eurodist)

distance <- function(sq) {  # Target function
  sq2 <- embed(sq, 2) #organiza los numeros por parejas 
  sum(eurodistmat[cbind(sq2[,2], sq2[,1])])
}

sq <- c(1:nrow(eurodistmat), 1) #valores 1 2 3 4 5 1
distance(sq) #distancia entre Athens,Barcelona,Brussels,Calais,Cherbourg
colnames(eurodistmat)[1:5]

genseq <- function(sq) {  # Generate new candidate sequence
  idx <- seq(2, NROW(eurodistmat)-1)
  changepoints <- sample(idx, size = 2, replace = FALSE)
  tmp <- sq[changepoints[1]]
  sq[changepoints[1]] <- sq[changepoints[2]]
  sq[changepoints[2]] <- tmp
  sq
}


#acabar este ejercicio
#interpretar como ejecuta la distancia 







