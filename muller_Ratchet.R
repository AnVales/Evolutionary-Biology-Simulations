if(!require(rcompanion)){install.packages("fields")}
library(fields)

# parameters
replicas = 100

N_vector = c(10, 30, 100, 300, 1000)       #number of individuals
n_loc = 5       #number of genes
u = 0.05       #overall mutation rate per genome
u_ben = u * 1e-3  #beneficial fraction
u_del = u * 1e-1  #deletereous fraction
tdays = 25000   # lenght of experiment
f_ben_vector = c(0.005, 0.01, 0.02, 0.04, 0.08)    # fitness advantage confered by each mutation

container_info <- matrix(0, 5, 5) # matriz intermedia
end_matrix <- matrix(0, 25, 3) # matrix final, col1: N, col2: f_ben, col3:average

i<-0 # index for end_matrix

for (N in N_vector) {
  if (N == N_vector[1]) {
    col <- 1
  } else if (N == N_vector[2]) {
    col <- 2
  } else if (N == N_vector[3]) {
    col <- 3
  } else if (N == N_vector[4]) {
    col <- 4
  } else if (N == N_vector[5]) {
    col <- 5
  }
  
  for (f_ben in f_ben_vector) {
    if (f_ben == f_ben_vector[1]) {
      row <- 1
    } else if (f_ben == f_ben_vector[2]) {
      row <- 2
    } else if (f_ben == f_ben_vector[3]) {
      row <- 3
    } else if (f_ben == f_ben_vector[4]) {
      row <- 4
    } else if (f_ben == f_ben_vector[5]) {
      row <- 5
    }
    i<-i+1 # add 1 to index
    
    for (replica in 1:replicas) {
      # variables
      pop <-
        matrix(1, n_loc, N) # matriz llena de 1s, filas=genes, columnas=individuos
      fit_vector <-
        matrix(1, 1, N) # matriz llena de 1s, filas=1, columnas=individuos
      
      obs <- 0
      for (day in 1:days) {
        fit_vector <- 1 + colSums(pop) * f_ben # fit de cada indv
        
        # reproduccion #
        offspring <-
          sample(1:N, N / 2, replace = TRUE, prob = fit_vector) # cuello de botella de 0.5
        pop <-
          cbind(pop[, offspring], pop[, offspring], deparse.level = 0)
        
        # mutaciones beneficiosas#
        mutations <- rpois(1, N * u_ben)
        ind <- round(runif(mutations, 1, N))
        gen <- round(runif(mutations, 1, n_loc))
        coords <- cbind(c(gen), c(ind))
        pop[coords] <- 1
        
        # mutaciones deletereas #
        mutations <- rpois(1, N * u_del)
        ind <- round(runif(mutations, 1, N))
        gen <- round(runif(mutations, 1, n_loc))
        coords <- cbind(c(gen), c(ind))
        pop[coords] <- 0
        
        # final de replica, save data
        if (day == days) {
          container_info[row, col] <-
            container_info[row, col] + mean(fit_vector)
          
        }
      }
      if (replica == replicas){
        end_matrix[i, 1] = N
        end_matrix[i, 2] = f_ben
        end_matrix[i, 3] = (container_info[row, col])/replicas
      }
    }
  }
}

# matrix de aveg obtenida
print(end_matrix)

#plot
x<-log(N_vector)
y<-log(f_ben_vector)
z<-matrix(end_matrix[,3], 5,5)
image.plot(x, y, z, xlab="log(Population size)", ylab="log(Mutation benefit)",legend.lab ="Average fitness", main="Fitness with different population sizes and mutation benefits" )

