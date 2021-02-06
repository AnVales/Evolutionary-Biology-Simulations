
# función fijación alelo

fijacion_alelo<-function(N, n_loc, daysF,replicasF, matrix_0, matrix_1, matrix_other, count_0, count_1, count_other){

  
  for (replica in 1:replicasF){
    
    # matriz de población
    pop<-matrix(0,n_loc,N) # una matriz llena de 0 de nuestra población y su gen
    
    # introducimos la mutación
    # primero las coordenadas
    ind<-round(runif(n_loc,1,N)) # columna
    gen<-round(runif(n_loc,1,n_loc)) # fila
    coords<-rbind(cbind(c(gen),c(ind))) # fila + columna
    
    # en la coordenada ponemos un uno
    pop[coords]<-pop[coords]+1
    
    #sumaI<-rowSums(pop[,])
    #print(sumaI) # funciona
    
    
    for (day in 1:daysF) { # para los 10 mil días
      # obs<-obs+1
      offspring<-sample(1:N, N/2, replace=TRUE) # cogemos la mitad de numeros al azar, de 1 a 100
      pop[1,]<-cbind(pop[,offspring],pop[,offspring],deparse.level=0) # se crian
      
      suma<-rowSums(pop)
      
      
      if (suma[1]==0){
        matrix_0[replica,1]<-day
        matrix_0[replica,2]<-suma[1]
        count_0<-count_0+1
        break
      }
      
      else if (suma[1]==N){
        matrix_1[replica,1]<-day
        matrix_1[replica,2]<-suma[1]
        count_1<-count_1+1
        break
      }
      
      else if (day==daysF){
        matrix_other[replica,1]<-day
        matrix_other[replica,2]<-suma[1]
        count_other<-count_other+1
        break
      }
      
      
      
      
      
    }
  }
  
  # porcentaje fijación
  porcentaje_fijacion=(count_1)/(count_0+count_1+count_other)
  
  # tiempo fijacion 
  sumaTF<-colSums(matrix_1[,])
  tiempo_fijacion=sumaTF[1]/count_1
  
  
  fijacion_N<-c(0.0,0.0)
  fijacion_N[1]<-porcentaje_fijacion
  fijacion_N[2]<-tiempo_fijacion
  #print(porcentaje_fijacion)
  #print(tiempo_fijacion)
  return(fijacion_N)
}






# POBLACIÓN N=100

# parámetros
N_100=100 # una población de 100
n_loc=1 # un gen
u=0 # no hay mutaciones nuevas
days=10000# para 10 mil días
replicas=100000

# storage
all_0_100<-matrix(0,replicas,2)
count_0_100<-0

all_1_100<-matrix(0,replicas,2)
count_1_100<-0

other_100<-matrix(0,replicas,2)
count_other_100<-0


# funcion
fijacion_100<-fijacion_alelo(N_100, n_loc, days, replicas, all_0_100, all_1_100, other_100, count_0_100, count_1_100, count_other_100)


# POBLACIÓN N=300

# parámetros
N_300=300 # una población de 100

# storage
all_0_300<-matrix(0,replicas,2)
count_0_300<-0

all_1_300<-matrix(0,replicas,2)
count_1_300<-0

other_300<-matrix(0,replicas,2)
count_other_300<-0


# funcion
fijacion_300<-fijacion_alelo(N_300, n_loc, days, replicas, all_0_300, all_1_300, other_300, count_0_300, count_1_300, count_other_300)


# POBLACIÓN N=300

# parámetros
N_1000=1000 # una población de 100

# storage
all_0_1000<-matrix(0,replicas,2)
count_0_1000<-0

all_1_1000<-matrix(0,replicas,2)
count_1_1000<-0

other_1000<-matrix(0,replicas,2)
count_other_1000<-0


# funcion
fijacion_1000<-fijacion_alelo(N_1000, n_loc, days, replicas, all_0_1000, all_1_1000, other_1000, count_0_1000, count_1_1000, count_other_1000)


# POBLACIÓN N=3000

# parámetros
N_3000=3000 # una población de 100

# storage
all_0_3000<-matrix(0,replicas,2)
count_0_3000<-0

all_1_3000<-matrix(0,replicas,2)
count_1_3000<-0

other_3000<-matrix(0,replicas,2)
count_other_3000<-0


# funcion
fijacion_3000<-fijacion_alelo(N_3000, n_loc, days, replicas, all_0_3000, all_1_3000, other_3000, count_0_3000, count_1_3000, count_other_3000)




# tamaño de población y probabilidad

Population_size<-c(N_100,N_300,N_1000,N_3000)

relacion_np1<- lm (1/Population_size ~ Fixation_probability )
anova(relacion_np1)
relacion_sumary_np1<-summary(relacion_np1)
relacion_sumary_np1
relacion_sumary_np1$r.squared

plot(Fixation_probability, 1/Population_size, main = "Population vs. probability of fixation")
abline(relacion_np1,col = "purple")


# tamaño de población y tiempo

relacion_nt<- lm (Population_size ~ Time_fixation )
anova(relacion_nt)
relacion_sumary_nt<-summary(relacion_nt)
relacion_sumary_nt
abline(relacion_nt)
relacion_sumary_nt$r.squared

plot(Time_fixation,Population_size, main = "Population vs. fixation time")
abline(relacion_nt,col = "purple")


# relacion de porcentaje fijacion y tiempo fijacion

Fixation_probability<-c(fijacion_100[1],fijacion_300[1],fijacion_1000[1],fijacion_3000[1])
Time_fixation<-c(fijacion_100[2],fijacion_300[2],fijacion_1000[2],fijacion_3000[2])


relacion_pt<- lm (1/Fixation_probability ~ Time_fixation )
anova(relacion_pt)
relacion_sumary_pt<-summary(relacion_pt)
relacion_sumary_pt
relacion_sumary_pt$r.squared


plot(Time_fixation,1/Fixation_probability, main = "Fixation probability vs. fixation time")
abline(relacion_pt,col = "purple")


# graficas de fijacion


fijacion_grafica<-function(N, n_loc, daysF,replicasF){
  
  time_matrix<-matrix(0,replicasF,daysF/1)
  for (replica in 1:replicasF){
    
    # matriz de población
    pop<-matrix(0,n_loc+1,N) # una matriz llena de 0 de nuestra población y su gen
    
    # introducimos la mutación
    # primero las coordenadas
    ind<-round(runif(n_loc,1,N)) # columna
    gen<-round(runif(n_loc,1,n_loc)) # fila
    coords<-rbind(cbind(c(gen),c(ind))) # fila + columna
    
    # en la coordenada ponemos un uno
    pop[coords]<-pop[coords]+1
    
    # almacenar tiempos
    #time_matrix<-matrix(0,replicasF,daysF/1)
    
    for (day in 1:daysF) { # para los 10 mil días
      offspring<-sample(1:N, N/2, replace=TRUE) # cogemos la mitad de numeros al azar, de 1 a 100
      pop<-cbind(pop[,offspring],pop[,offspring],deparse.level=0) # se crian
      
      suma<-rowSums(pop[,])
      
      if(day%%1==0){ # guardamos os tiempos
        time_matrix[replica,day/1]<-time_matrix[replica,day/1]+suma[1]
        #print(replica)
        #print(day)
      }
      
      else if (suma[1]==0||suma[1]==N||day==days){
        break }
      
    }
  }
  
  #print(colSums(fijacion_grafica_100[,])
  return(time_matrix)
}

# N=100

minireplicas=10000

day=400# para 10 mil días

fijacion_grafica_300<-fijacion_grafica(N_300, n_loc, day, minireplicas)

print(fijacion_grafica_300)


for (i in 1:day){

  if (i==1){
    plot(1:day, fijacion_grafica_300[1,], type = "l", ylim=c(0,300), main="Allele frequency when N=300", xlab="days", ylab="Allele frequency")
  } else {
    lines(1:day, fijacion_grafica_300[i,],col = "purple")
  }
  

}


dim(fijacion_grafica_300)


