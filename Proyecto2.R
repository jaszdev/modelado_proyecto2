# install
install.packages('hash')
install.packages('diagram')
install.packages("expm")
install.packages("markovchain")

# dependencies
library(hash)
library(diagram)
library(expm)
library(markovchain)

#Cargar datos iniciales e inicializacion
df= (read.csv(file = "Bitacora.csv", header=T, encoding = "UTF-8"))
state_dict = hash()
trans_dict = hash()
prevState = ''
firstState = ''


# df a lista 
df_list = rep(0, 168)
i = 1
for (col in df) {
  for (entry in col) {
    val = 1
    if (entry == 'D') 
      val = 2
    else if (entry == 'E') 
      val = 3
    else if (entry == '0') 
      val = 4
    df_list[i] = val
    i = i + 1
  }
}


#Creación de diccionarios sobre estados y transiciones
for (col in df) {
  for (entry in col) {
    if (entry %in% keys(state_dict)) {
      state_dict[[entry]] = state_dict[[entry]] + 1
    }
    else {
      if (firstState == '') { firstState = entry }
      state_dict[[entry]] = 1
    }
    if (prevState != '') {
      tran_name = paste(prevState, entry, sep="->")
      if (tran_name %in% keys(trans_dict)) {
        trans_dict[[tran_name]] = trans_dict[[tran_name]] + 1
      }
      else {
        trans_dict[[tran_name]] = 1
      }
    }
    prevState = entry
  }
}

# Agregar transicion de ultimo estado a primero
tran_name = paste(prevState, firstState, sep="->")
if (tran_name %in% keys(trans_dict)) {
  trans_dict[[tran_name]] = trans_dict[[tran_name]] + 1
} else {
  trans_dict[[tran_name]] = 1
}

states = keys(state_dict)
transitions = keys(trans_dict)
m_dim = length(states)
m_matrix = matrix(0, nrow=m_dim, ncol=m_dim, dimnames=list(states, states))

#punto 1

#Llenado de matrix de markov
for (state in states) { #Para recorrer cada columna de la matriz
  den = state_dict[[state]]
  pattern = paste("^",state, sep="")
  for (tran in transitions) { #Para recorrer cada entrada de la col.
    if (grepl(pattern,tran)) {
      tstate = substr(tran, nchar(tran), nchar(tran))
      num = trans_dict[[tran]]
      prob = num / den
      m_matrix[state, tstate] = prob
    }
  }
}

print(m_matrix)
#Diagrama de cadena de markov
plotmat(round(m_matrix,2), pos = c(2,2),
        lwd = 1, box.lwd = 2,
        cex.txt = 0.8,
        box.size = 0.1,
        box.type = "circle",
        box.prop = 0.5,
        box.col = "light yellow",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .13,
        shadow.size = 0,
        main = "")

#punto 2
m_markov <- as(m_matrix, "markovchain")
is.regular(m_markov) #Se verifica si la matrix es regular
# Se requieren 2 pasos!

#punto 4
s_states <- steadyStates(m_markov) #Se obtiene el vector de estados 


#puntos 3 y 5
N <- 1000
P1 = rep(0,N)
P2 = rep(0,N)
P3 = rep(0,N)
P4 = rep(0,N)
PS = rep(0,N) # porcentajes de similitud

for(i in 1:N){ 
  H <- 168; X <- rep(0,H); U <- runif(H);
  P <- m_matrix ; q <- c(0,1,0,0) # P0
  for (k in 1:H) {
    if (U[k] < q[1])
      X[k] = 1
    else if (U[k] < q[1]+q[2])
      X[k] = 2
    else if (U[k] < q[1]+q[2]+q[3])
      X[k] = 3
    else
      X[k] = 4
    q <- P[X[k], ] 
  }
  P1[i] <- sum(X==1)/H
  P2[i] <- sum(X==2)/H
  P3[i] <- sum(X==3)/H
  P4[i] <- sum(X==4)/H #comparar con distribucion estacionaria
  
  # comparar similitud entre X y bitacora
  PS[i] = sum(X == df_list) / H
  
}
par(mar = 0.1 + c(4,4,4,4));
plot(1:H, X, yaxt="n", ylim=c(0,4))
axis(2, at = c(1,2,3,4), labels = c("Clases","Dormir","Estudio","Ocio"), las = 1)

#punto 6
p1 <- mean(P1)
p2 <- mean(P2)
p3 <- mean(P3)
p4 <- mean(P4)
print(s_states)
cat(p1,p2,p3,p4)
#Referirse a la similitud (buena)

# punto 7 
((mean(PS)))

# punto 8
N <- 1000

for(i in 1:N){ 
  H <- 168; X <- rep(0,H); U <- runif(H);
  P <- m_matrix ; q <- s_states;
  for (k in 1:H) {
    if (U[k] < q[1])
      X[k] = 1
    else if (U[k] < q[1]+q[2])
      X[k] = 2
    else if (U[k] < q[1]+q[2]+q[3])
      X[k] = 3
    else
      X[k] = 4
  }
  
  # comparar similitud entre X y bitacora
  PS[i] = sum(X == df_list) / H
  
}
par(mar = 0.1 + c(4,4,4,4));
plot(1:H, X, yaxt="n", ylim=c(0,4))
axis(2, at = c(1,2,3,4), labels = c("Clases","Dormir","Estudio","Ocio"), las = 1)

expected = s_states[1]**2 + s_states[2]**2 + s_states[3]**2 + s_states[4]**2

#punto 9
((mean(PS)))