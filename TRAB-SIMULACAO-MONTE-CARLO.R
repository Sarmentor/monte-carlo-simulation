# integral de uma fun??o no intervalo (0,1) pelo m?todo Monte Carlo 
# Integra??o entre XMIN e XMAX, YMIN e YMAX

#Defini??o dos limites do integral
XMIN <- 0.0
XMAX <- 1.0
YMIN <- 0.0
YMAX <- 1.0

# Defini??o da fun??o a integrar

f <- function(x,y)
{
#defini??o da fun??o a integrar
func <- x^2+3*log(y+1,base = exp(1))
#browser()
}

#Rotina para gerar n?meros aleat?rios

gera.binorm <- function(ro) 
{
x <- rnorm(1,ro)
y <- rnorm(1,ro)
answer <- cbind(x,y)
if ( x <1 & x > 0 & y < 1 & y > 0 )#filtra n?meros gerados aleatoriamente, para que estejam entre 0 e 1
{ 
answer
}else{gera.binorm(ro)} 
}

#
#Rotina para fazer uma integra??o por Monte Carlo
#Devolve estimativa do integral
#

mcint <- function(n,ro)# n- n?mero de pontos; ro - centro da distribui??o
{
s=0
s2=0
i=0
	for (i in 0:n-1)
	{
	aux=gera.binorm(ro)#chama fun??o geradora de coordenadas de pontos aleat?rios de distribui??o normal
	#C?lculo do valor da Fun??o
	k <- f(XMIN + (XMAX-XMIN)*aux[1], YMIN + (YMAX-YMIN)*aux[2])
	#Incrementa valores
	s <- s + k
	#s2 <- s2 + k*k
	#browser()
	}
#s2 = s2/n
#C?lculo do valor do integral
s =s/n
#erro = sqrt( (s2 - s*s) / (n-1) )
return (s) 
}

#Rotina de Gera??o de estimativas do valor do Integral
main <- function(nreps,nvals,ro) #nreps - n?mero de repeti??es/amostras; nvals - n?mero de pontos; ro - m?dia da distribui??o normal para gera??o de coordenadas aleat?rias
{
estimativas <- NULL
	for (i in 1:nreps)
	{
	estimativas[i] <- mcint(nvals,ro)#guarda estimativas do valor num vector 
	}
#browser()
	estimativas 
} 

#Defini??o de algumas vari?veis
aux_mean <- 0
ro <- 0.5 #(centro) dos valores aleat?rios a gerar, como temos dominio no intervalo 0 e 1, 0.5 ? o valor mais correcto para a fun??o rnorm()
i <- 100
while (i < 5000) {
nreps <- 100 #n?mero de repeti??es do c?lculo
simvalues <- main(nreps,i,ro) 
print(paste('O valor aproximado do c?lculo do integral da fun??o x^2+3*log(y+1), com x e y entre 0 e 1 ?:',mean(simvalues),'(',nreps,' amostras),com ',i,' pontos gerados por amostra'))
aux_mean = aux_mean + mean(simvalues)
i = i+100
}
media= aux_mean/49
print(paste('O valor final aproximado do c?lculo do integral da fun??o x^2+3*log(y+1), com x e y entre 0 e 1 ?:',media,'(',nreps,' amostras max.), com ',i,' pontos gerados no max. por amostra'))
hist(simvalues)
