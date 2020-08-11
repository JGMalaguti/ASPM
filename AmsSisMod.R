AmsSisMod=function(n,N,Movimento,Batida,Tamanho){

  #Movimentos: 1 = Soma aos passos pares
              #2 = Subtração aos passos pares
              #3 = Soma aos passos pares e subtração aos passos ímpares

  k=ceiling(N/n)
  J=sample(1:k, 1, replace = F)
  Amostra=seq(J,N,by=k)	#Cria uma amostra clássica antes de alterar os valores
  
				#Primeiro se calcula o novo valor de j para os movimentos
  if(Movimento!=1){ 			#Caso o movimento subraia mov
    ValAr=J-Tamanho
    if(ValAr<1){
      ValAr=k-abs(J-Tamanho)
    }
  }
  if(Movimento!=2){			#Caso o movimento adicione mov
    ValAv=J+Tamanho
    
    if(ValAv>k){
      ValAv=J+Tamanho-k
    }
  }
  
  n=length(Amostra)
  Mudar=seq((Batida+1), n, by=Batida)
  Metade=seq(1,length(Mudar),by=2)
  UmaMud=Mudar[Metade]
  OutraMud=Mudar[-Metade]
  
  if(Movimento==1){					#Aqui se alteram os valores dos elementos
    Amostra[UmaMud]=(k*(UmaMud-1)+ValAv)
  }else if(Movimento==2){
    Amostra[UmaMud]=(k*(UmaMud-1)+ValAr)
  }else if(Movimento==3){
    Amostra[UmaMud]=(k*(UmaMud-1)+ValAv)
    Amostra[OutraMud]=(k*(OutraMud-1)+ValAr)
  }
  
  return(Amostra)
  
}
