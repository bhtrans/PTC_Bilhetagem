
library(dplyr)
library(stringr)
library(tidyr)

#EMBARQUE E DESEMBARQUE COM SENHA

setwd(choose.dir())
CITOP<-read.csv(file.choose(),sep=";")
DIN<-read.csv(file.choose(),sep=";")
CITGIS1<-read.csv(file.choose(),sep=";")
CITGIS2<-read.csv(file.choose(),sep=";")
SUBLINHA_VS_PED<-read.csv(file.choose(),sep=";",encoding='latin1',stringsAsFactors=FALSE)
VIAGENS<-read.csv(file.choose(),sep=";",encoding='latin1')
COORDENADAS_ESTACOES<-read.csv(file.choose(),sep=";")
ESTACOES_LINHAS<-read.csv(file.choose(),sep=';')
#FILTRANDO A LINHA
l<-'104'
CITOP_l<-filter(CITOP,CODIGO_LINHA==l)
CITOP<-filter(CITOP,CARTAO_USUARIO %in% CITOP_l$CARTAO_USUARIO)
rm(CITOP_l)

#UNINDO ARQUIVOS
CITGIS<-rbind(CITGIS1,CITGIS2)
rm(CITGIS1,CITGIS2)
DIN<-DIN %>% select(colnames(CITOP))
DIN$DATAHORA_UTILIZACAO<-str_sub(DIN$DATAHORA_UTILIZACAO,end=-3)
DIN<-filter(DIN,DIN$CODIGO_LINHA==l)
CITOP<-rbind(CITOP,DIN)
rm(DIN)
CITOP$X<-NULL

#TRANSFORMANDO CARTOES PARA TEXTO

CITOP$CARTAO_USUARIO<-as.character(CITOP$CARTAO_USUARIO)

#SEPARANDO DATA E HORA NO CITOP

CIT<-str_split_fixed(CITOP$DATAHORA_UTILIZACAO," ",2)
colnames(CIT) <-c("Data","Hora")
CITOP<-cbind.data.frame(CITOP,CIT)
rm(CIT)
CITOP$DATAHORA_UTILIZACAO<-NULL
CITOP$Data<-NULL
CITOP$X<-NULL

#CRIANDO CITOP_ESTACOES E CITOP SUPLEMENTAR

CITOP_ESTACOES<-filter(CITOP,CODIGO_LINHA %in% c("AC01","AC02","AC03","AC04","AC05","AC06","AC07","AC08","AC09","AC10","AC11","AC13","AC14","AC15","AC16","AC17","AC18","AC19","AC20","AC21","AC22","AC23","AC24","AC25","CM02","CM03","CM04","CM05","CM06","CM07","CM08","CM09","CM10","SD01","SD02","PR01","PR02","1000","1002","2000","2002","3000","3002","4002","4003","4004","6000","7000","5001","5002","5003","5004","5005","5006","5007","5008","5009","5010","5011","5012","5013","5014","5015","5016","5017","5018","5019","1001","2001","3001","4001","6001","7001"))
CITOP_SUPLEMENTAR<-filter(CITOP,NOME_OPERADORA=="SINDPAUTRAS")

#RETIRANDO VALIDACOES EM ESTACOES E SUPLEMENTARES

CITOP<-filter(CITOP,!CODIGO_LINHA %in% c("AC01","AC02","AC03","AC04","AC05","AC06","AC07","AC08","AC09","AC10","AC11","AC13","AC14","AC15","AC16","AC17","AC18","AC19","AC20","AC21","AC22","AC23","AC24","AC25","CM02","CM03","CM04","CM05","CM06","CM07","CM08","CM09","CM10","SD01","SD02","PR01","PR02","1000","1002","2000","2002","3000","3002","4002","4003","4004","6000","7000","5001","5002","5003","5004","5005","5006","5007","5008","5009","5010","5011","5012","5013","5014","5015","5016","5017","5018","5019","1001","2001","3001","4001","6001","7001"))
CITOP<-filter(CITOP,NOME_OPERADORA!="SINDPAUTRAS")

#VEICULO E HORA EM OPERACAO NO CITOP

CITOP$VEIC_COM_FX<-str_c(CITOP$CODIGO_VEICULO,str_sub(CITOP$Hora,end=2),sep="-")
VEIC.CITGIS<-data.frame(unique(str_c(CITGIS$vei_nro_veiculo_gestor,str_sub(CITGIS$horario_passagem,end=2),sep="-")))
CITOP<-merge(CITOP,VEIC.CITGIS,by.x="VEIC_COM_FX",by.y="unique.str_c.CITGIS.vei_nro_veiculo_gestor..str_sub.CITGIS.horario_passagem..",all.x=T)

#CRIANDO A CHAVE NO CITOP

CITOP$CHAVE<-paste(CITOP$CODIGO_VEICULO,CITOP$Hora)
CITOP$CHAVE<-gsub(':','',CITOP$CHAVE)
CITOP$CHAVE<-gsub(' ','',CITOP$CHAVE)
CITOP$CHAVE<-as.numeric(CITOP$CHAVE)

#RETIRANDO VEICULOS NO CITGIS
CITOP$VEIC_COM_FX<-str_c(CITOP$CODIGO_VEICULO,str_sub(CITOP$Hora,end=2),sep="-")
CITGIS$VEIC_COM_FX<-str_c(CITGIS$vei_nro_veiculo_gestor,str_sub(CITGIS$horario_passagem,end=2),sep='-')
CITGIS<-filter(CITGIS,VEIC_COM_FX %in% CITOP$VEIC_COM_FX)
CITGIS$VEIC_COM_FX<-NULL
CITOP$VEIC_COM_FX<-NULL

#CRIANDO A CHAVE NO CITGIS
CITGIS$CHAVE<-paste(CITGIS$vei_nro_veiculo_gestor,CITGIS$horario_passagem)
CITGIS$CHAVE<-gsub(':','',CITGIS$CHAVE)
CITGIS$CHAVE<-gsub(' ','',CITGIS$CHAVE)
CITGIS$CHAVE<-as.numeric(CITGIS$CHAVE)

#JUNTANDO OS ARQUIVOS DE EMBARQUE
CITOP<-arrange(CITOP,CODIGO_VEICULO,Hora)
CITGIS<-arrange(CITGIS,vei_nro_veiculo_gestor,horario_passagem)

procura <- function(x, y){
  return(which.min(abs(x-y)))
}

Chave_CITOP <- 0

for (j in 1:length(CITOP$CHAVE)){
  Chave_CITOP[j] <- procura(CITOP$CHAVE[j], CITGIS$CHAVE)
}

CITOP$SIU<-CITGIS$par_cod_siu[Chave_CITOP]
CITOP$Cod.Viagem<-CITGIS$cod_viagem[Chave_CITOP]
CITOP$PC<-CITGIS$sentido_itinerario[Chave_CITOP]
CITOP$PC<-gsub('PC','',CITOP$PC)  
CITOP$Hora_Abertura_Viagem<-CITGIS$inicio_viagem[Chave_CITOP]
CITOP$Sublinha<-CITGIS$sub_lin_sg_linha[Chave_CITOP]
CITOP<-arrange(CITOP,CODIGO_VEICULO,Hora)
CITOP$CHAVE<-NULL
CITOP_ESTACOES$SIU<-CITOP_ESTACOES$CODIGO_LINHA
CITOP_ESTACOES$Cod.Viagem<-1
CITOP_ESTACOES$PC<-1
CITOP_ESTACOES$Hora_Abertura_Viagem<-CITOP_ESTACOES$Hora
CITOP_ESTACOES$Sublinha<-1
CITOP_SUPLEMENTAR$SIU<-CITOP_SUPLEMENTAR$CODIGO_LINHA
CITOP_SUPLEMENTAR$Cod.Viagem<-1
CITOP_SUPLEMENTAR$PC<-1
CITOP_SUPLEMENTAR$Hora_Abertura_Viagem<-CITOP_SUPLEMENTAR$Hora
CITOP_SUPLEMENTAR$Sublinha<-1
CITOP<-CITOP %>% select(colnames(CITOP_ESTACOES))
CITOP<-rbind(CITOP,CITOP_ESTACOES,CITOP_SUPLEMENTAR)
CITOP<-arrange(CITOP,CARTAO_USUARIO,Hora)
rm(CITOP_ESTACOES,CITOP_SUPLEMENTAR,VEIC.CITGIS,Chave_CITOP,j)
EMBARQUES<-CITOP

###Coordenadas de Ponto e Estações

EMBARQUES$SIU[is.na(EMBARQUES$SIU)]<-0
SIU_X_Y<-SUBLINHA_VS_PED %>% dplyr::select(Código.SIU,Coord..X,Coord..Y)
colnames(COORDENADAS_ESTACOES)<-colnames(SIU_X_Y)
SIU_X_Y<-rbind(SIU_X_Y,COORDENADAS_ESTACOES)
SIU_X_Y<-SIU_X_Y[!duplicated(SIU_X_Y$Código.SIU),]
SIU_X_Y$Código.SIU<-(as.character(SIU_X_Y$Código.SIU))
EMBARQUES<-merge(EMBARQUES,SIU_X_Y,by.x="SIU",by.y="Código.SIU",all.x=T)
EMBARQUES<-arrange(EMBARQUES,CARTAO_USUARIO,Hora)
EMB<-filter(EMBARQUES,EMBARQUES$CODIGO_LINHA==l)

#PRIMEIRA VALIDACAO

val1<-EMBARQUES %>% dplyr::select(CARTAO_USUARIO,SIU,CODIGO_LINHA)
val1<-val1[!duplicated(val1$CARTAO_USUARIO),]
colnames(val1)<-c("CARTAO_USUARIO","SIU_VAL1","LINHA_VAL1")
EMBARQUES<-merge(EMBARQUES,val1,all.x=T)
rm(val1)

#SEGUNDA_VALIDACAO

EMBARQUES<-arrange(EMBARQUES,CARTAO_USUARIO,Hora)

i=1
for(i in 1:nrow(EMBARQUES)){
  if(i==nrow(EMBARQUES)){
    EMBARQUES$SIU_DESTINO[i]<-EMBARQUES$SIU_VAL1[i]
    EMBARQUES$LINHA_DESTINO[i]<-EMBARQUES$LINHA_VAL1[i]
  }else if(EMBARQUES$CARTAO_USUARIO[i]==EMBARQUES$CARTAO_USUARIO[i+1]){
    EMBARQUES$SIU_DESTINO[i]<-EMBARQUES$SIU[i+1]
    EMBARQUES$LINHA_DESTINO[i]<-EMBARQUES$CODIGO_LINHA[i+1]
  }else{
    EMBARQUES$SIU_DESTINO[i]<-EMBARQUES$SIU_VAL1[i]
    EMBARQUES$LINHA_DESTINO[i]<-EMBARQUES$LINHA_VAL1[i]
  }
}

OD<-EMBARQUES
EMBARQUES<-filter(EMBARQUES,CODIGO_LINHA==l)
EMBARQUES<-filter(EMBARQUES,TIPO_CARTAO!='DINHEIRO')
colnames(SIU_X_Y)<-c("SIU_DESTINO","X_DESTINO","Y_DESTINO")
EMBARQUES<-merge(EMBARQUES,SIU_X_Y,all.x=T)
EMBARQUES<-arrange(EMBARQUES,CARTAO_USUARIO,Hora)
EMBARQUES$LINHA_VAL1<-NULL
EMBARQUES$SIU_VAL1<-NULL
## rm(SIU_X_Y)

#RETIRAR CARTOES COM VALIDACAO NO MESMO LOCAL

EMBARQUE_VALDUPLA<-filter(EMBARQUES,SIU==SIU_DESTINO)
EMBARQUE_VALDUPLA<-filter(EMBARQUES,CARTAO_USUARIO %in% EMBARQUE_VALDUPLA$CARTAO_USUARIO)
EMBARQUE_VALDUPLA<-arrange(EMBARQUE_VALDUPLA,CARTAO_USUARIO,Hora)
EMBARQUES<-filter(EMBARQUES,SIU!=SIU_DESTINO)
EMBARQUES<-arrange(EMBARQUES,CARTAO_USUARIO,Hora)

#ATRIBUINDO SENTIDO NO ITINERARIO#

EMBARQUES$LIN_SUB_PC_EMB<-str_c(EMBARQUES$Sublinha,EMBARQUES$PC,sep="-")
EMBARQUES$LIN_SUB_PC_EMB<-gsub('-0','-',EMBARQUES$LIN_SUB_PC_EMB)
SUBLINHA_VS_PED$LIN_SUB_PC_EMB<-str_c(SUBLINHA_VS_PED$Linha,SUBLINHA_VS_PED$Sublinha,SUBLINHA_VS_PED$PC,sep="-")
SUBLINHA_VS_PED$Coord..X<-as.double(SUBLINHA_VS_PED$Coord..X)
SUBLINHA_VS_PED$Coord..Y<-as.double(SUBLINHA_VS_PED$Coord..Y)
SUBLINHA_VS_PED$Tipo.Ponto<-as.character(SUBLINHA_VS_PED$Tipo.Ponto)
PED_PROXIMO<-SUBLINHA_VS_PED

i<-0
for (i in 1:nrow(PED_PROXIMO)){
  if(i==1){
    PED_PROXIMO$Sentido[i]<-1
  }else if(PED_PROXIMO$Tipo.Ponto[i]=="PED"){
    PED_PROXIMO$Sentido[i]<-PED_PROXIMO$Sentido[i-1]
  }else if(PED_PROXIMO$Tipo.Ponto[i]=="NOT"){
    PED_PROXIMO$Sentido[i]<-PED_PROXIMO$Sentido[i-1]
  }else if(PED_PROXIMO$Tipo.Ponto[i]=="PR"){
    PED_PROXIMO$Sentido[i]<-2
  }else if(PED_PROXIMO$Tipo.Ponto[i]=="PC" & PED_PROXIMO$PC[i]==1){
    PED_PROXIMO$Sentido[i]<-1
  }else if(PED_PROXIMO$Tipo.Ponto[i]=="PC" & PED_PROXIMO$PC[i]==2){
    PED_PROXIMO$Sentido[i]<-2
  }
}

EMBARQUES$LIN_SUB_PC_EMB_SIU<-str_c(EMBARQUES$LIN_SUB_PC_EMB,EMBARQUES$SIU,sep="-")
PED_PROXIMO$LIN_SUB_PC_EMB_SIU<-str_c(PED_PROXIMO$LIN_SUB_PC_EMB,PED_PROXIMO$Código.SIU,sep="-")
EMBARQUES<-merge(EMBARQUES,PED_PROXIMO,by.x="LIN_SUB_PC_EMB_SIU",by.y="LIN_SUB_PC_EMB_SIU",all.x=T)
EMBARQUES<-EMBARQUES %>% dplyr::select(LIN_SUB_PC_EMB_SIU,SIU_DESTINO,CARTAO_USUARIO,SIU,NOME_OPERADORA,CODIGO_VEICULO,CODIGO_LINHA,TIPO_CARTAO,VALOR_COBRADO,Hora,Cod.Viagem,PC.x,Hora_Abertura_Viagem,Sublinha.x,Coord..X.x,Coord..Y.x,X_DESTINO,Y_DESTINO,LIN_SUB_PC_EMB.x,Sentido)
EMBARQUES<-arrange(EMBARQUES,CARTAO_USUARIO,Hora)
EMBARQUES$Sentido[is.na(EMBARQUES$Sentido)]<-1

i<-0

for(i in 1:nrow(EMBARQUES)){
  if(EMBARQUES$PC.x[i]=="2" && EMBARQUES$Sentido[i]=="1"){
    EMBARQUES$Sentido[i]=="2"
  }else{
    EMBARQUES$Sentido[i]==EMBARQUES$Sentido[i]
  }
}

#PROCURANDO PED DE DESEMBARQUE

procura_ped_proximo<-function(x,y,z,w){
  return(which.min(sqrt(((z-x)^2)+((w-y)^2))))
}

EMBARQUES$X_DESTINO<-as.numeric(EMBARQUES$X_DESTINO)
EMBARQUES$Y_DESTINO<-as.numeric(EMBARQUES$Y_DESTINO)
EMBARQUES$Coord..X.x<-as.numeric(EMBARQUES$Coord..X.x)
EMBARQUES$Coord..Y.x<-as.numeric(EMBARQUES$Coord..Y.x)
EMBARQUES$DESLOC<-as.integer(sqrt((EMBARQUES$X_DESTINO-EMBARQUES$Coord..X.x)^2)+((EMBARQUES$Y_DESTINO-EMBARQUES$Coord..Y.x)^2))/1000
EMBARQUES_600<-filter(EMBARQUES,DESLOC<600)
EMBARQUES<-filter(EMBARQUES,DESLOC>=600)
colnames(ESTACOES_LINHAS)<-c("Linha","Estacao")

#ESTACAO_CODIGO SIU

ESTACAO_SIU<-filter(SUBLINHA_VS_PED,Estação.1!='')
ESTACAO_SIU<-filter(ESTACAO_SIU,Linha %in% ESTACOES_LINHAS$Linha)
ESTACAO_SIU<-ESTACAO_SIU[!duplicated(ESTACAO_SIU$Código.SIU),]
ESTACAO_SIU<-ESTACAO_SIU %>% dplyr::select(Código.SIU,Coord..X,Coord..Y)

i<-0
Cód_EST<-0

for(i in 1:nrow(ESTACAO_SIU)){
  Cód_EST[i]<-procura_ped_proximo(ESTACAO_SIU$Coord..X[i],ESTACAO_SIU$Coord..Y[i],COORDENADAS_ESTACOES$Coord..X,COORDENADAS_ESTACOES$Coord..Y)  
}

ESTACAO_SIU$Cód_Est<-COORDENADAS_ESTACOES$Código.SIU[Cód_EST]
rm(Cód_EST)
CIT_FX<-filter(CITGIS,par_cod_siu %in% ESTACAO_SIU$Código.SIU)
CIT_FX<-merge(CIT_FX,ESTACAO_SIU,by.x="par_cod_siu",by.y="Código.SIU")
CIT_FX$FX<-str_sub(CIT_FX$horario_passagem,start=-8,end=-7)
CIT_FX$FX<-as.numeric(CIT_FX$FX)
EMBARQUES$CODIGO_LINHA_CORR<-gsub(1001,1000,EMBARQUES$CODIGO_LINHA)
EMBARQUES$CODIGO_LINHA_CORR<-gsub(1002,1000,EMBARQUES$CODIGO_LINHA_CORR)
EMBARQUES$CODIGO_LINHA_CORR<-gsub(2001,2000,EMBARQUES$CODIGO_LINHA_CORR)
EMBARQUES$CODIGO_LINHA_CORR<-gsub(2002,2000,EMBARQUES$CODIGO_LINHA_CORR)
EMBARQUES$CODIGO_LINHA_CORR<-gsub(3001,3000,EMBARQUES$CODIGO_LINHA_CORR)
EMBARQUES$CODIGO_LINHA_CORR<-gsub(3002,3000,EMBARQUES$CODIGO_LINHA_CORR)
EMBARQUES$CODIGO_LINHA_CORR<-gsub(4002,4001,EMBARQUES$CODIGO_LINHA_CORR)
EMBARQUES$CODIGO_LINHA_CORR<-gsub(4003,4001,EMBARQUES$CODIGO_LINHA_CORR)
EMBARQUES$CODIGO_LINHA_CORR<-gsub(4004,4001,EMBARQUES$CODIGO_LINHA_CORR)
EMBARQUES$CODIGO_LINHA_CORR<-gsub(6001,6000,EMBARQUES$CODIGO_LINHA_CORR)
EMBARQUES$CODIGO_LINHA_CORR<-gsub(7001,7000,EMBARQUES$CODIGO_LINHA_CORR)

#ATRIBUINDO PED DE DESEMBARQUE

EMBARQUES<-filter(EMBARQUES,is.na(EMBARQUES$X_DESTINO)==F)
EMBARQUES<-filter(EMBARQUES,is.na(EMBARQUES$Coord..X.x)==F)
CIT_FX$Lin_Sub_PC<-str_c(CIT_FX$sub_lin_sg_linha,CIT_FX$PC,sep="-")
PED_PROXIMO$Lin_Sub_PC<-str_c(PED_PROXIMO$Linha,PED_PROXIMO$Sublinha,PED_PROXIMO$PC,sep="-")
ESTACAO_FILTRO<-ESTACOES_LINHAS
PED_Op<-0
Exceto_PED<-0
i<-1
n<-0


for(i in 1:nrow(EMBARQUES)){
  if(EMBARQUES$Cod.Viagem[i]==1){ 
    ESTACAO_FILTRO<-filter(CIT_FX,Cód_Est==EMBARQUES$CODIGO_LINHA_CORR[i])
    ESTACAO_FILTRO<-filter(ESTACAO_FILTRO,FX<=EMBARQUES$FX_EMB[i]+1,horario_passagem>EMBARQUES$Hora[i])
    Exceto_PED<-filter(PED_PROXIMO,Lin_Sub_PC %in% ESTACAO_FILTRO$Lin_Sub_PC)
  }else{
    Exceto_PED<-filter(PED_PROXIMO,LIN_SUB_PC_EMB==EMBARQUES$LIN_SUB_PC_EMB.x[i],Sentido==EMBARQUES$Sentido[i])
  }
  n<-nrow(Exceto_PED)
  if(n>0){
    PED_Op[i]<-procura_ped_proximo(EMBARQUES$X_DESTINO[i],EMBARQUES$Y_DESTINO[i],Exceto_PED$Coord..X,Exceto_PED$Coord..Y)
    EMBARQUES$Lin_PED_Oposto[i]<-Exceto_PED$Linha[PED_Op[i]]
    EMBARQUES$Código.SIU.Oposto[i]<-Exceto_PED$Código.SIU[PED_Op[i]]
    EMBARQUES$Coord..X.Oposto[i]<-Exceto_PED$Coord..X[PED_Op[i]]
    EMBARQUES$Coord..Y.Oposto[i]<-Exceto_PED$Coord..Y[PED_Op[i]] 
  }else{
    EMBARQUES$Lin_PED_Oposto[i]<-"PROBLEMA"
    EMBARQUES$Código.SIU.Oposto[i]<-"PROBLEMA"
    EMBARQUES$Coord..X.Oposto[i]<-"PROBLEMA"
    EMBARQUES$Coord..Y.Oposto[i]<-"PROBLEMA"
  }
}

EMBARQUES_PROBLEMA<-filter(EMBARQUES,EMBARQUES$Lin_PED_Oposto=='PROBLEMA')
EMBARQUES$DESLOC<-sqrt(((as.numeric(EMBARQUES$X_DESTINO)-as.numeric(EMBARQUES$Coord..X.Oposto))^2)+(EMBARQUES$X_DESTINO-EMBARQUES$Coord..X.Oposto)^2)
EMBARQUES_OP600m<-filter(EMBARQUES,DESLOC>600)
EMBARQUES<-filter(EMBARQUES,DESLOC<=600)

#COLETANDO SEQUENCIA DO PED
SUBLINHA_VS_PED$Ord<-c(1:nrow(SUBLINHA_VS_PED))
SUBLINHA_VS_PED$LIN_SUB_PC_SIU<-str_c(SUBLINHA_VS_PED$Linha,SUBLINHA_VS_PED$Sublinha,SUBLINHA_VS_PED$PC,SUBLINHA_VS_PED$Código.SIU,sep='-')
SEQ_PED<-SUBLINHA_VS_PED %>% dplyr::select(LIN_SUB_PC_SIU,Ord)
EMBARQUES<-merge(EMBARQUES,SEQ_PED,by.x='LIN_SUB_PC_EMB_SIU',by.y='LIN_SUB_PC_SIU',all.x=T)
EMBARQUES$Sublinha.x<-gsub(l,'',EMBARQUES$Sublinha.x)
EMBARQUES$Sublinha.x<-gsub('-','',EMBARQUES$Sublinha.x)
EMBARQUES$Sublinha.x<-as.numeric(EMBARQUES$Sublinha.x)
EMBARQUES$LIN_SUB_PC_DES<-str_c(EMBARQUES$CODIGO_LINHA,EMBARQUES$Sublinha.x,EMBARQUES$PC.x,EMBARQUES$Código.SIU.Oposto,sep='-')
EMBARQUES<-merge(EMBARQUES,SEQ_PED,by.x='LIN_SUB_PC_DES',by.y='LIN_SUB_PC_SIU',all.x=T)
EMBARQUES<-filter(EMBARQUES,Ord.x<=Ord.y)

#TRABALHANDO COM EXPANS?O
#FE1
QTUo<-EMB
QTUo$VG_SIU<-str_c(QTUo$Cod.Viagem,QTUo$SIU)
QTUo$COUNT<-1
QTUo<-QTUo %>% dplyr::select(VG_SIU,COUNT) %>% dplyr::group_by(VG_SIU) %>% dplyr::summarise(QTUo=(sum(COUNT)))
QTUod<-EMBARQUES
QTUod$COUNT<-1
QTUod$VG_SIU<-str_c(QTUod$Cod.Viagem,QTUod$SIU)
QTUod<-QTUod %>% dplyr::select(VG_SIU,COUNT) %>% dplyr::group_by(VG_SIU) %>% dplyr::summarise(QTUod=(sum(COUNT)))
FE1<-merge(QTUo,QTUod,all.x=T)
FE1$QTUod[is.na(FE1$QTUod)]<-1
FE1$FE1<-FE1$QTUo/FE1$QTUod
rm(QTUo,QTUod)

EMBARQUES$VG_SIU<-str_c(EMBARQUES$Cod.Viagem,EMBARQUES$SIU)
EMBARQUES<-merge(EMBARQUES,FE1)

#FE2
QTU<-EMB
QTU$COUNT<-1
QTU<-QTU %>% dplyr::select(Cod.Viagem,COUNT) %>% dplyr::group_by(Cod.Viagem) %>% dplyr::summarise(QTU=sum(COUNT)) 
FE1<-EMBARQUES %>% dplyr::select(Cod.Viagem,FE1) %>% dplyr::group_by(Cod.Viagem) %>% dplyr::summarise(sumFE1=sum(FE1))
sumFE1<-merge(QTU,FE1)
sumFE1$fat<-sumFE1$QTU/sumFE1$sumFE1
sumFE1[,2:3]<-NULL
EMBARQUES<-merge(EMBARQUES,sumFE1,all.x=T)
EMBARQUES$FE2<-EMBARQUES$FE1*EMBARQUES$fat
rm(FE1,sumFE1,QTU)

#SOBE E DESCE
SOBE<-EMBARQUES %>% dplyr::select(Cod.Viagem,Ord.x,FE2) %>% dplyr::group_by(Cod.Viagem,Ord.x) %>% dplyr::summarise(SOBE=sum(FE2))
DESCE<-EMBARQUES %>% dplyr::select(Cod.Viagem,Ord.y,FE2) %>% dplyr::group_by(Cod.Viagem,Ord.y) %>% dplyr::summarise(DESCE=sum(FE2))
colnames(DESCE)<-c("Cod.Viagem","Ord.x","DESCE") 
DESCE$CHAVE<-str_c(DESCE$Cod.Viagem,DESCE$Ord.x)
SOBE$CHAVE<-str_c(SOBE$Cod.Viagem,SOBE$Ord.x)
SD<-merge(SOBE,DESCE,all.x=T)
DESCE<-filter(DESCE,!CHAVE %in% SOBE$CHAVE)
DESCE$SOBE<-0
SD<-rbind(DESCE %>% dplyr::select(colnames(SD)),SD)
SD$CHAVE<-as.numeric(SD$CHAVE)
SD<-arrange(SD,CHAVE)
SD[is.na(SD)]<-0
SD$OCUP<-0

for(j in 1:nrow(SD)){
  if(j==1){
    SD$OCUP[j]<-SD$SOBE[j]
  }else if(SD$Cod.Viagem[j]==SD$Cod.Viagem[j-1]){
    SD$OCUP[j]<-SD$OCUP[j-1]+SD$SOBE[j]-SD$DESCE[j]
  }else{
    SD$OCUP[j]<-SD$SOBE[j]
  }
}
SD$OCUP<-round(SD$OCUP,6)
rm(SOBE,DESCE)

#ENDEREÇO PED
SD<-arrange(merge(SD,SUBLINHA_VS_PED %>% dplyr::select(Ord,Código.SIU,Endereço.do.Ponto,Nº.Imóvel),by.x='Ord.x',by.y='Ord'),CHAVE)
SD<-arrange(merge(SD,VIAGENS %>% dplyr::select('Data.Prog.','Sigla.Linha',"Sentido","Cód..Viagem","Hora.Abertura.da.Viagem"),by.x='Cod.Viagem',by.y='Cód..Viagem'),CHAVE)
IR<-SD %>% dplyr::select(Cod.Viagem,SOBE,OCUP) %>% dplyr::group_by(Cod.Viagem) %>% summarise(PTr=sum(SOBE),PTC=max(OCUP))
SD<-arrange(merge(SD,IR),CHAVE)
rm(IR)
SD$IR=SD$PTr/SD$PTC
SD<-SD %>% dplyr::select(Data.Prog.,Sigla.Linha,Sentido,Cod.Viagem,Hora.Abertura.da.Viagem,Ord.x,Código.SIU,Endereço.do.Ponto,Nº.Imóvel,SOBE,DESCE,OCUP,PTr,PTC,IR)

#SALVANDO ARQUIVOS FINAIS
write.csv2(SD,str_c(l,'_Sobe_e_Desce.csv'),row.names = F,fileEncoding="latin1")
write.csv2(EMBARQUES,str_c(l,'_microdados.csv'),row.names = F)