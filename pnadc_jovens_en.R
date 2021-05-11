library("PNADcIBGE")
library("survey")
library("tidyverse")
library(writexl)


#baixando dados
PNADc <- get_pnadc(year = 2019, interview = 1)

#atividades economicas
atividade<-c('Agricultura, pecuária, produção florestal, pesca e aquicultura', 
             'Indústria geral',
             'Construção',
             'Comércio, reparação de veículos automotores e motocicletas',
             'Transporte, armazenagem e correio ',
             'Alojamento e alimentação ',
             'Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas',             
             'Administração pública, defesa e seguridade social ',
             'Educação, saúde humana e serviços sociais',
             'Outros Serviços',
             'Serviços domésticos')

#gerando df para armazenar os dados
trab_e<-data.frame()


#looping para pegar quem estuda e trabalha
for (j in atividade) {
    
    df<-svyby(~interaction(V3003A,VD4009),~V2009,
                   subset(PNADc,UF=="Alagoas"&V2009>=15&V2009<=29
                            &VD4010==j),
                          svytotal,na.rm=T)
    
    df$atividade<-j

    
    trab_e<-rbind(trab_e,df)
    
}
  
#desocupadas e estudam
desoc_e<-svyby(~interaction(VD4002,V3003A),~V2009,
               subset(PNADc,UF=="Alagoas"&V2009>=15&V2009<=29&
                                       V3002=="Sim"),svytotal,na.rm=T)

#fora da forca e estudam 
ff_e<-svyby(~interaction(V3003A,VD4030),~V2009,
                   subset(PNADc,UF=="Alagoas"&V2009>=15&V2009<=29),
                   svytotal,na.rm=T)


#nao estudam e trabalham
trab_n<-data.frame()

memory.limit(9999999999)

for (j in atividade) {
  
  df<-svyby(~interaction(V3009A,VD4009),~V2009,
            subset(PNADc,UF=="Alagoas"&V2009>=15&V2009<=29
                   &VD4010==j),
            svytotal,na.rm=T)
  
  df$atividade<-j
  
  
  trab_n<-rbind(trab_n,df)
  
}

#fora da forca e nao estudam
ff_n<-svyby(~interaction(V3009A,VD4030),~V2009,
                   subset(PNADc,UF=="Alagoas"&V2009>=15&V2009<=29&V3002=="Não"),
                   svytotal,na.rm=T)

#desocupadas e nao estudam
desoc_n<-svyby(~interaction(VD4002,V3009A),~V2009,
               subset(PNADc,UF=="Alagoas"&V2009>=15&V2009<=29&
                                         V3002=="Não"),svytotal,na.rm=T)

write_xlsx(desoc_e,'desoc_e.xlsx')
write_xlsx(desoc_n,'desoc_n.xlsx')
write_xlsx(ff_e,'ff_e.xlsx')
write_xlsx(ff_n,'ff_n.xlsx')
write_xlsx(trab_e,'trab_e.xlsx')
write_xlsx(trab_n,'trab_n.xlsx')
