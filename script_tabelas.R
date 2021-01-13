library(tidyverse)
library(readxl)

filtro_setor <- read_xlsx('Dados2/setor.xlsx')
banco_filtrado <- 
  read_xlsx('Dados2/bancotratado.xlsx') %>% filter(str_detect(Fornecedor,"GRUPO FR - ")) %>%
  mutate(Empresas=str_remove_all(Fornecedor,"GRUPO FR - ")) %>%
  mutate(
    Empresas=case_when(
      str_detect(Empresas,'CEF|BANCO CAIXA') ~ 'CAIXA',
      str_detect(Empresas, 'BANCO BRADESCO') ~ 'BRADESCO',
      str_detect(Empresas,'SUBMARINO|SHOPTIME|AMERICANAS.COM') ~ 'B2W',
      str_detect(Empresas, 'CASAS BAHIA|PONTO FRIO') ~ 'VIA VAREJO',
      str_detect(Empresas,'BANCO OMNI') ~ 'OMNI FINANCEIRA',
      str_detect(Empresas,'RIACHUELO') ~ 'RIACHUELO MIDWAY FINANCEIRA',
      TRUE ~ Empresas
    )
  )%>%
  left_join(filtro_setor) %>% filter(!is.na(Setor))




banco <- read_xlsx('Dados2/bancotratado.xlsx')













#########tabelas gerais##
tabela6 <- banco %>% group_by(ano) %>% summarise(`Cursos Ofertados`=toString(unique(Curso)),Ofertas=length(unique(Oferta))) %>% janitor::adorn_totals()


tabela7 <- banco %>% group_by(ano,Curso) %>%
  summarise(Ofertas=length(unique(Oferta))) %>%
  pivot_wider(names_from = ano,values_from=Ofertas) %>%
  janitor::adorn_totals(where="col")

tabela8 <- banco %>% group_by(ano,Curso) %>%
  summarise(n=sum(as.numeric(str_trim(Total)),na.rm=T)) %>% 
  pivot_wider(names_from = ano,values_from=n) %>% janitor::adorn_totals(where=c('row','col'))

tabela9 <- banco_filtrado %>% group_by(ano,Curso) %>%
  summarise(n=sum(as.numeric(str_trim(Total)),na.rm=T)) %>% 
  pivot_wider(names_from = ano,values_from=n) %>% janitor::adorn_totals(where=c('row','col'))

tabela10 <- banco_filtrado %>% group_by(Setor) %>%
  summarise(Matrículas=sum(as.numeric(str_trim(Total)),na.rm=T))

tabela11 <- banco %>% group_by(ano) %>% summarise(`Aprovados`=sum(as.numeric(str_trim(Aprovado)))) %>% janitor::adorn_totals()

tabela12 <-  banco %>% group_by(Curso) %>%
  summarise(Aprovados=sum(as.numeric(str_trim(Aprovado))),
            Percentual=sum(as.numeric(str_trim(Total)))) %>%
  janitor::adorn_totals() %>% mutate(Percentual=paste0(Percentual*100/Aprovados,'%'))

tabela13 <- banco_filtrado %>% group_by(ano) %>% summarise(`Aprovados`=sum(as.numeric(str_trim(Aprovado)))) %>% janitor::adorn_totals()


tabela14 <-  banco_filtrado %>% group_by(Curso) %>%
  summarise(Aprovados=sum(as.numeric(str_trim(Aprovado))),
            Percentual=sum(as.numeric(str_trim(Total)))) %>%
  janitor::adorn_totals() %>% mutate(Percentual=paste0(Percentual*100/Aprovados,'%'))


tabela15 <- banco_filtrado %>% group_by(Setor) %>%
  summarise(Aprovados=sum(as.numeric(str_trim(Aprovado))),
            Percentual=sum(as.numeric(str_trim(Total)))) %>%
  janitor::adorn_totals() %>% mutate(Percentual=paste0(Percentual*100/Aprovados,'%'))


# list(tabela7,tabela8,tabela9,tabela10,tabela11,tabela12,tabela13,tabela14) %>% openxlsx::write.xlsx('tabelas_provisorio.xlsx')
# 
# tabela3 <- banco %>% group_by(ano) %>% summarise(Ofertas=length(unique(Oferta))) %>% janitor::adorn_totals() %>% pivot_wider(names_from = ano,values_from=Ofertas)
# 
# 
# tabela4 <- banco %>% group_by(ano) %>% summarise(Ofertas=sum(as.numeric(str_trim(Total)))) %>% janitor::adorn_totals() %>% pivot_wider(names_from = ano,values_from=Ofertas)
# 
# 
# tabela5 <- banco %>% group_by(ano,Curso) %>%
#   summarise(n=sum(as.numeric(str_trim(Aprovado)),na.rm=T)) %>% 
#   pivot_wider(names_from = ano,values_from=n) %>% janitor::adorn_totals()


########tratamento reacao##########

reacao_dir <- list.files('Dados/',full.names = T)[str_detect(list.files('Dados/',full.names = T),'satisfação')]

nome_fix <- names(read_xlsx(reacao_dir[1]))
reacao <-data.frame() 

for(i in reacao_dir){
  import <- read_xlsx(i)
  reacao <- bind_rows(reacao,import)
}
interesse <-
  c(
    "Assimilação do conteúdo do curso",
    "Capacidade de aplicar o conhecimento ensinado no curso em diferentes situações",
    "Capacidade de transmitir os conhecimentos adquiridos no curso a outras pessoas",
    "Conciliação do curso com minhas atividades profissionais.",
    "Disponibilidade de computador nos horários que tenho para estudar."
  )

tabela16 <-
  reacao %>% select(interesse) %>% summarise(
    across(everything(),  ~ mean(as.numeric(.))),
    Média = reacao %>% select(interesse) %>% unlist() %>% as.numeric() %>% mean()
  )




#tabela17 <-
analise <- 
  left_join(reacao %>% select(Oferta,"Assimilação do conteúdo do curso"),banco %>% select(Curso,Oferta) %>% unique())
