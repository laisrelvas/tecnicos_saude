# CARREGAR BIBLIOTECAS E BASE ####
library(data.table)
library(tidyverse)

setwd("C:/Users/laisr/OneDrive/Documentos/Área de Trabalho/CENSO ESCOLAR/analise/github/tecnicos_saude/dados")
escolas <- read.table("escolas.csv", sep="|", header=T)

# SELCIONA VARIÁVEIS DE INTERESSE PARA O ESTUDO NA BASE DE ESCOLAS
escolas <- escolas %>% 
  select(CO_ENTIDADE, TP_SITUACAO_FUNCIONAMENTO, CO_UF, CO_REGIAO, TP_DEPENDENCIA, TP_LOCALIZACAO, IN_VINCULO_SECRETARIA_EDUCACAO, IN_VINCULO_SEGURANCA_PUBLICA, 
         IN_VINCULO_SECRETARIA_SAUDE, IN_VINCULO_OUTRO_ORGAO, TP_REGULAMENTACAO, IN_COMPUTADOR, IN_DESKTOP_ALUNO, IN_COMP_PORTATIL_ALUNO,
         IN_TABLET_ALUNO, IN_INTERNET, IN_INTERNET_ALUNOS, IN_INTERNET_ADMINISTRATIVO, IN_INTERNET_APRENDIZAGEM, IN_INTERNET_COMUNIDADE,
         IN_ACESSO_INTERNET_COMPUTADOR, IN_ACES_INTERNET_DISP_PESSOAIS, TP_REDE_LOCAL, IN_BANDA_LARGA, IN_PROFISSIONALIZANTE, IN_COMUM_PROF, IN_ESP_EXCLUSIVA_PROF)

# CURADORIA DOS DADOS ####
 
escolas <- escolas %>% 
  # ATRIBUI NOMES AOS ESTADOS
  mutate(CO_UF=case_when(CO_UF==12 ~ "ACRE",               CO_UF==27 ~ "ALAGOAS", 
                         CO_UF==16 ~ "AMAPÁ",              CO_UF==13 ~ "AMAZONAS", 
                         CO_UF==29 ~ "BAHIA",              CO_UF==23 ~ "CEARÁ",
                         CO_UF==53 ~ "DISTRITO FEDERAL",   CO_UF==32 ~ "ESPÍRITO SANTO", 
                         CO_UF==52 ~ "GOIÁS",              CO_UF==21 ~ "MARANHÃO", 
                         CO_UF==51 ~ "MATO GROSSO",        CO_UF==50 ~ "MATO GROSSO DO SUL", 
                         CO_UF==31 ~ "MINAS GERAIS",       CO_UF==15 ~ "PARÁ", 
                         CO_UF==25 ~ "PARAÍBA",            CO_UF==41 ~ "PARANÁ", 
                         CO_UF==26 ~ "PERNAMBUCO",         CO_UF==22 ~ "PIAUÍ", 
                         CO_UF==33 ~ "RIO DE JANEIRO",     CO_UF==24 ~ "RIO GRANDE DO NORTE", 
                         CO_UF==43 ~ "RIO GRANDE DO SUL",  CO_UF==11 ~ "RONDÔNIA",           
                         CO_UF==14 ~ "RORAIMA",            CO_UF==42 ~ "SANTA CATARINA",  
                         CO_UF==35 ~ "SÃO PAULO",          CO_UF==28 ~ "SERGIPE", 
                         CO_UF==17 ~ "TOCANTINS", 
                         TRUE ~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE SITUAÇÃO DE FUNCIONAMENTO
  mutate(TP_SITUACAO_FUNCIONAMENTO = case_when(TP_SITUACAO_FUNCIONAMENTO==1 ~ "Em Atividade", 
                                               TP_SITUACAO_FUNCIONAMENTO==2 ~ "Paralisada", 
                                               TP_SITUACAO_FUNCIONAMENTO==3 ~ "Extinta (ano do Censo)", 
                                               TP_SITUACAO_FUNCIONAMENTO==4 ~ "Extinta em Anos Anteriores", 
                                               TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE DEPENDÊNCIA 
  mutate(TP_DEPENDENCIA = case_when(TP_DEPENDENCIA==1 ~ "Federal",
                                     TP_DEPENDENCIA==2 ~ "Estadual",
                                     TP_DEPENDENCIA==3 ~ "Municipal",
                                     TP_DEPENDENCIA==4 ~ "Privada",
                                    TRUE ~ "Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE LOCALIZAÇÃO
  mutate(TP_LOCALIZACAO= case_when(TP_LOCALIZACAO==1	~"Urbana", 
                                   TP_LOCALIZACAO==2	~"Rural",
                                   TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE SITUAÇÃO DE FUNCIONAMENTO
  mutate(IN_VINCULO_SECRETARIA_EDUCACAO= case_when(IN_VINCULO_SECRETARIA_EDUCACAO==0	~"Não", 
                                                   IN_VINCULO_SECRETARIA_EDUCACAO==1	~"Sim",
                                                   TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_VINCULO_SEGURANCA_PUBLICA= case_when(IN_VINCULO_SEGURANCA_PUBLICA==0	~"Não", 
                                                 IN_VINCULO_SEGURANCA_PUBLICA==1	~"Sim", 
                                                 TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_VINCULO_SECRETARIA_SAUDE= case_when(IN_VINCULO_SECRETARIA_SAUDE==0	~"Não", 
                                                IN_VINCULO_SECRETARIA_SAUDE==1	~"Sim", 
                                                TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_VINCULO_OUTRO_ORGAO= case_when(IN_VINCULO_OUTRO_ORGAO==0	~"Não", 
                                           IN_VINCULO_OUTRO_ORGAO==1	~"Sim", 
                                           TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(TP_REGULAMENTACAO= case_when(TP_REGULAMENTACAO==0	~"Não", 
                                      TP_REGULAMENTACAO==1	~"Sim", 
                                      TP_REGULAMENTACAO==2~"Em tramitação", 
                                      TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_COMPUTADOR= case_when(IN_COMPUTADOR==0	~"Não", 
                                  IN_COMPUTADOR==1	~"Sim", 
                                  TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_DESKTOP_ALUNO= case_when(IN_DESKTOP_ALUNO==0	~"Não", 
                                     IN_DESKTOP_ALUNO==1	~"Sim", 
                                     TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_COMP_PORTATIL_ALUNO= case_when(IN_COMP_PORTATIL_ALUNO==0	~"Não", 
                                           IN_COMP_PORTATIL_ALUNO==1	~"Sim", 
                                           TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_TABLET_ALUNO= case_when(IN_TABLET_ALUNO==0	~"Não", 
                                    IN_TABLET_ALUNO==1	~"Sim", 
                                    TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_INTERNET= case_when(IN_INTERNET==0	~"Não", 
                                IN_INTERNET==1	~"Sim", 
                                TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_INTERNET_ALUNOS= case_when(IN_INTERNET_ALUNOS==0	~"Não", 
                                       IN_INTERNET_ALUNOS==1	~"Sim", 
                                       TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_INTERNET_ADMINISTRATIVO= case_when(IN_INTERNET_ADMINISTRATIVO==0	~"Não", 
                                               IN_INTERNET_ADMINISTRATIVO==1	~"Sim", 
                                               TRUE~"Ignorado")) %>%
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_INTERNET_APRENDIZAGEM= case_when(IN_INTERNET_APRENDIZAGEM==0	~"Não", 
                                             IN_INTERNET_APRENDIZAGEM==1	~"Sim", 
                                             TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_INTERNET_COMUNIDADE= case_when(IN_INTERNET_COMUNIDADE==0	~"Não", 
                                           IN_INTERNET_COMUNIDADE==1	~"Sim", 
                                           TRUE~"Ignorado")) %>% 
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_ACESSO_INTERNET_COMPUTADOR= case_when(IN_ACESSO_INTERNET_COMPUTADOR==0	~"Não", 
                                                  IN_ACESSO_INTERNET_COMPUTADOR==1	~"Sim", 
                                                  TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_ACES_INTERNET_DISP_PESSOAIS= case_when(IN_ACES_INTERNET_DISP_PESSOAIS==0	~"Não", 
                                                   IN_ACES_INTERNET_DISP_PESSOAIS==1	~"Sim", 
                                                   TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(TP_REDE_LOCAL= case_when(TP_REDE_LOCAL==0	~"Não há rede local interligando computadores", 
                                  TP_REDE_LOCAL==1	~"A cabo", 
                                  TP_REDE_LOCAL==2	~"Wireless", 
                                  TP_REDE_LOCAL==1	~"A cabo e wireless", 
                                  TRUE~"Ignorado")) %>% 
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate_at(c("IN_PROFISSIONALIZANTE", "IN_COMUM_PROF", "IN_ESP_EXCLUSIVA_PROF"), 
            ~case_when( . == 0 ~ "Não", 
                        . == 1 ~ "Sim", 
                          TRUE ~ "Ignorado")) %>%
  
  # ATRIBUI LABEL AOS CÓDIGOS DE 
  mutate(IN_BANDA_LARGA = case_when(IN_BANDA_LARGA==1~"Sim", 
                                    IN_BANDA_LARGA==0~"Não", 
                                    TRUE~"Ignorado"))

# FILTRA ESCOLAS ESCOLAS QUE TÊM CURSOS TÉCNICOS DA SAÚDE EM CADA ESTADO ####  
  # RONDÔNIA  
    # VINCULA A BASE CURADA DE ESCOLAS COM LISTAS DE IDS DAS TURMAS COM OS CURSOS TÉCNICOS DA SAÚDE PARA SELECIONAR AS ESCOLAS EM CADA ESTADO 
    # CRIA OBJETOS TEMPORÁRIO (TOTAL E POR CURSO)
      escolas_RO <- left_join (id_turmas_RO, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_RO_anal_cli<- left_join(id_turmas_RO_anal_cli, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_RO_enf <- left_join(id_turmas_RO_enf, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_RO_nut <- left_join(id_turmas_RO_nut, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_RO_prot<- left_join(id_turmas_RO_prot, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_RO_rad<- left_join(id_turmas_RO_rad, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
    # UNIFICA OBJETOS TEMPORÁRIOS (POR CURSO)
      escolas_RO_long = rbind(escolas_RO_anal_cli,escolas_RO_enf,escolas_RO_nut, escolas_RO_prot, escolas_RO_rad)
    # REMOVE OBJETOS TEMPORÁRIOS
      rm(escolas_RO_anal_cli,escolas_RO_enf,escolas_RO_nut, escolas_RO_prot, escolas_RO_rad, id_turmas_RO, id_turmas_RO_anal_cli, 
       id_turmas_RO_enf, id_turmas_RO_nut, id_turmas_RO_prot, id_turmas_RO_rad)

  # MARANHÃO  
    # VINCULA A BASE CURADA DE ESCOLAS COM LISTAS DE IDS DAS TURMAS COM OS CURSOS TÉCNICOS DA SAÚDE PARA SELECIONAR AS ESCOLAS EM CADA ESTADO 
    # CRIA OBJETOS TEMPORÁRIO (TOTAL E POR CURSO)
      escolas_MA <- left_join (id_turmas_MA, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_MA_anal_cli<- left_join(id_turmas_MA_anal_cli, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_MA_enf<- left_join(id_turmas_MA_enf, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_MA_equip_biom<- left_join(id_turmas_MA_equip_biom, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_MA_far <- left_join(id_turmas_MA_far, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_MA_ger_sau <- left_join(id_turmas_MA_ger_sau, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_MA_nut<- left_join(id_turmas_MA_nut, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_MA_radio<- left_join(id_turmas_MA_radio, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_MA_reg<- left_join(id_turmas_MA_reg, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_MA_sbucal<- left_join(id_turmas_MA_sbucal, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
    # UNIFICA OBJETOS TEMPORÁRIOS (POR CURSO)
      escolas_MA_long <- rbind(escolas_MA_anal_cli, escolas_MA_enf, escolas_MA_equip_biom, escolas_MA_far, escolas_MA_ger_sau,
                               escolas_MA_nut, escolas_MA_radio, escolas_MA_reg, escolas_MA_sbucal)
    # REMOVE OBJETOS TEMPORÁRIOS  
      rm(escolas_MA_anal_cli, escolas_MA_enf, escolas_MA_equip_biom, escolas_MA_far, escolas_MA_ger_sau,
         escolas_MA_nut, escolas_MA_radio, escolas_MA_reg, escolas_MA_sbucal, id_turmas_MA, id_turmas_MA_anal_cli, id_turmas_MA_enf,
         id_turmas_MA_equip_biom, id_turmas_MA_far, id_turmas_MA_ger_sau, id_turmas_MA_nut, id_turmas_MA_radio, id_turmas_MA_reg, 
         id_turmas_MA_sbucal)
  
  # GOIÁS  
    # VINCULA A BASE CURADA DE ESCOLAS COM LISTAS DE IDS DAS TURMAS COM OS CURSOS TÉCNICOS DA SAÚDE PARA SELECIONAR AS ESCOLAS EM CADA ESTADO 
    # CRIA OBJETOS TEMPORÁRIO (TOTAL E POR CURSO)
      escolas_GO <- left_join (id_turmas_GO, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_GO_enf <- left_join (id_turmas_GO_enf, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_GO_anal_cli <- left_join (id_turmas_GO_anal_cli, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_GO_radio <- left_join (id_turmas_GO_radio, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_GO_sbucal <- left_join (id_turmas_GO_sbucal, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_GO_nut <- left_join (id_turmas_GO_nut, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_GO_prot <- left_join (id_turmas_GO_prot, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_GO_vig <- left_join (id_turmas_GO_vig, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_GO_necro <- left_join (id_turmas_GO_necro, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_GO_far <- left_join (id_turmas_GO_far, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
    # UNIFICA OBJETOS TEMPORÁRIOS (POR CURSO)
      escolas_GO_long <- rbind(escolas_GO_enf, escolas_GO_anal_cli, escolas_GO_radio, escolas_GO_sbucal,
                               escolas_GO_nut, escolas_GO_prot, escolas_GO_vig, escolas_GO_necro, escolas_GO_far)
    # REMOVE OBJETOS TEMPORÁRIOS  
      rm(escolas_GO_enf, escolas_GO_anal_cli, escolas_GO_radio, escolas_GO_sbucal,
         escolas_GO_nut, escolas_GO_prot, escolas_GO_vig, escolas_GO_necro, escolas_GO_far, 
         id_turmas_GO, id_turmas_GO_anal_cli, id_turmas_GO_enf, id_turmas_GO_far, id_turmas_GO_necro,
         id_turmas_GO_nut, id_turmas_GO_prot, id_turmas_GO_radio, id_turmas_GO_sbucal, id_turmas_GO_vig)
      
  # RIO GRANDE DO SUL  
    # VINCULA A BASE CURADA DE ESCOLAS COM LISTAS DE IDS DAS TURMAS COM OS CURSOS TÉCNICOS DA SAÚDE PARA SELECIONAR AS ESCOLAS EM CADA ESTADO 
    # CRIA OBJETOS TEMPORÁRIO (TOTAL E POR CURSO)
      escolas_RS <- left_join (id_turmas_RS, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_RS_enf<- left_join (id_turmas_RS_enf, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_RS_radio <- left_join (id_turmas_RS_radio, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_RS_nut <- left_join (id_turmas_RS_nut, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_RS_anal_cli <- left_join (id_turmas_RS_anal_cli, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_RS_prot <- left_join (id_turmas_RS_prot, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_RS_far <- left_join (id_turmas_RS_far, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_RS_ger <- left_join (id_turmas_RS_ger, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_RS_mass <- left_join (id_turmas_RS_mass, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_RS_sbucal <- left_join (id_turmas_RS_sbucal, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_RS_cuidoso <- left_join (id_turmas_RS_cuidoso, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_RS_necro <- left_join (id_turmas_RS_necro, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
    # UNIFICA OBJETOS TEMPORÁRIOS (POR CURSO)
      escolas_RS_long <- rbind(escolas_RS_enf, escolas_RS_radio, escolas_RS_nut, escolas_RS_anal_cli, escolas_RS_prot, 
                               escolas_RS_far, escolas_RS_ger, escolas_RS_mass, escolas_RS_sbucal, escolas_RS_cuidoso,
                               escolas_RS_necro )
    # REMOVE OBJETOS TEMPORÁRIOS  
      rm(escolas_RS_enf, escolas_RS_radio, escolas_RS_nut, escolas_RS_anal_cli, escolas_RS_prot,
         escolas_RS_far, escolas_RS_ger, escolas_RS_mass, escolas_RS_sbucal, escolas_RS_cuidoso,
         escolas_RS_necro ,id_turmas_RS, id_turmas_RS_anal_cli, id_turmas_RS_cuidoso, id_turmas_RS_enf, id_turmas_RS_far,
         id_turmas_RS_ger, id_turmas_RS_mass, id_turmas_RS_necro, id_turmas_RS_nut, id_turmas_RS_prot, id_turmas_RS_radio, id_turmas_RS_sbucal)
      
      
  # PERNAMBUCO  
    # VINCULA A BASE CURADA DE ESCOLAS COM LISTAS DE IDS DAS TURMAS COM OS CURSOS TÉCNICOS DA SAÚDE PARA SELECIONAR AS ESCOLAS EM CADA ESTADO 
    # CRIA OBJETOS TEMPORÁRIO (TOTAL E POR CURSO)
      escolas_PE <- left_join (id_turmas_PE, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) 
      escolas_PE_enf <- left_join (id_turmas_PE_enf, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_PE_radio <- left_join (id_turmas_PE_radio, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_PE_anal_cli <- left_join (id_turmas_PE_anal_cli, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_PE_nut <- left_join (id_turmas_PE_nut, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_PE_far <- left_join (id_turmas_PE_far, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_PE_sbucal <- left_join (id_turmas_PE_sbucal, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_PE_vig <- left_join (id_turmas_PE_vig, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_PE_ger <- left_join (id_turmas_PE_ger, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
      escolas_PE_hemo <- left_join (id_turmas_PE_hemo, escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE"))
    # UNIFICA OBJETOS TEMPORÁRIOS (POR CURSO)
      escolas_PE_long <- rbind(escolas_PE_enf, escolas_PE_radio, escolas_PE_anal_cli, escolas_PE_nut, 
                               escolas_PE_far, escolas_PE_sbucal, escolas_PE_vig, escolas_PE_ger, escolas_PE_hemo)
      # REMOVE OBJETOS TEMPORÁRIOS  
      rm(escolas_PE_enf, escolas_PE_radio, escolas_PE_anal_cli, escolas_PE_nut, 
         escolas_PE_far, escolas_PE_sbucal, escolas_PE_vig, escolas_PE_ger, escolas_PE_hemo, id_turmas_PE, id_turmas_PE_anal_cli, id_turmas_PE_enf, 
         id_turmas_PE_far, id_turmas_PE_ger, id_turmas_PE_hemo, id_turmas_PE_nut, id_turmas_PE_radio, id_turmas_PE_sbucal, id_turmas_PE_vig)
      
# TABELA - ESCOLAS BRASIL (QUALQUER ESCOLA, INDEPENDENTE DE TER CURSO TÉCNICO DA SAÚDE)####      
tb_escolas <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas %>% count(var = TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Órgão que a escola pública está vinculada (n=)", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Disponibilidade de computadores", `n`=NA, `prop`=NA) %>%
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  add_row(var = "Desktop (Computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  #add_row(var = "Quantidade de computadores em uso pelos alunos (as) - Computador de mesa (desktop)", `n`=NA, `prop`=NA) %>%
  #add_row(escolas %>% count(var =QT_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  #add_row(var = "Quantidade de computadores em uso pelos alunos (as) - Computador portátil", `n`=NA, `prop`=NA) %>%
  #add_row(escolas %>% count(var =QT_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  #add_row(var = "Quantidade de computadores em uso pelos alunos (as) - Tablet", `n`=NA, `prop`=NA) %>%
  #add_row(escolas %>% count(var =QT_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  #add_row(escolas %>% count(var =IN_INTERNET) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) #%>% 
  
  
# TABELA - ESCOLAS BRASIL - POR REGIÃO E ESTADOS (QUALQUER ESCOLA) ####
tb_estados <- data.frame(var = "Nordeste",`n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas %>% filter(CO_REGIAO==2) %>% count(var=CO_UF) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))  %>% 
        
  add_row(var = "Sudeste", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(CO_REGIAO==3) %>% count(var=CO_UF) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))  %>% 
        
  add_row(var = "Sul", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(CO_REGIAO==4) %>% count(var=CO_UF) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))  %>% 
        
  add_row(var = "Norte", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(CO_REGIAO==1) %>% count(var=CO_UF) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))  %>% 
        
  add_row(var = "Centro-Oeste", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(CO_REGIAO==5) %>% count(var=CO_UF) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))  
      
# TABELA - ESCOLAS - RONDÔNIA ####  
tb_escolas_RO <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RO %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de Funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%   
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%   
  add_row(escolas_RO %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de computadores", `n`=NA, `prop`=NA) %>%   
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%   add_row(var = "Desktop (Computador de mesa)", `n`=NA, `prop`=NA) %>%   
  add_row(escolas_RO %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =IN_INTERNET) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) #%>% 
  
  
# TABELA - ESCOLAS - RONDÔNIA - ENFERMAGEM ####  
tb_escolas_RO_enf <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%

  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%

  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RONDÔNIA - RADIOLOGIA ####  
tb_escolas_RO_radio <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Radiologia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RONDÔNIA - ANÁLISES  CLÍNICAS ####  
      
tb_escolas_RO_anal_cli <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  #add_row(var = "Quantidade de computadores em uso pelos alunos (as) - Computador de mesa (desktop)", `n`=NA, `prop`=NA) %>%
  #add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =QT_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  #add_row(var = "Quantidade de computadores em uso pelos alunos (as) - Computador portátil", `n`=NA, `prop`=NA) %>%
  #add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =QT_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  #add_row(var = "Quantidade de computadores em uso pelos alunos (as) - Tablet", `n`=NA, `prop`=NA) %>%
  #add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =QT_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  #add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  #add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RONDÔNIA - NUTRIÇÃO ####  
      
tb_escolas_RO_nut <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RONDÔNIA - PRÓTESE DENTÁRIA ####  
      
tb_escolas_RO_prot <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - MARANHÃO ####  
      
tb_escolas_MA <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_MA %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de Funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%   
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%   
  add_row(escolas_MA %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de computadores", `n`=NA, `prop`=NA) %>%   
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%   add_row(var = "Desktop (Computador de mesa)", `n`=NA, `prop`=NA) %>%   
  add_row(escolas_MA %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =IN_INTERNET) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))  
  

# TABELA - ESCOLAS - MARANHÃO - ENFERMAGEM ####  
tb_escolas_MA_enf <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Enfermagem") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - MARANHÃO - RADIOLOGIA ####  
tb_escolas_MA_radio <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Radiologia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))  

# TABELA - ESCOLAS - MARANHÃO - ENFERMAGEM ####  
      
tb_escolas_MA_anal_cli <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - MARANHÃO - EQUIPAMENTOS BIOMÉDICOS ####
tb_escolas_MA_equip_biom <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Equipamentos BiomÃ©dicos") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# TABELA - ESCOLAS - MARANHÃO - FARMÁCIA ####
tb_escolas_MA_far <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Farmácia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 


# TABELA - ESCOLAS - MARANHÃO - GERÊNCIA DE SAÚDE ####
tb_escolas_MA_ger <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - MARANHÃO - NUTRIÇÃO ####
tb_escolas_MA_nut <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - MARANHÃO - REGISTROS E INFORMAÇÕES ####
tb_escolas_MA_reg <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Registros e Informações em Saúde") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - MARANHÃO - SAÚDE MENTAL ####
tb_escolas_MA_sbucal <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_MA_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - GOIÁS ####  

tb_escolas_GO <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_GO %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de Funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%   
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%   
  add_row(escolas_GO %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de computadores", `n`=NA, `prop`=NA) %>%
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  add_row(var = "Desktop (Computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_INTERNET) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - GOIÁS - ENFERMAGEM ####
tb_escolas_GO_enf <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Enfermagem") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - GOIÁS - ANÁLISES CLÍNICAS ####
tb_escolas_GO_anal_cli <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - GOIÁS - RADIOLOGIA ####
tb_escolas_GO_radio <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Radiologia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

      
# TABELA - ESCOLAS - GOIÁS - SAÚDE BUCAL ####     
tb_escolas_GO_sbucal <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - GOIÁS - NUTRIÇÃO ####
tb_escolas_GO_nut <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 


# TABELA - ESCOLAS - GOIÁS - PRÓTESE ####
tb_escolas_GO_prot <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - GOIÁS - VIGILÂNCIA ####
tb_escolas_GO_VIG <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - GOIÁS - NECRO ####
tb_escolas_GO_necro <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Necropsia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - GOIÁS - FARMÁCIA ####
tb_escolas_GO_far <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_GO_long %>% filter(curso=="Farmácia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# TABELA - ESCOLAS - RIO GRANDE DO SUL ####

tb_escolas_RS <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de Funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%   
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%   
  add_row(escolas_RS %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de computadores", `n`=NA, `prop`=NA) %>%   
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%   add_row(var = "Desktop (Computador de mesa)", `n`=NA, `prop`=NA) %>%   
  add_row(escolas_RS %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =IN_INTERNET) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) #%>% 
  

# TABELA - ESCOLAS - RIO GRANDE DO SUL - ENFERMAGEM ####
tb_escolas_RS_enf <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Enfermagem") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RIO GRANDE DO SUL - RADIOLOGIA  ####
tb_escolas_RS_radio <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%

  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Radiologia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RIO GRANDE DO SUL - NUTRIÇÃO  ####
tb_escolas_RS_nut <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RIO GRANDE DO SUL - ANÁLISES CLÍNICAS  ####
tb_escolas_RS_anal_cli <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RIO GRANDE DO SUL - PRÓTESE  ####
tb_escolas_RS_prot <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Prótese dentária") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RIO GRANDE DO SUL - FARMÁCIA  ####
tb_escolas_RS_far <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Farmácia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RIO GRANDE DO SUL - GERÊNCIA  ####
tb_escolas_RS_ger <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# TABELA - ESCOLAS - RIO GRANDE DO SUL - MASSOTERAPIA  ####
tb_escolas_RS_masso <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Massoterapia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RIO GRANDE DO SUL - SAÚDE BUCAL  ####
tb_escolas_RS_sbucal <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RIO GRANDE DO SUL - VIGILÂNCIA  ####
tb_escolas_RS_sbucal <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RIO GRANDE DO SUL - CUIDADO IDOSO  ####
tb_escolas_RS_cuidoso <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Cuidados de Idosos") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - RIO GRANDE DO SUL - CITOPATOLOGIA  ####
tb_escolas_RS_cito <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  

  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_RS_long %>% filter(curso=="Citopatologia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 


# TABELA - ESCOLAS - PERNAMBUCO ####

tb_escolas_PE <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_PE %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de Funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%   
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%   
  add_row(escolas_PE %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de computadores", `n`=NA, `prop`=NA) %>%   
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%   add_row(var = "Desktop (Computador de mesa)", `n`=NA, `prop`=NA) %>%   
  add_row(escolas_PE %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =IN_INTERNET) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) #%>% 
  

# TABELA - ESCOLAS - PERNAMBUCO - ENFERMAGEM ####
tb_escolas_PE_enf <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Enfermagem") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - PERNAMBUCO - RADIOLOGIA  ####
tb_escolas_PE_radio <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Radiologia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - PERNAMBUCO - ANÁLISES CLÍNICAS  ####
tb_escolas_PE_anal_cli <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Análises clínicas") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - PERNAMBUCO - NUTRIÇÃO  ####
tb_escolas_PE_nut <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Nutrição e Dietética") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 



# TABELA - ESCOLAS - PERNAMBUCO - FARMÁCIA  ####
tb_escolas_PE_far <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Farmácia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - PERNAMBUCO - SAÚDE BUCAL  ####
tb_escolas_PE_sbucal <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Saúde Bucal") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - PERNAMBUCO - VIGILÂNCIA  ####
tb_escolas_PE_VIG <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Vigilância em Saúde") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - PERNAMBUCO - GERÊNCIA  ####
tb_escolas_PE_ger <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Gerência de Saúde") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

# TABELA - ESCOLAS - PERNAMBUCO - HEMO  ####
tb_escolas_PE_hemo <- data.frame(var = "Localização", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =TP_LOCALIZACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var = TP_SITUACAO_FUNCIONAMENTO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =TP_DEPENDENCIA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Órgão que a escola pública está vinculada", `n`=NA, `prop`=NA) %>%
  add_row(var = "Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_VINCULO_SECRETARIA_EDUCACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_VINCULO_SEGURANCA_PUBLICA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_VINCULO_SECRETARIA_SAUDE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_VINCULO_OUTRO_ORGAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =TP_REGULAMENTACAO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Disponibilidade de Computadores", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Uso técnico e administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_COMPUTADOR) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Uso pelos alunos (as)", `n`=NA, `prop`=NA) %>%
  
  add_row(var = "Desktop (computador de mesa)", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_DESKTOP_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Portátil", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_COMP_PORTATIL_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_TABLET_ALUNO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>%
  add_row(var = "Para uso dos alunos", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_INTERNET_ALUNOS) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso administrativo", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_INTERNET_ADMINISTRATIVO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_INTERNET_APRENDIZAGEM) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Para uso da comunidade", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_INTERNET_COMUNIDADE) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>%
  add_row(escolas_PE_long %>% filter(curso=="Hemoterapia") %>% count(var =IN_BANDA_LARGA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) 

      
# REMOVER BASES ####
rm(escolas_MA, escolas_MA_long, 
   escolas_RO, escolas_RO_long, 
   escolas_GO, escolas_GO_long,
   escolas_RS, escolas_RS_long,
   escolas_PE, escolas_PE_long,
   escolas)



