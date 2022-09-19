# CARREGAR BIBLIOTECAS ####

library(data.table)
library(tidyverse)

setwd("C:/Users/laisr/OneDrive/Documentos/Área de Trabalho/CENSO ESCOLAR/analise/github/dados")

# IMPORTA BASE DE MATRICULAS DO NORDESTE, SELECIONANDO APENAS VARIAVEIS DO DESCRITIVO ####
matriculas_ne <- fread("matricula_nordeste.csv", select=c("TP_TIPO_ATENDIMENTO_TURMA", "TP_SEXO", "TP_COR_RACA", "CO_UF", "TP_ETAPA_ENSINO", 
                                                          "CO_CURSO_EDUC_PROFISSIONAL", "IN_PROFISSIONALIZANTE"))

# CALCULA DENOMINADOR - CURSO DE EDUCAÇÃO PROFISSIONALIZANTE/ NORDESTE ####
matriculas_ne %>% filter (CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
                            CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
                            CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
                            CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
                            CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
                            CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
                            CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>%  count()

# CURADORIA / NORDESTE ####
matriculas_ne <- matriculas_ne %>% 
  filter(CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
           CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
           CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
           CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
           CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
           CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
           CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>% 
  filter(CO_UF==21 | CO_UF==26) %>% 
  mutate(TP_SEXO = case_when(TP_SEXO==1 ~ "Masculino", TP_SEXO==2 ~ "Feminino", TRUE~"Ignorado")) %>% 
  mutate(TP_COR_RACA = case_when(TP_COR_RACA==1~"Branca", TP_COR_RACA==2~"Preta", TP_COR_RACA==3~"Parda",
                                 TP_COR_RACA==4~"Amarela", TP_COR_RACA==5~"Indígena", TRUE~"Ignorado")) %>% 
  mutate(TP_ETAPA_ENSINO = case_when(TP_ETAPA_ENSINO==30 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série",
                                     TP_ETAPA_ENSINO==31 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série",
                                     TP_ETAPA_ENSINO==32 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série",
                                     TP_ETAPA_ENSINO==33 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série",
                                     TP_ETAPA_ENSINO==34 ~ "Curso Técnico Integrado (Ensino Médio Integrado) Não Seriada",
                                     TP_ETAPA_ENSINO==35 ~ "Ensino Médio - Modalidade Normal/Magistério 1ª Série",
                                     TP_ETAPA_ENSINO==36 ~ "Ensino Médio - Modalidade Normal/Magistério 2ª Série",
                                     TP_ETAPA_ENSINO==37 ~ "Ensino Médio - Modalidade Normal/Magistério 3ª Série",
                                     TP_ETAPA_ENSINO==38 ~ "Ensino Médio - Modalidade Normal/Magistério 4ª Série",
                                     TP_ETAPA_ENSINO==39 ~ "Curso Técnico - Concomitante",
                                     TP_ETAPA_ENSINO==40 ~ "Curso Técnico - Subsequente",
                                     TP_ETAPA_ENSINO==65 ~ "EJA - Ensino Fundamental - Projovem Urbano",
                                     TP_ETAPA_ENSINO==67 ~ "Curso FIC integrado na modalidade EJA  - Nível Médio",
                                     TP_ETAPA_ENSINO==68 ~ "Curso FIC Concomitante",
                                     TP_ETAPA_ENSINO==69 ~ "EJA - Ensino Fundamental - Anos Iniciais",
                                     TP_ETAPA_ENSINO==70 ~ "EJA - Ensino Fundamental - Anos Finais",
                                     TP_ETAPA_ENSINO==71 ~ "EJA - Ensino Médio",
                                     TP_ETAPA_ENSINO==72 ~ "EJA - Ensino Fundamental  - Anos iniciais e Anos finais",
                                     TP_ETAPA_ENSINO==73 ~ "Curso FIC integrado na modalidade EJA - Nível Fundamental (EJA integrada à Educação Profissional de Nível Fundamental)",
                                     TP_ETAPA_ENSINO==74 ~ "Curso Técnico Integrado na Modalidade EJA (EJA integrada à Educação Profissional de Nível Médio)", 
                                     TRUE ~ "Ignorado")) %>% 
  mutate(CO_CURSO_EDUC_PROFISSIONAL= case_when(CO_CURSO_EDUC_PROFISSIONAL==1001	~"Agente Comunitário de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1002	~"Análises clínicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1004	~"Citopatologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1006	~"Enfermagem",
                                               CO_CURSO_EDUC_PROFISSIONAL==1007	~"Equipamentos Biomédicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1009	~"Farmácia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1010	~"Gerência de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1011	~"Hemoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1012	~"Saúde Bucal",
                                               CO_CURSO_EDUC_PROFISSIONAL==1014	~"Imobilizações Ortopédicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1015	~"Massoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1018	~"Nutrição e Dietética",
                                               CO_CURSO_EDUC_PROFISSIONAL==1020	~"Órteses e Próteses",
                                               CO_CURSO_EDUC_PROFISSIONAL==1022	~"Prótese Dentária",
                                               CO_CURSO_EDUC_PROFISSIONAL==1023	~"Radiologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1024	~"Reabilitação de Dependentes Químicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1026	~"Registros e Informações em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1028	~"Vigilância em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1029	~"Cuidados de Idosos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1030	~"Necropsia",
                                               TRUE~"Ignorado"))


# TABELA - MATRÍCULAS - MARANHÃO #### 
tb_matriculas_MA <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# TABELA - MATRÍCULAS - MARANHÃO - CURSOS ####
tb_matriculas_MA_Enfermagem <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_MA_Radiologia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_MA_Analises <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_MA_nutricao <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_MA_farmacia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_MA_gerencia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_MA_saudebucal <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_MA_equipebiom <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Equipamentos Biomédicos") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Equipamentos Biomédicos") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Equipamentos Biomédicos") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Equipamentos Biomédicos") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_MA_registros <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Registros e Informações em Saúde") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Registros e Informações em Saúde") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Registros e Informações em Saúde") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==21) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Registros e Informações em Saúde") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# TABELA - MATRÍCULAS - PERNAMBUCO #### 
tb_matriculas_PE <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# TABELA - MATRÍCULAS - PERNAMBUCO - CURSOS ####
tb_matriculas_PE_Enfermagem <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_PE_Radiologia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_PE_Analises <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_PE_Nutricao <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_PE_Farmacia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_PE_SBucal <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_PE_VigSaude <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Vigilância em Saúde") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Vigilância em Saúde") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Vigilância em Saúde") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Vigilância em Saúde") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_PE_Gerencia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_PE_Hemo <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Hemoterapia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Hemoterapia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Hemoterapia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_ne %>% filter(CO_UF==26) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Hemoterapia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

rm(matriculas_ne)


# IMPORTA BASE DE MATRICULAS DO NORTE, SELECIONANDO APENAS VARIAVEIS DO DESCRITIVO ####
matriculas_no <- fread("matricula_norte.csv", select=c("TP_TIPO_ATENDIMENTO_TURMA", "TP_SEXO", "TP_COR_RACA", "CO_UF", "TP_ETAPA_ENSINO", 
                                                       "CO_CURSO_EDUC_PROFISSIONAL", "IN_PROFISSIONALIZANTE"))

# CALCULA DENOMINADOR - CURSO DE EDUCAÇÃO PROFISSIONALIZANTE/ NORTE ####
matriculas_no %>% filter (TP_ETAPA_ENSINO==30 | TP_ETAPA_ENSINO==31 | TP_ETAPA_ENSINO==32 |
                            TP_ETAPA_ENSINO==33 | TP_ETAPA_ENSINO==34 | TP_ETAPA_ENSINO==35 |
                            TP_ETAPA_ENSINO==36 | TP_ETAPA_ENSINO==37 | TP_ETAPA_ENSINO==38 |
                            TP_ETAPA_ENSINO==39 | TP_ETAPA_ENSINO==40 | TP_ETAPA_ENSINO==65 |
                            TP_ETAPA_ENSINO==67 | TP_ETAPA_ENSINO==68 | TP_ETAPA_ENSINO==73 |
                            TP_ETAPA_ENSINO==74 | IN_PROFISSIONALIZANTE==1) %>%  count()

# CURADORIA / NORTE ####
matriculas_no <- matriculas_no %>% 
  filter(CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
           CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
           CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
           CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
           CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
           CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
           CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>% 
  filter(CO_UF==11) %>% 
  mutate(TP_SEXO = case_when(TP_SEXO==1 ~ "Masculino", TP_SEXO==2 ~ "Feminino", TRUE~"Ignorado")) %>% 
  mutate(TP_COR_RACA = case_when(TP_COR_RACA==1~"Branca", TP_COR_RACA==2~"Preta", TP_COR_RACA==3~"Parda",
                                 TP_COR_RACA==4~"Amarela", TP_COR_RACA==5~"Indígena", TRUE~"Ignorado")) %>% 
  mutate(TP_ETAPA_ENSINO = case_when(TP_ETAPA_ENSINO==30 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série",
                                     TP_ETAPA_ENSINO==31 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série",
                                     TP_ETAPA_ENSINO==32 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série",
                                     TP_ETAPA_ENSINO==33 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série",
                                     TP_ETAPA_ENSINO==34 ~ "Curso Técnico Integrado (Ensino Médio Integrado) Não Seriada",
                                     TP_ETAPA_ENSINO==35 ~ "Ensino Médio - Modalidade Normal/Magistério 1ª Série",
                                     TP_ETAPA_ENSINO==36 ~ "Ensino Médio - Modalidade Normal/Magistério 2ª Série",
                                     TP_ETAPA_ENSINO==37 ~ "Ensino Médio - Modalidade Normal/Magistério 3ª Série",
                                     TP_ETAPA_ENSINO==38 ~ "Ensino Médio - Modalidade Normal/Magistério 4ª Série",
                                     TP_ETAPA_ENSINO==39 ~ "Curso Técnico - Concomitante",
                                     TP_ETAPA_ENSINO==40 ~ "Curso Técnico - Subsequente",
                                     TP_ETAPA_ENSINO==65 ~ "EJA - Ensino Fundamental - Projovem Urbano",
                                     TP_ETAPA_ENSINO==67 ~ "Curso FIC integrado na modalidade EJA  - Nível Médio",
                                     TP_ETAPA_ENSINO==68 ~ "Curso FIC Concomitante",
                                     TP_ETAPA_ENSINO==69 ~ "EJA - Ensino Fundamental - Anos Iniciais",
                                     TP_ETAPA_ENSINO==70 ~ "EJA - Ensino Fundamental - Anos Finais",
                                     TP_ETAPA_ENSINO==71 ~ "EJA - Ensino Médio",
                                     TP_ETAPA_ENSINO==72 ~ "EJA - Ensino Fundamental  - Anos iniciais e Anos finais",
                                     TP_ETAPA_ENSINO==73 ~ "Curso FIC integrado na modalidade EJA - Nível Fundamental (EJA integrada à Educação Profissional de Nível Fundamental)",
                                     TP_ETAPA_ENSINO==74 ~ "Curso Técnico Integrado na Modalidade EJA (EJA integrada à Educação Profissional de Nível Médio)", 
                                     TRUE ~ "Ignorado")) %>% 
  mutate(CO_CURSO_EDUC_PROFISSIONAL= case_when(CO_CURSO_EDUC_PROFISSIONAL==1001	~"Agente Comunitário de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1002	~"Análises clínicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1004	~"Citopatologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1006	~"Enfermagem",
                                               CO_CURSO_EDUC_PROFISSIONAL==1007	~"Equipamentos Biomédicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1009	~"Farmácia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1010	~"Gerência de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1011	~"Hemoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1012	~"Saúde Bucal",
                                               CO_CURSO_EDUC_PROFISSIONAL==1014	~"Imobilizações Ortopédicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1015	~"Massoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1018	~"Nutrição e Dietética",
                                               CO_CURSO_EDUC_PROFISSIONAL==1020	~"Órteses e Próteses",
                                               CO_CURSO_EDUC_PROFISSIONAL==1022	~"Prótese Dentária",
                                               CO_CURSO_EDUC_PROFISSIONAL==1023	~"Radiologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1024	~"Reabilitação de Dependentes Químicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1026	~"Registros e Informações em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1028	~"Vigilância em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1029	~"Cuidados de Idosos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1030	~"Necropsia",
                                               TRUE~"Ignorado"))


# TABELA - MATRÍCULAS - RONDÔNIA ####
tb_matriculas_RO <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# TABELA - MATRÍCULAS - RONDÔNIA - CURSOS ####
tb_matriculas_RO_Enfermagem <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_RO_Radiologia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_RO_Analises <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_RO_protese <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Prótese Dentária") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Prótese Dentária") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Prótese Dentária") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Prótese Dentária") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_RO_nutricao <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_no %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))


rm(matriculas_no)


# IMPORTA BASE DE MATRICULAS DO SUL, SELECIONANDO APENAS VARIAVEIS DO DESCRITIVO ####
matriculas_sul <- fread("matricula_sul.csv", select=c("TP_TIPO_ATENDIMENTO_TURMA", "TP_SEXO", "TP_COR_RACA", 
                                                      "CO_UF", "TP_ETAPA_ENSINO", 
                                                      "CO_CURSO_EDUC_PROFISSIONAL", "IN_PROFISSIONALIZANTE"))

# CALCULA DENOMINADOR - CURSO DE EDUCAÇÃO PROFISSIONALIZANTE/ SUL ####
matriculas_sul %>% filter (CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
                             CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
                             CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
                             CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
                             CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
                             CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
                             CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>%  count()


# CURADORIA / SUL ####
matriculas_sul <- matriculas_sul %>% 
  filter(CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
           CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
           CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
           CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
           CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
           CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
           CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>% 
  #filter(CO_UF==43) %>% 
  mutate(TP_SEXO = case_when(TP_SEXO==1 ~ "Masculino", TP_SEXO==2 ~ "Feminino", TRUE~"Ignorado")) %>% 
  mutate(TP_COR_RACA = case_when(TP_COR_RACA==1~"Branca", TP_COR_RACA==2~"Preta", TP_COR_RACA==3~"Parda",
                                 TP_COR_RACA==4~"Amarela", TP_COR_RACA==5~"Indígena", TRUE~"Ignorado")) %>% 
  mutate(TP_ETAPA_ENSINO = case_when(TP_ETAPA_ENSINO==30 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série",
                                     TP_ETAPA_ENSINO==31 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série",
                                     TP_ETAPA_ENSINO==32 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série",
                                     TP_ETAPA_ENSINO==33 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série",
                                     TP_ETAPA_ENSINO==34 ~ "Curso Técnico Integrado (Ensino Médio Integrado) Não Seriada",
                                     TP_ETAPA_ENSINO==35 ~ "Ensino Médio - Modalidade Normal/Magistério 1ª Série",
                                     TP_ETAPA_ENSINO==36 ~ "Ensino Médio - Modalidade Normal/Magistério 2ª Série",
                                     TP_ETAPA_ENSINO==37 ~ "Ensino Médio - Modalidade Normal/Magistério 3ª Série",
                                     TP_ETAPA_ENSINO==38 ~ "Ensino Médio - Modalidade Normal/Magistério 4ª Série",
                                     TP_ETAPA_ENSINO==39 ~ "Curso Técnico - Concomitante",
                                     TP_ETAPA_ENSINO==40 ~ "Curso Técnico - Subsequente",
                                     TP_ETAPA_ENSINO==65 ~ "EJA - Ensino Fundamental - Projovem Urbano",
                                     TP_ETAPA_ENSINO==67 ~ "Curso FIC integrado na modalidade EJA  - Nível Médio",
                                     TP_ETAPA_ENSINO==68 ~ "Curso FIC Concomitante",
                                     TP_ETAPA_ENSINO==69 ~ "EJA - Ensino Fundamental - Anos Iniciais",
                                     TP_ETAPA_ENSINO==70 ~ "EJA - Ensino Fundamental - Anos Finais",
                                     TP_ETAPA_ENSINO==71 ~ "EJA - Ensino Médio",
                                     TP_ETAPA_ENSINO==72 ~ "EJA - Ensino Fundamental  - Anos iniciais e Anos finais",
                                     TP_ETAPA_ENSINO==73 ~ "Curso FIC integrado na modalidade EJA - Nível Fundamental (EJA integrada à Educação Profissional de Nível Fundamental)",
                                     TP_ETAPA_ENSINO==74 ~ "Curso Técnico Integrado na Modalidade EJA (EJA integrada à Educação Profissional de Nível Médio)", 
                                     TRUE ~ "Ignorado")) %>% 
  mutate(CO_CURSO_EDUC_PROFISSIONAL= case_when(CO_CURSO_EDUC_PROFISSIONAL==1001	~"Agente Comunitário de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1002	~"Análises clínicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1004	~"Citopatologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1006	~"Enfermagem",
                                               CO_CURSO_EDUC_PROFISSIONAL==1007	~"Equipamentos Biomédicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1009	~"Farmácia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1010	~"Gerência de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1011	~"Hemoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1012	~"Saúde Bucal",
                                               CO_CURSO_EDUC_PROFISSIONAL==1014	~"Imobilizações Ortopédicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1015	~"Massoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1018	~"Nutrição e Dietética",
                                               CO_CURSO_EDUC_PROFISSIONAL==1020	~"Órteses e Próteses",
                                               CO_CURSO_EDUC_PROFISSIONAL==1022	~"Prótese Dentária",
                                               CO_CURSO_EDUC_PROFISSIONAL==1023	~"Radiologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1024	~"Reabilitação de Dependentes Químicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1026	~"Registros e Informações em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1028	~"Vigilância em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1029	~"Cuidados de Idosos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1030	~"Necropsia",
                                               TRUE~"Ignorado"))

# TABELA - MATRÍCULAS - RIO GRANDE DO SUL #### 
tb_matriculas_RS <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# TABELA - MATRÍCULAS - RIO GRANDE DO SUL - CURSOS ####
tb_matriculas_RS_Enfermagem <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_RS_Radiologia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_RS_nutricao <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_RS_Analises <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_RS_protese <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Prótese Dentária") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Prótese Dentária") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Prótese Dentária") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Prótese Dentária") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))


tb_matriculas_RS_farmacia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_RS_gerencia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_RS_massot <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Massoterapia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Massoterapia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Massoterapia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Massoterapia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))


tb_matriculas_RS_saudebucal <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_RS_CuIdosos <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Cuidados de Idosos") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Cuidados de Idosos") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Cuidados de Idosos") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Cuidados de Idosos") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_RS_citop <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Citopatologia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Citopatologia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Citopatologia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_sul %>% filter(CO_UF==43) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Citopatologia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

rm(matriculas_sul)


# IMPORTA BASE DE MATRICULAS DO CENTRO-OESTE, SELECIONANDO APENAS VARIAVEIS DO DESCRITIVO ####
matriculas_co <- fread("matricula_co.csv", select=c("TP_TIPO_ATENDIMENTO_TURMA", "TP_SEXO", "TP_COR_RACA", "CO_UF", "TP_ETAPA_ENSINO", 
                                                    "CO_CURSO_EDUC_PROFISSIONAL", "IN_PROFISSIONALIZANTE"))
# CALCULA DENOMINADOR - CURSO DE EDUCAÇÃO PROFISSIONALIZANTE/ CENTRO-OESTE ####
matriculas_co %>% filter (CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
                            CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
                            CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
                            CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
                            CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
                            CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
                            CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>%  count()



# CURADORIA / CENTRO-OESTE####
matriculas_co <- matriculas_co %>% 
  filter(CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
           CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
           CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
           CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
           CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
           CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
           CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>% 
  #filter(CO_UF==52) %>% 
  mutate(TP_SEXO = case_when(TP_SEXO==1 ~ "Masculino", TP_SEXO==2 ~ "Feminino", TRUE~"Ignorado")) %>% 
  mutate(TP_COR_RACA = case_when(TP_COR_RACA==1~"Branca", TP_COR_RACA==2~"Preta", TP_COR_RACA==3~"Parda",
                                 TP_COR_RACA==4~"Amarela", TP_COR_RACA==5~"Indígena", TRUE~"Ignorado")) %>% 
  mutate(TP_ETAPA_ENSINO = case_when(TP_ETAPA_ENSINO==30 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série",
                                     TP_ETAPA_ENSINO==31 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série",
                                     TP_ETAPA_ENSINO==32 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série",
                                     TP_ETAPA_ENSINO==33 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série",
                                     TP_ETAPA_ENSINO==34 ~ "Curso Técnico Integrado (Ensino Médio Integrado) Não Seriada",
                                     TP_ETAPA_ENSINO==35 ~ "Ensino Médio - Modalidade Normal/Magistério 1ª Série",
                                     TP_ETAPA_ENSINO==36 ~ "Ensino Médio - Modalidade Normal/Magistério 2ª Série",
                                     TP_ETAPA_ENSINO==37 ~ "Ensino Médio - Modalidade Normal/Magistério 3ª Série",
                                     TP_ETAPA_ENSINO==38 ~ "Ensino Médio - Modalidade Normal/Magistério 4ª Série",
                                     TP_ETAPA_ENSINO==39 ~ "Curso Técnico - Concomitante",
                                     TP_ETAPA_ENSINO==40 ~ "Curso Técnico - Subsequente",
                                     TP_ETAPA_ENSINO==65 ~ "EJA - Ensino Fundamental - Projovem Urbano",
                                     TP_ETAPA_ENSINO==67 ~ "Curso FIC integrado na modalidade EJA  - Nível Médio",
                                     TP_ETAPA_ENSINO==68 ~ "Curso FIC Concomitante",
                                     TP_ETAPA_ENSINO==69 ~ "EJA - Ensino Fundamental - Anos Iniciais",
                                     TP_ETAPA_ENSINO==70 ~ "EJA - Ensino Fundamental - Anos Finais",
                                     TP_ETAPA_ENSINO==71 ~ "EJA - Ensino Médio",
                                     TP_ETAPA_ENSINO==72 ~ "EJA - Ensino Fundamental  - Anos iniciais e Anos finais",
                                     TP_ETAPA_ENSINO==73 ~ "Curso FIC integrado na modalidade EJA - Nível Fundamental (EJA integrada à Educação Profissional de Nível Fundamental)",
                                     TP_ETAPA_ENSINO==74 ~ "Curso Técnico Integrado na Modalidade EJA (EJA integrada à Educação Profissional de Nível Médio)", 
                                     TRUE ~ "Ignorado")) %>% 
  mutate(CO_CURSO_EDUC_PROFISSIONAL= case_when(CO_CURSO_EDUC_PROFISSIONAL==1001	~"Agente Comunitário de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1002	~"Análises clínicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1004	~"Citopatologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1006	~"Enfermagem",
                                               CO_CURSO_EDUC_PROFISSIONAL==1007	~"Equipamentos Biomédicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1009	~"Farmácia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1010	~"Gerência de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1011	~"Hemoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1012	~"Saúde Bucal",
                                               CO_CURSO_EDUC_PROFISSIONAL==1014	~"Imobilizações Ortopédicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1015	~"Massoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1018	~"Nutrição e Dietética",
                                               CO_CURSO_EDUC_PROFISSIONAL==1020	~"Órteses e Próteses",
                                               CO_CURSO_EDUC_PROFISSIONAL==1022	~"Prótese Dentária",
                                               CO_CURSO_EDUC_PROFISSIONAL==1023	~"Radiologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1024	~"Reabilitação de Dependentes Químicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1026	~"Registros e Informações em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1028	~"Vigilância em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1029	~"Cuidados de Idosos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1030	~"Necropsia",
                                               TRUE~"Ignorado"))


# TABELA - MATRÍCULAS - GOIÁS #### 
tb_matriculas_GO <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co %>% filter(CO_UF==52) %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# TABELA - MATRÍCULAS - GOIÁS - CURSOS ####
tb_matriculas_GO_Enfermagem <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_GO_Radiologia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_GO_Analises <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

tb_matriculas_GO_saudebucal <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))


tb_matriculas_GO_nutricao <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))


tb_matriculas_GO_protese <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Prótese Dentária") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Prótese Dentária") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Prótese Dentária") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Prótese Dentária") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))


tb_matriculas_GO_Vigilancia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Vigilância em Saúde") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Vigilância em Saúde") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Vigilância em Saúde") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Vigilância em Saúde") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))


tb_matriculas_GO_necro <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Necropsia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Necropsia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Necropsia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Necropsia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))





tb_matriculas_GO_farmacia <- data.frame(var = "Sexo", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = TP_SEXO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = TP_COR_RACA) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(matriculas_co %>% filter(CO_UF==52) %>% filter(CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

rm(matriculas_co)

