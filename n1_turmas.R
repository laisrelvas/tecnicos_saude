# CARREGAR BIBLIOTECAS E BASE ####
library(data.table)
library(tidyverse)
library(openxlsx)

setwd("C:/Users/laisr/OneDrive/Documentos/Área de Trabalho/CENSO ESCOLAR/analise/github/tecnicos_saude/dados")

turmas <- fread("turmas.csv", 
                select=c(
                  "TP_MEDIACAO_DIDATICO_PEDAGO", 
                  "TP_ETAPA_ENSINO", 
                  "CO_CURSO_EDUC_PROFISSIONAL", 
                  "CO_REGIAO", 
                  "CO_UF", 
                  "CO_ENTIDADE",
                  "IN_PROFISSIONALIZANTE"))

str(turmas)

escolas <- read.table("escolas.csv", sep="|", header=T)

str(escolas)

# CALCULAR DENOMINADOR - CURSO DE EDUCAÇÃO PROFISSIONALIZANTE ####
turmas %>% 
  filter (TP_ETAPA_ENSINO==30 | TP_ETAPA_ENSINO==31 | TP_ETAPA_ENSINO==32 |
          TP_ETAPA_ENSINO==33 | TP_ETAPA_ENSINO==34 | TP_ETAPA_ENSINO==35 |
          TP_ETAPA_ENSINO==36 | TP_ETAPA_ENSINO==37 | TP_ETAPA_ENSINO==38 |
          TP_ETAPA_ENSINO==39 | TP_ETAPA_ENSINO==40 | TP_ETAPA_ENSINO==65 |
          TP_ETAPA_ENSINO==67 | TP_ETAPA_ENSINO==68 | TP_ETAPA_ENSINO==73 |
          TP_ETAPA_ENSINO==74 | IN_PROFISSIONALIZANTE==1) %>%  
  count()

# CRIA BANCO DE TURMAS EM RONDÔNIA ####
turmas_RO <- turmas %>% 
  filter(CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
           CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
           CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
           CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
           CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
           CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
           CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>% 
  filter(CO_UF==11) %>% 
  mutate(TP_MEDIACAO_DIDATICO_PEDAGO = case_when (TP_MEDIACAO_DIDATICO_PEDAGO==1 ~ "Presencial",
                                                  TP_MEDIACAO_DIDATICO_PEDAGO==2 ~ "Semipresencial",
                                                  TP_MEDIACAO_DIDATICO_PEDAGO==3 ~ "Educação a Distância - EAD",
                                                  TRUE~"Ignorado")) %>% 


  mutate(TP_ETAPA_ENSINO = case_when(TP_ETAPA_ENSINO == 1 ~ "Educação Infantil - Creche",
                                     TP_ETAPA_ENSINO == 2 ~ "Educação Infantil - Pré-escola",
                                     TP_ETAPA_ENSINO == 3 ~ "Educação Infantil - Unificada",
                                     TP_ETAPA_ENSINO == 56 ~ "Educação Infantil e Ensino Fundamental (9 anos) Multietapa",
                                     TP_ETAPA_ENSINO == 4 ~ "Ensino Fundamental de 8 anos - 1ª Série",
                                     TP_ETAPA_ENSINO == 5 ~ "Ensino Fundamental de 8 anos - 2ª Série",
                                     TP_ETAPA_ENSINO == 6 ~ "Ensino Fundamental de 8 anos - 3ª Série",
                                     TP_ETAPA_ENSINO == 7 ~ "Ensino Fundamental de 8 anos - 4ª Série",
                                     TP_ETAPA_ENSINO == 8 ~ "Ensino Fundamental de 8 anos - 5ª Série",
                                     TP_ETAPA_ENSINO == 9 ~ "Ensino Fundamental de 8 anos - 6ª Série",
                                     TP_ETAPA_ENSINO == 10 ~ "Ensino Fundamental de 8 anos - 7ª Série",
                                     TP_ETAPA_ENSINO == 11 ~ "Ensino Fundamental de 8 anos - 8ª Série",
                                     TP_ETAPA_ENSINO == 12 ~ "Ensino Fundamental de 8 anos - Multi",
                                     TP_ETAPA_ENSINO == 13 ~ "Ensino Fundamental de 8 anos - Correção de Fluxo",
                                     TP_ETAPA_ENSINO == 14 ~ "Ensino Fundamental de 9 anos - 1º Ano",
                                     TP_ETAPA_ENSINO == 15 ~ "Ensino Fundamental de 9 anos - 2º Ano",
                                     TP_ETAPA_ENSINO == 16 ~ "Ensino Fundamental de 9 anos - 3º Ano",
                                     TP_ETAPA_ENSINO == 17 ~ "Ensino Fundamental de 9 anos - 4º Ano",
                                     TP_ETAPA_ENSINO == 18 ~ "Ensino Fundamental de 9 anos - 5º Ano",
                                     TP_ETAPA_ENSINO == 19 ~ "Ensino Fundamental de 9 anos - 6º Ano",
                                     TP_ETAPA_ENSINO == 20 ~ "Ensino Fundamental de 9 anos - 7º Ano",
                                     TP_ETAPA_ENSINO == 21 ~ "Ensino Fundamental de 9 anos - 8º Ano",
                                     TP_ETAPA_ENSINO == 41 ~ "Ensino Fundamental de 9 anos - 9º Ano",
                                     TP_ETAPA_ENSINO == 22 ~ "Ensino Fundamental de 9 anos - Multi",
                                     TP_ETAPA_ENSINO == 23 ~ "Ensino Fundamental de 9 anos - Correção de Fluxo",
                                     TP_ETAPA_ENSINO == 24 ~ "Ensino Fundamental de 8 e 9 anos - Multi 8 e 9 anos",
                                     TP_ETAPA_ENSINO == 25 ~ "Ensino Médio - 1º ano/1ª Série",
                                     TP_ETAPA_ENSINO == 26 ~ "Ensino Médio - 2º ano/2ª Série",
                                     TP_ETAPA_ENSINO == 27 ~ "Ensino Médio - 3ºano/3ª Série",
                                     TP_ETAPA_ENSINO == 28 ~ "Ensino Médio - 4º ano/4ª Série",
                                     TP_ETAPA_ENSINO == 29 ~ "Ensino Médio - Não Seriada",
                                     TP_ETAPA_ENSINO==30 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série",
                                     TP_ETAPA_ENSINO==31 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série",
                                     TP_ETAPA_ENSINO==32 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série",
                                     TP_ETAPA_ENSINO==33 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série",
                                     TP_ETAPA_ENSINO==34 ~ "Curso Técnico Integrado (Ensino Médio Integrado) NÃ£o Seriada",
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
  mutate(CO_CURSO_EDUC_PROFISSIONAL= case_when(CO_CURSO_EDUC_PROFISSIONAL==1001	~"Agente Comunitério de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1002	~"Análises clínicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1004	~"Citopatologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1006	~"Enfermagem",
                                               CO_CURSO_EDUC_PROFISSIONAL==1007	~"Equipamentos BiomÃ©dicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1009	~"Farmácia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1010	~"Gerência de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1011	~"Hemoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1012	~"Saúde Bucal",
                                               CO_CURSO_EDUC_PROFISSIONAL==1014	~"Imobilizações ortopédicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1015	~"Massoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1018	~"Nutrição e Dietética",
                                               CO_CURSO_EDUC_PROFISSIONAL==1020	~"Órteses e Próteses",
                                               CO_CURSO_EDUC_PROFISSIONAL==1022	~"Prótese dentária",
                                               CO_CURSO_EDUC_PROFISSIONAL==1023	~"Radiologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1024	~"Reabilitação de Dependentes Químicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1026	~"Registros e Informações em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1028	~"Vigilância em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1029	~"Cuidados de Idosos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1030	~"Necropsia",
                                               TRUE~"Ignorado"))
# CRIA BANCO DE TURMAS EM GOIÁS ####
turmas_GO <- turmas %>% 
  filter(CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
           CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
           CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
           CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
           CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
           CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
           CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>% 
  filter(CO_UF==52) %>% 
  mutate(TP_MEDIACAO_DIDATICO_PEDAGO = case_when (TP_MEDIACAO_DIDATICO_PEDAGO==1 ~ "Presencial",
                                                  TP_MEDIACAO_DIDATICO_PEDAGO==2 ~ "Semipresencial",
                                                  TP_MEDIACAO_DIDATICO_PEDAGO==3 ~ "Educação a Distância - EAD",
                                                  TRUE~"Ignorado")) %>% 
  mutate(TP_ETAPA_ENSINO = case_when(TP_ETAPA_ENSINO == 1 ~ "Educação Infantil - Creche",
                                     TP_ETAPA_ENSINO == 2 ~ "Educação Infantil - Pré-escola",
                                     TP_ETAPA_ENSINO == 3 ~ "Educação Infantil - Unificada",
                                     TP_ETAPA_ENSINO == 56 ~ "Educação Infantil e Ensino Fundamental (9 anos) Multietapa",
                                     TP_ETAPA_ENSINO == 4 ~ "Ensino Fundamental de 8 anos - 1ª Série",
                                     TP_ETAPA_ENSINO == 5 ~ "Ensino Fundamental de 8 anos - 2ª Série",
                                     TP_ETAPA_ENSINO == 6 ~ "Ensino Fundamental de 8 anos - 3ª Série",
                                     TP_ETAPA_ENSINO == 7 ~ "Ensino Fundamental de 8 anos - 4ª Série",
                                     TP_ETAPA_ENSINO == 8 ~ "Ensino Fundamental de 8 anos - 5ª Série",
                                     TP_ETAPA_ENSINO == 9 ~ "Ensino Fundamental de 8 anos - 6ª Série",
                                     TP_ETAPA_ENSINO == 10 ~ "Ensino Fundamental de 8 anos - 7ª Série",
                                     TP_ETAPA_ENSINO == 11 ~ "Ensino Fundamental de 8 anos - 8ª Série",
                                     TP_ETAPA_ENSINO == 12 ~ "Ensino Fundamental de 8 anos - Multi",
                                     TP_ETAPA_ENSINO == 13 ~ "Ensino Fundamental de 8 anos - Correção de Fluxo",
                                     TP_ETAPA_ENSINO == 14 ~ "Ensino Fundamental de 9 anos - 1º Ano",
                                     TP_ETAPA_ENSINO == 15 ~ "Ensino Fundamental de 9 anos - 2º Ano",
                                     TP_ETAPA_ENSINO == 16 ~ "Ensino Fundamental de 9 anos - 3º Ano",
                                     TP_ETAPA_ENSINO == 17 ~ "Ensino Fundamental de 9 anos - 4º Ano",
                                     TP_ETAPA_ENSINO == 18 ~ "Ensino Fundamental de 9 anos - 5º Ano",
                                     TP_ETAPA_ENSINO == 19 ~ "Ensino Fundamental de 9 anos - 6º Ano",
                                     TP_ETAPA_ENSINO == 20 ~ "Ensino Fundamental de 9 anos - 7º Ano",
                                     TP_ETAPA_ENSINO == 21 ~ "Ensino Fundamental de 9 anos - 8º Ano",
                                     TP_ETAPA_ENSINO == 41 ~ "Ensino Fundamental de 9 anos - 9º Ano",
                                     TP_ETAPA_ENSINO == 22 ~ "Ensino Fundamental de 9 anos - Multi",
                                     TP_ETAPA_ENSINO == 23 ~ "Ensino Fundamental de 9 anos - Correção de Fluxo",
                                     TP_ETAPA_ENSINO == 24 ~ "Ensino Fundamental de 8 e 9 anos - Multi 8 e 9 anos",
                                     TP_ETAPA_ENSINO == 25 ~ "Ensino Médio - 1º ano/1ª Série",
                                     TP_ETAPA_ENSINO == 26 ~ "Ensino Médio - 2º ano/2ª Série",
                                     TP_ETAPA_ENSINO == 27 ~ "Ensino Médio - 3ºano/3ª Série",
                                     TP_ETAPA_ENSINO == 28 ~ "Ensino Médio - 4º ano/4ª Série",
                                     TP_ETAPA_ENSINO == 29 ~ "Ensino Médio - Não Seriada",
                                     TP_ETAPA_ENSINO==30 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série",
                                     TP_ETAPA_ENSINO==31 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série",
                                     TP_ETAPA_ENSINO==32 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série",
                                     TP_ETAPA_ENSINO==33 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série",
                                     TP_ETAPA_ENSINO==34 ~ "Curso Técnico Integrado (Ensino Médio Integrado) NÃ£o Seriada",
                                     TP_ETAPA_ENSINO==35 ~ "Ensino Médio - Modalidade Normal/Magistério 1ª Série",
                                     TP_ETAPA_ENSINO==36 ~ "Ensino Médio - Modalidade Normal/Magistério 2ª Série",
                                     TP_ETAPA_ENSINO==37 ~ "Ensino Médio - Modalidade Normal/Magistério 3ª Série",
                                     TP_ETAPA_ENSINO==38 ~ "Ensino Médio - Modalidade Normal/Magistério 4ª Série",
                                     TP_ETAPA_ENSINO==39 ~ "Curso Técnico - Concomitante",
                                     TP_ETAPA_ENSINO==40 ~ "Curso Técnico - Subsequente",
                                     TP_ETAPA_ENSINO==64 ~ "Curso Técnico Misto (Concomitante e Subsequente)",
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
  mutate(CO_CURSO_EDUC_PROFISSIONAL= case_when(CO_CURSO_EDUC_PROFISSIONAL==1001	~"Agente Comunitério de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1002	~"Análises clínicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1004	~"Citopatologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1006	~"Enfermagem",
                                               CO_CURSO_EDUC_PROFISSIONAL==1007	~"Equipamentos BiomÃ©dicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1009	~"Farmácia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1010	~"Gerência de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1011	~"Hemoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1012	~"Saúde Bucal",
                                               CO_CURSO_EDUC_PROFISSIONAL==1014	~"Imobilizações ortopédicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1015	~"Massoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1018	~"Nutrição e Dietética",
                                               CO_CURSO_EDUC_PROFISSIONAL==1020	~"Órteses e Próteses",
                                               CO_CURSO_EDUC_PROFISSIONAL==1022	~"Prótese dentária",
                                               CO_CURSO_EDUC_PROFISSIONAL==1023	~"Radiologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1024	~"Reabilitação de Dependentes Químicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1026	~"Registros e Informações em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1028	~"Vigilância em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1029	~"Cuidados de Idosos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1030	~"Necropsia",
                                               TRUE~"Ignorado"))

# CRIA BANCO DE TURMAS NO MARANHÃO ####
turmas_MA <- turmas %>% 
  filter(CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
           CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
           CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
           CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
           CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
           CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
           CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>% 
  filter(CO_UF==21) %>% 
  mutate(TP_MEDIACAO_DIDATICO_PEDAGO = case_when (TP_MEDIACAO_DIDATICO_PEDAGO==1 ~ "Presencial",
                                                  TP_MEDIACAO_DIDATICO_PEDAGO==2 ~ "Semipresencial",
                                                  TP_MEDIACAO_DIDATICO_PEDAGO==3 ~ "Educação a Distância - EAD",
                                                  TRUE~"Ignorado")) %>% 
  mutate(TP_ETAPA_ENSINO = case_when(TP_ETAPA_ENSINO == 1 ~ "Educação Infantil - Creche",
                                     TP_ETAPA_ENSINO == 2 ~ "Educação Infantil - Pré-escola",
                                     TP_ETAPA_ENSINO == 3 ~ "Educação Infantil - Unificada",
                                     TP_ETAPA_ENSINO == 56 ~ "Educação Infantil e Ensino Fundamental (9 anos) Multietapa",
                                     TP_ETAPA_ENSINO == 4 ~ "Ensino Fundamental de 8 anos - 1ª Série",
                                     TP_ETAPA_ENSINO == 5 ~ "Ensino Fundamental de 8 anos - 2ª Série",
                                     TP_ETAPA_ENSINO == 6 ~ "Ensino Fundamental de 8 anos - 3ª Série",
                                     TP_ETAPA_ENSINO == 7 ~ "Ensino Fundamental de 8 anos - 4ª Série",
                                     TP_ETAPA_ENSINO == 8 ~ "Ensino Fundamental de 8 anos - 5ª Série",
                                     TP_ETAPA_ENSINO == 9 ~ "Ensino Fundamental de 8 anos - 6ª Série",
                                     TP_ETAPA_ENSINO == 10 ~ "Ensino Fundamental de 8 anos - 7ª Série",
                                     TP_ETAPA_ENSINO == 11 ~ "Ensino Fundamental de 8 anos - 8ª Série",
                                     TP_ETAPA_ENSINO == 12 ~ "Ensino Fundamental de 8 anos - Multi",
                                     TP_ETAPA_ENSINO == 13 ~ "Ensino Fundamental de 8 anos - Correção de Fluxo",
                                     TP_ETAPA_ENSINO == 14 ~ "Ensino Fundamental de 9 anos - 1º Ano",
                                     TP_ETAPA_ENSINO == 15 ~ "Ensino Fundamental de 9 anos - 2º Ano",
                                     TP_ETAPA_ENSINO == 16 ~ "Ensino Fundamental de 9 anos - 3º Ano",
                                     TP_ETAPA_ENSINO == 17 ~ "Ensino Fundamental de 9 anos - 4º Ano",
                                     TP_ETAPA_ENSINO == 18 ~ "Ensino Fundamental de 9 anos - 5º Ano",
                                     TP_ETAPA_ENSINO == 19 ~ "Ensino Fundamental de 9 anos - 6º Ano",
                                     TP_ETAPA_ENSINO == 20 ~ "Ensino Fundamental de 9 anos - 7º Ano",
                                     TP_ETAPA_ENSINO == 21 ~ "Ensino Fundamental de 9 anos - 8º Ano",
                                     TP_ETAPA_ENSINO == 41 ~ "Ensino Fundamental de 9 anos - 9º Ano",
                                     TP_ETAPA_ENSINO == 22 ~ "Ensino Fundamental de 9 anos - Multi",
                                     TP_ETAPA_ENSINO == 23 ~ "Ensino Fundamental de 9 anos - Correção de Fluxo",
                                     TP_ETAPA_ENSINO == 24 ~ "Ensino Fundamental de 8 e 9 anos - Multi 8 e 9 anos",
                                     TP_ETAPA_ENSINO == 25 ~ "Ensino Médio - 1º ano/1ª Série",
                                     TP_ETAPA_ENSINO == 26 ~ "Ensino Médio - 2º ano/2ª Série",
                                     TP_ETAPA_ENSINO == 27 ~ "Ensino Médio - 3ºano/3ª Série",
                                     TP_ETAPA_ENSINO == 28 ~ "Ensino Médio - 4º ano/4ª Série",
                                     TP_ETAPA_ENSINO == 29 ~ "Ensino Médio - Não Seriada",
                                     TP_ETAPA_ENSINO==30 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série",
                                     TP_ETAPA_ENSINO==31 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série",
                                     TP_ETAPA_ENSINO==32 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série",
                                     TP_ETAPA_ENSINO==33 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série",
                                     TP_ETAPA_ENSINO==34 ~ "Curso Técnico Integrado (Ensino Médio Integrado) NÃ£o Seriada",
                                     TP_ETAPA_ENSINO==35 ~ "Ensino Médio - Modalidade Normal/Magistério 1ª Série",
                                     TP_ETAPA_ENSINO==36 ~ "Ensino Médio - Modalidade Normal/Magistério 2ª Série",
                                     TP_ETAPA_ENSINO==37 ~ "Ensino Médio - Modalidade Normal/Magistério 3ª Série",
                                     TP_ETAPA_ENSINO==38 ~ "Ensino Médio - Modalidade Normal/Magistério 4ª Série",
                                     TP_ETAPA_ENSINO==39 ~ "Curso Técnico - Concomitante",
                                     TP_ETAPA_ENSINO==40 ~ "Curso Técnico - Subsequente",
                                     TP_ETAPA_ENSINO==64 ~ "Curso Técnico Misto (Concomitante e Subsequente)",
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
  mutate(CO_CURSO_EDUC_PROFISSIONAL= case_when(CO_CURSO_EDUC_PROFISSIONAL==1001	~"Agente Comunitério de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1002	~"Análises clínicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1004	~"Citopatologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1006	~"Enfermagem",
                                               CO_CURSO_EDUC_PROFISSIONAL==1007	~"Equipamentos BiomÃ©dicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1009	~"Farmácia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1010	~"Gerência de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1011	~"Hemoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1012	~"Saúde Bucal",
                                               CO_CURSO_EDUC_PROFISSIONAL==1014	~"Imobilizações ortopédicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1015	~"Massoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1018	~"Nutrição e Dietética",
                                               CO_CURSO_EDUC_PROFISSIONAL==1020	~"Órteses e Próteses",
                                               CO_CURSO_EDUC_PROFISSIONAL==1022	~"Prótese dentária",
                                               CO_CURSO_EDUC_PROFISSIONAL==1023	~"Radiologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1024	~"Reabilitação de Dependentes Químicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1026	~"Registros e Informações em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1028	~"Vigilância em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1029	~"Cuidados de Idosos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1030	~"Necropsia",
                                               TRUE~"Ignorado"))

# CRIA BANCO DE TURMAS NO RIO GRANDE DO SUL ####
turmas_RS <- turmas %>% 
  filter(CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
           CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
           CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
           CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
           CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
           CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
           CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>% 
  filter(CO_UF==43) %>% 
  mutate(TP_MEDIACAO_DIDATICO_PEDAGO = case_when (TP_MEDIACAO_DIDATICO_PEDAGO==1 ~ "Presencial",
                                                  TP_MEDIACAO_DIDATICO_PEDAGO==2 ~ "Semipresencial",
                                                  TP_MEDIACAO_DIDATICO_PEDAGO==3 ~ "Educação a Distância - EAD",
                                                  TRUE~"Ignorado")) %>% 
  mutate(TP_ETAPA_ENSINO = case_when(TP_ETAPA_ENSINO == 1 ~ "Educação Infantil - Creche",
                                     TP_ETAPA_ENSINO == 2 ~ "Educação Infantil - Pré-escola",
                                     TP_ETAPA_ENSINO == 3 ~ "Educação Infantil - Unificada",
                                     TP_ETAPA_ENSINO == 56 ~ "Educação Infantil e Ensino Fundamental (9 anos) Multietapa",
                                     TP_ETAPA_ENSINO == 4 ~ "Ensino Fundamental de 8 anos - 1ª Série",
                                     TP_ETAPA_ENSINO == 5 ~ "Ensino Fundamental de 8 anos - 2ª Série",
                                     TP_ETAPA_ENSINO == 6 ~ "Ensino Fundamental de 8 anos - 3ª Série",
                                     TP_ETAPA_ENSINO == 7 ~ "Ensino Fundamental de 8 anos - 4ª Série",
                                     TP_ETAPA_ENSINO == 8 ~ "Ensino Fundamental de 8 anos - 5ª Série",
                                     TP_ETAPA_ENSINO == 9 ~ "Ensino Fundamental de 8 anos - 6ª Série",
                                     TP_ETAPA_ENSINO == 10 ~ "Ensino Fundamental de 8 anos - 7ª Série",
                                     TP_ETAPA_ENSINO == 11 ~ "Ensino Fundamental de 8 anos - 8ª Série",
                                     TP_ETAPA_ENSINO == 12 ~ "Ensino Fundamental de 8 anos - Multi",
                                     TP_ETAPA_ENSINO == 13 ~ "Ensino Fundamental de 8 anos - Correção de Fluxo",
                                     TP_ETAPA_ENSINO == 14 ~ "Ensino Fundamental de 9 anos - 1º Ano",
                                     TP_ETAPA_ENSINO == 15 ~ "Ensino Fundamental de 9 anos - 2º Ano",
                                     TP_ETAPA_ENSINO == 16 ~ "Ensino Fundamental de 9 anos - 3º Ano",
                                     TP_ETAPA_ENSINO == 17 ~ "Ensino Fundamental de 9 anos - 4º Ano",
                                     TP_ETAPA_ENSINO == 18 ~ "Ensino Fundamental de 9 anos - 5º Ano",
                                     TP_ETAPA_ENSINO == 19 ~ "Ensino Fundamental de 9 anos - 6º Ano",
                                     TP_ETAPA_ENSINO == 20 ~ "Ensino Fundamental de 9 anos - 7º Ano",
                                     TP_ETAPA_ENSINO == 21 ~ "Ensino Fundamental de 9 anos - 8º Ano",
                                     TP_ETAPA_ENSINO == 41 ~ "Ensino Fundamental de 9 anos - 9º Ano",
                                     TP_ETAPA_ENSINO == 22 ~ "Ensino Fundamental de 9 anos - Multi",
                                     TP_ETAPA_ENSINO == 23 ~ "Ensino Fundamental de 9 anos - Correção de Fluxo",
                                     TP_ETAPA_ENSINO == 24 ~ "Ensino Fundamental de 8 e 9 anos - Multi 8 e 9 anos",
                                     TP_ETAPA_ENSINO == 25 ~ "Ensino Médio - 1º ano/1ª Série",
                                     TP_ETAPA_ENSINO == 26 ~ "Ensino Médio - 2º ano/2ª Série",
                                     TP_ETAPA_ENSINO == 27 ~ "Ensino Médio - 3ºano/3ª Série",
                                     TP_ETAPA_ENSINO == 28 ~ "Ensino Médio - 4º ano/4ª Série",
                                     TP_ETAPA_ENSINO == 29 ~ "Ensino Médio - Não Seriada",
                                     TP_ETAPA_ENSINO==30 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série",
                                     TP_ETAPA_ENSINO==31 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série",
                                     TP_ETAPA_ENSINO==32 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série",
                                     TP_ETAPA_ENSINO==33 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série",
                                     TP_ETAPA_ENSINO==34 ~ "Curso Técnico Integrado (Ensino Médio Integrado) NÃ£o Seriada",
                                     TP_ETAPA_ENSINO==35 ~ "Ensino Médio - Modalidade Normal/Magistério 1ª Série",
                                     TP_ETAPA_ENSINO==36 ~ "Ensino Médio - Modalidade Normal/Magistério 2ª Série",
                                     TP_ETAPA_ENSINO==37 ~ "Ensino Médio - Modalidade Normal/Magistério 3ª Série",
                                     TP_ETAPA_ENSINO==38 ~ "Ensino Médio - Modalidade Normal/Magistério 4ª Série",
                                     TP_ETAPA_ENSINO==39 ~ "Curso Técnico - Concomitante",
                                     TP_ETAPA_ENSINO==40 ~ "Curso Técnico - Subsequente",
                                     TP_ETAPA_ENSINO==64 ~ "Curso Técnico Misto (Concomitante e Subsequente)",
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
  mutate(CO_CURSO_EDUC_PROFISSIONAL= case_when(CO_CURSO_EDUC_PROFISSIONAL==1001	~"Agente Comunitério de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1002	~"Análises clínicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1004	~"Citopatologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1006	~"Enfermagem",
                                               CO_CURSO_EDUC_PROFISSIONAL==1007	~"Equipamentos BiomÃ©dicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1009	~"Farmácia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1010	~"Gerência de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1011	~"Hemoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1012	~"Saúde Bucal",
                                               CO_CURSO_EDUC_PROFISSIONAL==1014	~"Imobilizações ortopédicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1015	~"Massoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1018	~"Nutrição e Dietética",
                                               CO_CURSO_EDUC_PROFISSIONAL==1020	~"Órteses e Próteses",
                                               CO_CURSO_EDUC_PROFISSIONAL==1022	~"Prótese dentária",
                                               CO_CURSO_EDUC_PROFISSIONAL==1023	~"Radiologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1024	~"Reabilitação de Dependentes Químicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1026	~"Registros e Informações em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1028	~"Vigilância em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1029	~"Cuidados de Idosos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1030	~"Necropsia",
                                               TRUE~"Ignorado"))

# CRIA BANCO DE TURMAS EM PERNAMBUCO ####
turmas_PE <- turmas %>% 
  filter(CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
           CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
           CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
           CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
           CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
           CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
           CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>% 
  filter(CO_UF==26) %>% 
  mutate(TP_MEDIACAO_DIDATICO_PEDAGO = case_when (TP_MEDIACAO_DIDATICO_PEDAGO==1 ~ "Presencial",
                                                  TP_MEDIACAO_DIDATICO_PEDAGO==2 ~ "Semipresencial",
                                                  TP_MEDIACAO_DIDATICO_PEDAGO==3 ~ "Educação a Distância - EAD",
                                                  TRUE~"Ignorado")) %>% 
  mutate(TP_ETAPA_ENSINO = case_when(TP_ETAPA_ENSINO == 1 ~ "Educação Infantil - Creche",
                                     TP_ETAPA_ENSINO == 2 ~ "Educação Infantil - Pré-escola",
                                     TP_ETAPA_ENSINO == 3 ~ "Educação Infantil - Unificada",
                                     TP_ETAPA_ENSINO == 56 ~ "Educação Infantil e Ensino Fundamental (9 anos) Multietapa",
                                     TP_ETAPA_ENSINO == 4 ~ "Ensino Fundamental de 8 anos - 1ª Série",
                                     TP_ETAPA_ENSINO == 5 ~ "Ensino Fundamental de 8 anos - 2ª Série",
                                     TP_ETAPA_ENSINO == 6 ~ "Ensino Fundamental de 8 anos - 3ª Série",
                                     TP_ETAPA_ENSINO == 7 ~ "Ensino Fundamental de 8 anos - 4ª Série",
                                     TP_ETAPA_ENSINO == 8 ~ "Ensino Fundamental de 8 anos - 5ª Série",
                                     TP_ETAPA_ENSINO == 9 ~ "Ensino Fundamental de 8 anos - 6ª Série",
                                     TP_ETAPA_ENSINO == 10 ~ "Ensino Fundamental de 8 anos - 7ª Série",
                                     TP_ETAPA_ENSINO == 11 ~ "Ensino Fundamental de 8 anos - 8ª Série",
                                     TP_ETAPA_ENSINO == 12 ~ "Ensino Fundamental de 8 anos - Multi",
                                     TP_ETAPA_ENSINO == 13 ~ "Ensino Fundamental de 8 anos - Correção de Fluxo",
                                     TP_ETAPA_ENSINO == 14 ~ "Ensino Fundamental de 9 anos - 1º Ano",
                                     TP_ETAPA_ENSINO == 15 ~ "Ensino Fundamental de 9 anos - 2º Ano",
                                     TP_ETAPA_ENSINO == 16 ~ "Ensino Fundamental de 9 anos - 3º Ano",
                                     TP_ETAPA_ENSINO == 17 ~ "Ensino Fundamental de 9 anos - 4º Ano",
                                     TP_ETAPA_ENSINO == 18 ~ "Ensino Fundamental de 9 anos - 5º Ano",
                                     TP_ETAPA_ENSINO == 19 ~ "Ensino Fundamental de 9 anos - 6º Ano",
                                     TP_ETAPA_ENSINO == 20 ~ "Ensino Fundamental de 9 anos - 7º Ano",
                                     TP_ETAPA_ENSINO == 21 ~ "Ensino Fundamental de 9 anos - 8º Ano",
                                     TP_ETAPA_ENSINO == 41 ~ "Ensino Fundamental de 9 anos - 9º Ano",
                                     TP_ETAPA_ENSINO == 22 ~ "Ensino Fundamental de 9 anos - Multi",
                                     TP_ETAPA_ENSINO == 23 ~ "Ensino Fundamental de 9 anos - Correção de Fluxo",
                                     TP_ETAPA_ENSINO == 24 ~ "Ensino Fundamental de 8 e 9 anos - Multi 8 e 9 anos",
                                     TP_ETAPA_ENSINO == 25 ~ "Ensino Médio - 1º ano/1ª Série",
                                     TP_ETAPA_ENSINO == 26 ~ "Ensino Médio - 2º ano/2ª Série",
                                     TP_ETAPA_ENSINO == 27 ~ "Ensino Médio - 3ºano/3ª Série",
                                     TP_ETAPA_ENSINO == 28 ~ "Ensino Médio - 4º ano/4ª Série",
                                     TP_ETAPA_ENSINO == 29 ~ "Ensino Médio - Não Seriada",
                                     TP_ETAPA_ENSINO==30 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série",
                                     TP_ETAPA_ENSINO==31 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série",
                                     TP_ETAPA_ENSINO==32 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série",
                                     TP_ETAPA_ENSINO==33 ~ "Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série",
                                     TP_ETAPA_ENSINO==34 ~ "Curso Técnico Integrado (Ensino Médio Integrado) NÃ£o Seriada",
                                     TP_ETAPA_ENSINO==35 ~ "Ensino Médio - Modalidade Normal/Magistério 1ª Série",
                                     TP_ETAPA_ENSINO==36 ~ "Ensino Médio - Modalidade Normal/Magistério 2ª Série",
                                     TP_ETAPA_ENSINO==37 ~ "Ensino Médio - Modalidade Normal/Magistério 3ª Série",
                                     TP_ETAPA_ENSINO==38 ~ "Ensino Médio - Modalidade Normal/Magistério 4ª Série",
                                     TP_ETAPA_ENSINO==39 ~ "Curso Técnico - Concomitante",
                                     TP_ETAPA_ENSINO==40 ~ "Curso Técnico - Subsequente",
                                     TP_ETAPA_ENSINO==64 ~ "Curso Técnico Misto (Concomitante e Subsequente)",
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
  mutate(CO_CURSO_EDUC_PROFISSIONAL= case_when(CO_CURSO_EDUC_PROFISSIONAL==1001	~"Agente Comunitério de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1002	~"Análises clínicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1004	~"Citopatologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1006	~"Enfermagem",
                                               CO_CURSO_EDUC_PROFISSIONAL==1007	~"Equipamentos BiomÃ©dicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1009	~"Farmácia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1010	~"Gerência de Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1011	~"Hemoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1012	~"Saúde Bucal",
                                               CO_CURSO_EDUC_PROFISSIONAL==1014	~"Imobilizações ortopédicas",
                                               CO_CURSO_EDUC_PROFISSIONAL==1015	~"Massoterapia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1018	~"Nutrição e Dietética",
                                               CO_CURSO_EDUC_PROFISSIONAL==1020	~"Órteses e Próteses",
                                               CO_CURSO_EDUC_PROFISSIONAL==1022	~"Prótese dentária",
                                               CO_CURSO_EDUC_PROFISSIONAL==1023	~"Radiologia",
                                               CO_CURSO_EDUC_PROFISSIONAL==1024	~"Reabilitação de Dependentes Químicos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1026	~"Registros e Informações em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1028	~"Vigilância em Saúde",
                                               CO_CURSO_EDUC_PROFISSIONAL==1029	~"Cuidados de Idosos",
                                               CO_CURSO_EDUC_PROFISSIONAL==1030	~"Necropsia",
                                               TRUE~"Ignorado"))

# CRIA TABELA DE TURMAS PARA RONDÔNIA ####
tb_turmas_RO <- data.frame(var = "Tipo de mediação didático-pedagógica", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(turmas_RO %>% count(var = TP_MEDIACAO_DIDATICO_PEDAGO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(turmas_RO %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(turmas_RO %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# CRIA TABELA DE TURMAS PARA MARANHÃO ####  
tb_turmas_MA <- data.frame(var = "Tipo de mediação didático-pedagógica", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(turmas_MA %>% count(var = TP_MEDIACAO_DIDATICO_PEDAGO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(turmas_MA %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(turmas_MA %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# CRIA TABELA DE TURMAS PARA GOIÁS ####
tb_turmas_GO <- data.frame(var = "Tipo de mediação didático-pedagógica", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(turmas_GO %>% count(var = TP_MEDIACAO_DIDATICO_PEDAGO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(turmas_GO %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(turmas_GO %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# CRIA TABELA DE TURMAS PARA RIO GRANDE DO SUL ####
tb_turmas_RS <- data.frame(var = "Tipo de mediação didático-pedagógica", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(turmas_RS %>% count(var = TP_MEDIACAO_DIDATICO_PEDAGO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(turmas_RS %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(turmas_RS %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# CRIA TABELA DE TURMAS PARA PERNAMBUCO ####
tb_turmas_PE <- data.frame(var = "Tipo de mediação didático-pedagógica", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(turmas_PE %>% count(var = TP_MEDIACAO_DIDATICO_PEDAGO) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>%
  add_row(turmas_PE %>% count(var = TP_ETAPA_ENSINO)%>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1))) %>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>%
  add_row(turmas_PE %>% count(var = CO_CURSO_EDUC_PROFISSIONAL) %>% arrange(desc(n)) %>% mutate(prop = round((n / sum(n))*100, 1)))

# IDENTIFICA ID DAS TURMAS COM CURSO TÉCNICO DA SAÚDE EM RONDÔNIA (TOTAL E POR CURSO) ####
id_turmas_RO <- turmas_RO %>% count(CO_ENTIDADE)
id_turmas_RO_enf <- turmas_RO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(CO_ENTIDADE) %>% mutate(n="Enfermagem") %>% rename (curso=n)
id_turmas_RO_nut <- turmas_RO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(CO_ENTIDADE)%>% mutate(n="Nutrição e Dietética") %>% rename (curso=n)
id_turmas_RO_rad<- turmas_RO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(CO_ENTIDADE) %>% mutate(n="Radiologia") %>% rename (curso=n)
id_turmas_RO_anal_cli <- turmas_RO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(CO_ENTIDADE) %>% mutate(n="Análises clínicas") %>% rename (curso=n)
id_turmas_RO_prot <- turmas_RO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Prótese dentária") %>% count(CO_ENTIDADE) %>% mutate(n="Prótese dentária") %>% rename (curso=n)

# IDENTIFICA ID DAS TURMAS COM CURSO TÉCNICO DA SAÚDE EM MARANHÃO (TOTAL E POR CURSO) ####
id_turmas_MA <- turmas_MA %>% count(CO_ENTIDADE)
id_turmas_MA_enf <- turmas_MA %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(CO_ENTIDADE) %>% mutate(n="Enfermagem") %>% rename (curso=n)
id_turmas_MA_anal_cli <- turmas_MA %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(CO_ENTIDADE) %>% mutate(n="Análises clínicas") %>% rename (curso=n)
id_turmas_MA_equip_biom <- turmas_MA %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Equipamentos BiomÃ©dicos") %>% count(CO_ENTIDADE) %>% mutate(n="Equipamentos BiomÃ©dicos") %>% rename (curso=n)
id_turmas_MA_far <- turmas_MA %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(CO_ENTIDADE) %>% mutate(n="Farmácia") %>% rename (curso=n)
id_turmas_MA_ger_sau <- turmas_MA %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(CO_ENTIDADE) %>% mutate(n="Gerência de Saúde") %>% rename (curso=n)
id_turmas_MA_nut <- turmas_MA %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(CO_ENTIDADE) %>% mutate(n="Nutrição e Dietética") %>% rename (curso=n)
id_turmas_MA_radio <- turmas_MA %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(CO_ENTIDADE) %>% mutate(n="Radiologia") %>% rename (curso=n)
id_turmas_MA_reg <- turmas_MA %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Registros e Informações em Saúde") %>% count(CO_ENTIDADE) %>% mutate(n="Registros e Informações em Saúde") %>% rename (curso=n)
id_turmas_MA_sbucal <- turmas_MA %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(CO_ENTIDADE) %>% mutate(n="Saúde Bucal") %>% rename (curso=n)

# IDENTIFICA ID DAS TURMAS COM CURSO TÉCNICO DA SAÚDE EM GOIÁS (TOTAL E POR CURSO) ####
id_turmas_GO <- turmas_GO %>% count(CO_ENTIDADE)
id_turmas_GO_enf <- turmas_GO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(CO_ENTIDADE) %>% mutate(n="Enfermagem") %>% rename (curso=n)
id_turmas_GO_anal_cli <- turmas_GO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(CO_ENTIDADE) %>% mutate(n="Análises clínicas") %>% rename (curso=n)
id_turmas_GO_radio <- turmas_GO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(CO_ENTIDADE) %>% mutate(n="Radiologia") %>% rename (curso=n)
id_turmas_GO_sbucal <- turmas_GO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(CO_ENTIDADE) %>% mutate(n="Saúde Bucal") %>% rename (curso=n)
id_turmas_GO_nut <- turmas_GO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(CO_ENTIDADE) %>% mutate(n="Nutrição e Dietética") %>% rename (curso=n)
id_turmas_GO_prot <- turmas_GO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Prótese dentária") %>% count(CO_ENTIDADE) %>% mutate(n="Prótese dentária") %>% rename (curso=n)
id_turmas_GO_vig <- turmas_GO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Vigilância em Saúde") %>% count(CO_ENTIDADE) %>% mutate(n="Vigilância em Saúde") %>% rename (curso=n)
id_turmas_GO_necro <- turmas_GO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Necropsia") %>% count(CO_ENTIDADE) %>% mutate(n="Necropsia") %>% rename (curso=n)
id_turmas_GO_far <- turmas_GO %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(CO_ENTIDADE) %>% mutate(n="Farmácia") %>% rename (curso=n)

# IDENTIFICA ID DAS TURMAS COM CURSO TÉCNICO DA SAÚDE EM RIO GRANDE DO SUL (TOTAL E POR CURSO) ####
id_turmas_RS <- turmas_RS %>% count(CO_ENTIDADE)
id_turmas_RS_enf <- turmas_RS %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(CO_ENTIDADE) %>% mutate(n="Enfermagem") %>% rename (curso=n)
id_turmas_RS_radio <- turmas_RS %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(CO_ENTIDADE) %>% mutate(n="Radiologia") %>% rename (curso=n)
id_turmas_RS_nut <- turmas_RS %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(CO_ENTIDADE) %>% mutate(n="Nutrição e Dietética") %>% rename (curso=n)
id_turmas_RS_anal_cli <- turmas_RS %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(CO_ENTIDADE) %>% mutate(n="Análises clínicas") %>% rename (curso=n)
id_turmas_RS_prot <- turmas_RS %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Prótese dentária") %>% count(CO_ENTIDADE) %>% mutate(n="Prótese dentária") %>% rename (curso=n)
id_turmas_RS_far <- turmas_RS %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(CO_ENTIDADE) %>% mutate(n="Farmácia") %>% rename (curso=n)
id_turmas_RS_ger <- turmas_RS %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(CO_ENTIDADE) %>% mutate(n="Gerência de Saúde") %>% rename (curso=n)
id_turmas_RS_mass <- turmas_RS %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Massoterapia") %>% count(CO_ENTIDADE) %>% mutate(n="Massoterapia") %>% rename (curso=n)
id_turmas_RS_sbucal <- turmas_RS %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(CO_ENTIDADE) %>% mutate(n="Saúde Bucal") %>% rename (curso=n)
id_turmas_RS_cuidoso <- turmas_RS %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Cuidados de Idosos") %>% count(CO_ENTIDADE) %>% mutate(n="Cuidados de Idosos") %>% rename (curso=n)
id_turmas_RS_necro <- turmas_RS %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Citopatologia") %>% count(CO_ENTIDADE) %>% mutate(n="Citopatologia") %>% rename (curso=n)

# IDENTIFICA ID DAS TURMAS COM CURSO TÉCNICO DA SAÚDE EM PERNAMBUCO (TOTAL E POR CURSO) ####
id_turmas_PE <- turmas_PE %>% count(CO_ENTIDADE)
id_turmas_PE_enf <- turmas_PE %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Enfermagem") %>% count(CO_ENTIDADE) %>% mutate(n="Enfermagem") %>% rename (curso=n)
id_turmas_PE_radio <- turmas_PE %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Radiologia") %>% count(CO_ENTIDADE) %>% mutate(n="Radiologia") %>% rename (curso=n)
id_turmas_PE_anal_cli <- turmas_PE %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Análises clínicas") %>% count(CO_ENTIDADE) %>% mutate(n="Análises clínicas") %>% rename (curso=n)
id_turmas_PE_nut <- turmas_PE %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Nutrição e Dietética") %>% count(CO_ENTIDADE) %>% mutate(n="Nutrição e Dietética") %>% rename (curso=n)
id_turmas_PE_far <- turmas_PE %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Farmácia") %>% count(CO_ENTIDADE) %>% mutate(n="Farmácia") %>% rename (curso=n)
id_turmas_PE_sbucal <- turmas_PE %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Saúde Bucal") %>% count(CO_ENTIDADE) %>% mutate(n="Saúde Bucal") %>% rename (curso=n)
id_turmas_PE_vig <- turmas_PE %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Vigilância em Saúde") %>% count(CO_ENTIDADE) %>% mutate(n="Vigilância em Saúde") %>% rename (curso=n)
id_turmas_PE_ger <- turmas_PE %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Gerência de Saúde") %>% count(CO_ENTIDADE) %>% mutate(n="Gerência de Saúde") %>% rename (curso=n)
id_turmas_PE_hemo <- turmas_PE %>% filter (CO_CURSO_EDUC_PROFISSIONAL=="Hemoterapia") %>% count(CO_ENTIDADE) %>% mutate(n="Hemoterapia") %>% rename (curso=n)

# FILTRAR BASE DE TURMAS PARA OS CURSOS PROFISSIONAIS E ESTADOS DE INTERESSE DO PROJETO ####
id_escolas_proj <- turmas %>% 
  filter(CO_CURSO_EDUC_PROFISSIONAL==1001 | CO_CURSO_EDUC_PROFISSIONAL==1002 | CO_CURSO_EDUC_PROFISSIONAL==1004 | 
           CO_CURSO_EDUC_PROFISSIONAL==1006 | CO_CURSO_EDUC_PROFISSIONAL==1007 | CO_CURSO_EDUC_PROFISSIONAL==1009 | 
           CO_CURSO_EDUC_PROFISSIONAL==1010 | CO_CURSO_EDUC_PROFISSIONAL==1011 | CO_CURSO_EDUC_PROFISSIONAL==1012 | 
           CO_CURSO_EDUC_PROFISSIONAL==1014 | CO_CURSO_EDUC_PROFISSIONAL==1015 | CO_CURSO_EDUC_PROFISSIONAL==1018 | 
           CO_CURSO_EDUC_PROFISSIONAL==1020 | CO_CURSO_EDUC_PROFISSIONAL==1022 | CO_CURSO_EDUC_PROFISSIONAL==1023 | 
           CO_CURSO_EDUC_PROFISSIONAL==1024 | CO_CURSO_EDUC_PROFISSIONAL==1026 | CO_CURSO_EDUC_PROFISSIONAL==1028 | 
           CO_CURSO_EDUC_PROFISSIONAL==1029 | CO_CURSO_EDUC_PROFISSIONAL==1030) %>% 
  filter(CO_UF==11 | CO_UF==21 | CO_UF == 52 | CO_UF == 43 | CO_UF == 26) %>%
  count(CO_ENTIDADE)

# FILTRAR BASE DE ESCOLAS COM BASE NO ID DAS TURMAS ####
escolas_proj <- left_join (id_escolas_proj, 
                           escolas, by= c("CO_ENTIDADE" = "CO_ENTIDADE")) %>% 
  select(c("CO_ENTIDADE", "NO_ENTIDADE", "CO_UF"))

# EXPORTAR LISTA DE ESCOLAS DO PROJETO ####
wb = createWorkbook()
addWorksheet(wb, sheetName = "ID_Escolas")
writeData(wb, sheet = "ID_Escolas", x = escolas_proj) 
saveWorkbook(wb, "C:/Users/laisr/OneDrive/Documentos/Área de Trabalho/CENSO ESCOLAR/analise/github/tecnicos_saude/resultados/escolas_RO_MA_GO_RS_PE.xlsx", overwrite = T)

rm(escolas_proj, turmas, escolas)
