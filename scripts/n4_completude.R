# CARREGAR BIBLIOTECAS ####
library(data.table)
library(tidyverse)

setwd("C:/Users/laisr/OneDrive/Documentos/Área de Trabalho/CENSO ESCOLAR/analise/github/dados")

# IMPORTA MATRÍCULAS DO CENTRO-OESTE E CONTA NA's (NOT AVAIABLE) EM 15 PARTES ####
# VARIÁVEIS USADAS NO DESCRITIVO
matriculas_codesc <- fread("matricula_co.csv", select=c("TP_SEXO","TP_COR_RACA",
                                                      "CO_UF","TP_ETAPA_ENSINO",
                                                      "CO_CURSO_EDUC_PROFISSIONAL",
                                                      "IN_PROFISSIONALIZANTE"))
tbcompletude_codesc <- data.frame(var = "Sexo",`n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_codesc %>% filter(is.na(TP_SEXO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))  %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_codesc %>% filter( TP_COR_RACA ==0| is.na(TP_COR_RACA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>% 
  
  add_row(var = "Unidade da Federação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_codesc %>% filter( CO_UF ==0| is.na(CO_UF)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_codesc %>% filter(is.na(TP_ETAPA_ENSINO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_codesc %>% filter (TP_ETAPA_ENSINO==30 | TP_ETAPA_ENSINO==31 | TP_ETAPA_ENSINO==32 |
                                        TP_ETAPA_ENSINO==33 | TP_ETAPA_ENSINO==34 | TP_ETAPA_ENSINO==35 |
                                        TP_ETAPA_ENSINO==36 | TP_ETAPA_ENSINO==37 | TP_ETAPA_ENSINO==38 |
                                        TP_ETAPA_ENSINO==39 | TP_ETAPA_ENSINO==40 | TP_ETAPA_ENSINO==65 |
                                        TP_ETAPA_ENSINO==67 | TP_ETAPA_ENSINO==68 | TP_ETAPA_ENSINO==73 |
                                        TP_ETAPA_ENSINO==74 | IN_PROFISSIONALIZANTE==1) %>% 
            filter(is.na(CO_CURSO_EDUC_PROFISSIONAL)==T) %>% count() %>% mutate(n = 95969-n)%>% mutate(prop = round((n)/95969*100, 1)))
rm(matriculas_codesc)

# OUTRAS VARIÁVEIS
matriculas_co1 <- fread("matricula_co.csv", select=c("ID_ALUNO","ID_MATRICULA","NU_MES","NU_ANO"))
tbcompletude_co1 <- data.frame(var = "Código do aluno (ID_INEP)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co1 %>% filter(is.na(ID_ALUNO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Código único da matrícula", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co1 %>% filter(is.na(ID_MATRICULA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Data de nascimento do aluno - mês", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co1 %>% filter(is.na(NU_MES)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Data de nascimento do aluno - ano", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co1 %>% filter(is.na(NU_ANO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))
rm(matriculas_co1)

matriculas_co2 <- fread("matricula_co.csv", select=c("NU_IDADE_REFERENCIA","NU_IDADE","TP_SEXO","TP_COR_RACA","TP_NACIONALIDADE"))
tbcompletude_co2 <- data.frame(var = "Idade do aluno no mês de referência do Censo Escolar (31 de maio)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co2 %>% filter(is.na(NU_IDADE_REFERENCIA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Idade calculada pelo ano de nascimento do aluno", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co2 %>% filter(is.na(NU_IDADE)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Sexo", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co2 %>% filter(is.na(TP_SEXO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Cor/raça", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co2 %>% filter(is.na(TP_COR_RACA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Nacionalidade", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co2 %>% filter(is.na(TP_NACIONALIDADE)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))
rm(matriculas_co2)

matriculas_co3 <- fread("matricula_co.csv", select=c("CO_PAIS_ORIGEM","CO_UF_NASC","CO_MUNICIPIO_NASC", "CO_UF_END"))
tbcompletude_co3 <- data.frame(var = "Código País da nacionalidade", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co3 %>% filter(is.na(CO_PAIS_ORIGEM)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Código UF de nascimento", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co3 %>% filter(is.na(CO_UF_NASC)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Código Município de nascimento", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co3 %>% filter(is.na(CO_MUNICIPIO_NASC)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Código UF de residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co3 %>% filter(is.na(CO_UF_END)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))

rm(matriculas_co3)

matriculas_co4 <- fread("matricula_co.csv", select=c("CO_MUNICIPIO_END","TP_ZONA_RESIDENCIAL","TP_LOCAL_RESID_DIFERENCIADA","IN_NECESSIDADE_ESPECIAL","IN_BAIXA_VISAO"))
tbcompletude_co4 <- data.frame(var = "Código Município de residência", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co4 %>% filter(is.na(CO_MUNICIPIO_END)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Localização/Zona de residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co4 %>% filter(is.na(TP_ZONA_RESIDENCIAL)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Localização diferenciada da residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co4 %>% filter(is.na(TP_LOCAL_RESID_DIFERENCIADA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Aluno (a) com deficiência, transtorno do espectro autista ou altas habilidades/superdotação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co4 %>% filter(is.na(IN_NECESSIDADE_ESPECIAL)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Baixa visão", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co4 %>% filter(is.na(IN_BAIXA_VISAO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))
rm(matriculas_co4)

matriculas_co5 <- fread("matricula_co.csv", select=c("IN_CEGUEIRA","IN_DEF_AUDITIVA","IN_DEF_FISICA","IN_DEF_INTELECTUAL","IN_SURDEZ"))
tbcompletude_co5 <- data.frame(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Cegueira", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co5 %>% filter(is.na(IN_CEGUEIRA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação (deficiência auditiva)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co5 %>% filter(is.na(IN_DEF_AUDITIVA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação  - Deficiência física", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co5 %>% filter(is.na(IN_DEF_FISICA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Deficiência intelectual", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co5 %>% filter(is.na(IN_DEF_INTELECTUAL)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Surdez", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co5 %>% filter(is.na(IN_SURDEZ)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))
rm(matriculas_co5)

matriculas_co6 <- fread("matricula_co.csv", select=c("IN_SURDOCEGUEIRA","IN_DEF_MULTIPLA","IN_AUTISMO","IN_SUPERDOTACAO","IN_RECURSO_LEDOR"))
tbcompletude_co6 <- data.frame(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Surdocegueira", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co6 %>% filter(is.na(IN_SURDOCEGUEIRA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Deficiência múltipla", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co6 %>% filter(is.na(IN_DEF_MULTIPLA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Transtorno do Espectro Autista", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co6 %>% filter(is.na(IN_AUTISMO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação  - Altas habilidades/superdotação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co6 %>% filter(is.na(IN_SUPERDOTACAO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Auxílio Ledor", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co6 %>% filter(is.na(IN_RECURSO_LEDOR)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))
rm(matriculas_co6)

matriculas_co7 <- fread("matricula_co.csv", select=c("IN_RECURSO_TRANSCRICAO","IN_RECURSO_INTERPRETE","IN_RECURSO_LIBRAS","IN_RECURSO_LABIAL","IN_RECURSO_AMPLIADA_18"))
tbcompletude_co7 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Auxílio Transcrição", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co7 %>% filter(is.na(IN_RECURSO_TRANSCRICAO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Guia-Intérprete", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co7 %>% filter(is.na(IN_RECURSO_INTERPRETE)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Tradutor e Intérprete de Libras", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co7 %>% filter(is.na(IN_RECURSO_LIBRAS)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Leitura Labial", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co7 %>% filter(is.na(IN_RECURSO_LABIAL)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova Ampliada (Fonte tamanho 18)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co7 %>% filter(is.na(IN_RECURSO_AMPLIADA_18)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))
rm(matriculas_co7)

matriculas_co8 <- fread("matricula_co.csv", select=c("IN_RECURSO_AMPLIADA_24","IN_RECURSO_CD_AUDIO","IN_RECURSO_PROVA_PORTUGUES","IN_RECURSO_VIDEO_LIBRAS","IN_RECURSO_BRAILLE"))
tbcompletude_co8 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova superampliada (Fonte tamanho 24)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co8 %>% filter(is.na(IN_RECURSO_AMPLIADA_24)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - CD com áudio para deficiente visual", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co8 %>% filter(is.na(IN_RECURSO_CD_AUDIO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova de Língua Portuguesa como segunda língua para surdos e deficientes auditivos", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co8 %>% filter(is.na(IN_RECURSO_PROVA_PORTUGUES)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova em vídeo Libras", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co8 %>% filter(is.na(IN_RECURSO_VIDEO_LIBRAS)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Material didático e Prova em Braille", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co8 %>% filter(is.na(IN_RECURSO_BRAILLE)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))
rm(matriculas_co8)

matriculas_co9 <- fread("matricula_co.csv", select=c("IN_RECURSO_NENHUM","IN_AEE_LIBRAS","IN_AEE_LINGUA_PORTUGUESA","IN_AEE_INFORMATICA_ACESSIVEL","IN_AEE_BRAILLE"))
tbcompletude_co9 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Nenhum", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co9 %>% filter(is.na(IN_RECURSO_NENHUM)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da Língua Brasileira de Sinais - LIBRAS", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co9 %>% filter(is.na(IN_AEE_LIBRAS)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da Língua Portuguesa como segunda língua", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co9 %>% filter(is.na(IN_AEE_LINGUA_PORTUGUESA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da informática acessível", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co9 %>% filter(is.na(IN_AEE_INFORMATICA_ACESSIVEL)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do Sistema Braille", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co9 %>% filter(is.na(IN_AEE_BRAILLE)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))
rm(matriculas_co9)

matriculas_co10 <- fread("matricula_co.csv", select=c("IN_AEE_CAA","IN_AEE_SOROBAN","IN_AEE_VIDA_AUTONOMA","IN_AEE_OPTICOS_NAO_OPTICOS","IN_AEE_ENRIQ_CURRICULAR"))
tbcompletude_co10 <- data.frame(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do uso da Comunicação Alternativa e Aumentativa (CAA)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co10 %>% filter(is.na(IN_AEE_CAA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino das técnicas do cálculo no Soroban", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co10 %>% filter(is.na(IN_AEE_SOROBAN)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Desenvolvimento de vida autônoma", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co10 %>% filter(is.na(IN_AEE_VIDA_AUTONOMA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do uso de recursos ópticos e não ópticos", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co10 %>% filter(is.na(IN_AEE_OPTICOS_NAO_OPTICOS)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Enriquecimento curricular", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co10 %>% filter(is.na(IN_AEE_ENRIQ_CURRICULAR)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))
rm(matriculas_co10)

matriculas_co11 <- fread("matricula_co.csv", select=c("IN_AEE_DESEN_COGNITIVO","IN_AEE_MOBILIDADE","TP_OUTRO_LOCAL_AULA","IN_TRANSPORTE_PUBLICO","TP_RESPONSAVEL_TRANSPORTE"))
tbcompletude_co11 <- data.frame(var = "Tipo de Atendimento Educacional Especializado (AEE) - Desenvolvimento de funções cognitivas", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co11 %>% filter(is.na(IN_AEE_DESEN_COGNITIVO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino de técnicas para orientação e mobilidade", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co11 %>% filter(is.na(IN_AEE_MOBILIDADE)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Recebe escolarização em outro local (diferente da escola)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co11 %>% filter(is.na(TP_OUTRO_LOCAL_AULA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Transporte escolar público", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co11 %>% filter(is.na(IN_TRANSPORTE_PUBLICO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Poder público responsável pelo transporte escolar", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co11 %>% filter(is.na(TP_RESPONSAVEL_TRANSPORTE)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))
rm(matriculas_co11)

matriculas_co12 <- fread("matricula_co.csv", select=c("IN_TRANSP_BICICLETA","IN_TRANSP_MICRO_ONIBUS","IN_TRANSP_ONIBUS","IN_TRANSP_TR_ANIMAL","IN_TRANSP_VANS_KOMBI"))
tbcompletude_co12 <- data.frame(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Bicicleta)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co12 %>% filter(is.na(IN_TRANSP_BICICLETA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Micro-ônibus)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co12 %>% filter(is.na(IN_TRANSP_MICRO_ONIBUS)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Ônibus)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co12 %>% filter(is.na(IN_TRANSP_ONIBUS)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público -  Rodoviário (Tração Animal)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co12 %>% filter(is.na(IN_TRANSP_TR_ANIMAL)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Vans/Kombi)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co12 %>% filter(is.na(IN_TRANSP_VANS_KOMBI)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))
rm(matriculas_co12)

matriculas_co13 <- fread("matricula_co.csv", select=c("IN_TRANSP_OUTRO_VEICULO","IN_TRANSP_EMBAR_ATE5","IN_TRANSP_EMBAR_5A15","IN_TRANSP_EMBAR_15A35","IN_TRANSP_EMBAR_35"))
tbcompletude_co13 <- data.frame(var = "Tipo de veículo utilizado no transporte escolar público -  Rodoviário (Outro tipo de veículo rodoviário)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co13 %>% filter(is.na(IN_TRANSP_OUTRO_VEICULO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de até 5 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co13 %>% filter(is.na(IN_TRANSP_EMBAR_ATE5)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de 5 a 15 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co13 %>% filter(is.na(IN_TRANSP_EMBAR_5A15)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de 15 a 35 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co13 %>% filter(is.na(IN_TRANSP_EMBAR_15A35)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade acima de 35 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co13 %>% filter(is.na(IN_TRANSP_EMBAR_35)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))

rm(matriculas_co13)

matriculas_co14 <- fread("matricula_co.csv", select=c("TP_ETAPA_ENSINO","IN_ESPECIAL_EXCLUSIVA","IN_REGULAR","IN_EJA","IN_PROFISSIONALIZANTE"))
tbcompletude_co14 <- data.frame(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_co14 %>% filter(is.na(TP_ETAPA_ENSINO)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1))) %>% 
  add_row(var = "Aluno de turma exclusiva de alunos com deficiência, transtorno do espectro autista ou altas habilidades/superdotação (Classes Especiais)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co14 %>% filter(is.na(IN_ESPECIAL_EXCLUSIVA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas com etapas de escolarização consecutivas, Creche ao Ensino Médio. Etapas consideradas (nas antigas modalidades 1 ou 2): TP_ETAPA_ENSINO igual a 1,2,4,5,6,7,8,9,10,11,14, 15,16,17,18,19,20,21,41,25,26,27,28,29,30,31, 32,33,34,35,36,37 ou 38.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co14 %>% filter(is.na(IN_REGULAR)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas destinadas a pessoas que não cursaram o ensino fundamental e/ou médio em idade própria. Etapas consideradas (nas antigas modalidades 2 ou 3): TP_ETAPA_ENSINO igual a 65,67,69,70,71,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co14 %>% filter(is.na(IN_EJA)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))%>%
  add_row(var = "Modo profissionalizante de ensino correspondente às turmas de cursos de formação inicial e continuada ou de qualificação profissional (Cursos FIC) articulados à EJA ou concomitantes; ou de cursos técnicos de nível médio nas formas articulada (integrada ou concomitante) ou subsequente ao ensino médio e de normal/magistério. Etapas consideradas (nas antigas modalidades 1, 2 ou 3): TP_ETAPA_ENSINO igual a 30,31,32,33,34, 35,36,37,38,39,40,65,67,68,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_co14 %>% filter(is.na(IN_PROFISSIONALIZANTE)==T) %>% count() %>% mutate(n = 3804465-n)%>% mutate(prop = round((n)/3804465*100, 1)))
rm(matriculas_co14)

# JUNTA AS 15 TABELAS EM UMA TABELA DE COMPLETUDE PARA CO, REVOME 15 TABELAS PARCIAIS ####
tbcompletude_co <-rbind(tbcompletude_codesc, tbcompletude_co1, tbcompletude_co2, tbcompletude_co3, tbcompletude_co4, tbcompletude_co5, tbcompletude_co6,
                        tbcompletude_co7, tbcompletude_co8, tbcompletude_co9, tbcompletude_co10, tbcompletude_co11, tbcompletude_co12,
                        tbcompletude_co13, tbcompletude_co14)

rm(tbcompletude_codesc,tbcompletude_co1, tbcompletude_co2, tbcompletude_co3, tbcompletude_co4, tbcompletude_co5, tbcompletude_co6,
   tbcompletude_co7, tbcompletude_co8, tbcompletude_co9, tbcompletude_co10, tbcompletude_co11, tbcompletude_co12,
   tbcompletude_co13, tbcompletude_co14)


# IMPORTA MATRÍCULAS DO SUL E CONTA NA's (NOT AVAIABLE) EM 15 PARTES ####
# VARIÁVEIS USADAS NO DESCRITIVO
matriculas_suldesc <- fread("matricula_sul.csv", select=c("TP_SEXO","TP_COR_RACA",
                                                        "CO_UF","TP_ETAPA_ENSINO",
                                                        "CO_CURSO_EDUC_PROFISSIONAL",
                                                        "IN_PROFISSIONALIZANTE"))

tbcompletude_suldesc <- data.frame(var = "Sexo",`n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_suldesc %>% filter(is.na(TP_SEXO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))  %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_suldesc %>% filter( TP_COR_RACA ==0| is.na(TP_COR_RACA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>% 
  
  add_row(var = "Unidade da Federação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_suldesc %>% filter( CO_UF ==0| is.na(CO_UF)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_suldesc %>% filter(is.na(TP_ETAPA_ENSINO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_suldesc %>% filter (TP_ETAPA_ENSINO==30 | TP_ETAPA_ENSINO==31 | TP_ETAPA_ENSINO==32 |
                                       TP_ETAPA_ENSINO==33 | TP_ETAPA_ENSINO==34 | TP_ETAPA_ENSINO==35 |
                                       TP_ETAPA_ENSINO==36 | TP_ETAPA_ENSINO==37 | TP_ETAPA_ENSINO==38 |
                                       TP_ETAPA_ENSINO==39 | TP_ETAPA_ENSINO==40 | TP_ETAPA_ENSINO==65 |
                                       TP_ETAPA_ENSINO==67 | TP_ETAPA_ENSINO==68 | TP_ETAPA_ENSINO==73 |
                                       TP_ETAPA_ENSINO==74 | IN_PROFISSIONALIZANTE==1) %>% 
            filter(is.na(CO_CURSO_EDUC_PROFISSIONAL)==T) %>% count() %>% mutate(n = 323216-n)%>% mutate(prop = round((n)/323216*100, 1)))
rm(matriculas_suldesc)

# OUTRAS VARIÁVEIS
matriculas_sul1 <- fread("matricula_sul.csv", select=c("ID_ALUNO","ID_MATRICULA","NU_MES","NU_ANO"))
tbcompletude_sul1 <- data.frame(var = "Código do aluno (ID_INEP)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul1 %>% filter(is.na(ID_ALUNO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Código único da matrícula", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul1 %>% filter(is.na(ID_MATRICULA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Data de nascimento do aluno - mês", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul1 %>% filter(is.na(NU_MES)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Data de nascimento do aluno - ano", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul1 %>% filter(is.na(NU_ANO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_sul1)

matriculas_sul2 <- fread("matricula_sul.csv", select=c("NU_IDADE_REFERENCIA","NU_IDADE","TP_SEXO","TP_COR_RACA","TP_NACIONALIDADE"))
tbcompletude_sul2 <- data.frame(var = "Idade do aluno no mês de referência do Censo Escolar (31 de maio)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul2 %>% filter(is.na(NU_IDADE_REFERENCIA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Idade calculada pelo ano de nascimento do aluno", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul2 %>% filter(is.na(NU_IDADE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Sexo", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul2 %>% filter(is.na(TP_SEXO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Cor/raça", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul2 %>% filter(is.na(TP_COR_RACA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Nacionalidade", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul2 %>% filter(is.na(TP_NACIONALIDADE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_sul2)

matriculas_sul3 <- fread("matricula_sul.csv", select=c("CO_PAIS_ORIGEM","CO_UF_NASC","CO_MUNICIPIO_NASC","CO_UF_END"))
tbcompletude_sul3 <- data.frame(var = "Código País da nacionalidade", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul3 %>% filter(is.na(CO_PAIS_ORIGEM)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Código UF de nascimento", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul3 %>% filter(is.na(CO_UF_NASC)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Código Município de nascimento", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul3 %>% filter(is.na(CO_MUNICIPIO_NASC)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Código UF de residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul3 %>% filter(is.na(CO_UF_END)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))

rm(matriculas_sul3)

matriculas_sul4 <- fread("matricula_sul.csv", select=c("CO_MUNICIPIO_END","TP_ZONA_RESIDENCIAL","TP_LOCAL_RESID_DIFERENCIADA","IN_NECESSIDADE_ESPECIAL","IN_BAIXA_VISAO"))
tbcompletude_sul4 <- data.frame(var = "Código Município de residência", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul4 %>% filter(is.na(CO_MUNICIPIO_END)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Localização/Zona de residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul4 %>% filter(is.na(TP_ZONA_RESIDENCIAL)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Localização diferenciada da residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul4 %>% filter(is.na(TP_LOCAL_RESID_DIFERENCIADA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Aluno (a) com deficiência, transtorno do espectro autista ou altas habilidades/superdotação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul4 %>% filter(is.na(IN_NECESSIDADE_ESPECIAL)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Baixa visão", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul4 %>% filter(is.na(IN_BAIXA_VISAO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_sul4)

matriculas_sul5 <- fread("matricula_sul.csv", select=c("IN_CEGUEIRA","IN_DEF_AUDITIVA","IN_DEF_FISICA","IN_DEF_INTELECTUAL","IN_SURDEZ"))
tbcompletude_sul5 <- data.frame(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Cegueira", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul5 %>% filter(is.na(IN_CEGUEIRA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação (deficiência auditiva)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul5 %>% filter(is.na(IN_DEF_AUDITIVA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação  - Deficiência física", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul5 %>% filter(is.na(IN_DEF_FISICA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Deficiência intelectual", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul5 %>% filter(is.na(IN_DEF_INTELECTUAL)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Surdez", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul5 %>% filter(is.na(IN_SURDEZ)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_sul5)

matriculas_sul6 <- fread("matricula_sul.csv", select=c("IN_SURDOCEGUEIRA","IN_DEF_MULTIPLA","IN_AUTISMO","IN_SUPERDOTACAO","IN_RECURSO_LEDOR"))
tbcompletude_sul6 <- data.frame(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Surdocegueira", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul6 %>% filter(is.na(IN_SURDOCEGUEIRA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Deficiência múltipla", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul6 %>% filter(is.na(IN_DEF_MULTIPLA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Transtorno do Espectro Autista", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul6 %>% filter(is.na(IN_AUTISMO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação  - Altas habilidades/superdotação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul6 %>% filter(is.na(IN_SUPERDOTACAO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Auxílio Ledor", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul6 %>% filter(is.na(IN_RECURSO_LEDOR)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_sul6)

matriculas_sul7 <- fread("matricula_sul.csv", select=c("IN_RECURSO_TRANSCRICAO","IN_RECURSO_INTERPRETE","IN_RECURSO_LIBRAS","IN_RECURSO_LABIAL","IN_RECURSO_AMPLIADA_18"))
tbcompletude_sul7 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Auxílio Transcrição", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul7 %>% filter(is.na(IN_RECURSO_TRANSCRICAO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Guia-Intérprete", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul7 %>% filter(is.na(IN_RECURSO_INTERPRETE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Tradutor e Intérprete de Libras", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul7 %>% filter(is.na(IN_RECURSO_LIBRAS)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Leitura Labial", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul7 %>% filter(is.na(IN_RECURSO_LABIAL)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova Ampliada (Fonte tamanho 18)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul7 %>% filter(is.na(IN_RECURSO_AMPLIADA_18)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_sul7)

matriculas_sul8 <- fread("matricula_sul.csv", select=c("IN_RECURSO_AMPLIADA_24","IN_RECURSO_CD_AUDIO","IN_RECURSO_PROVA_PORTUGUES","IN_RECURSO_VIDEO_LIBRAS","IN_RECURSO_BRAILLE"))
tbcompletude_sul8 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova superampliada (Fonte tamanho 24)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul8 %>% filter(is.na(IN_RECURSO_AMPLIADA_24)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - CD com áudio para deficiente visual", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul8 %>% filter(is.na(IN_RECURSO_CD_AUDIO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova de Língua Portuguesa como segunda língua para surdos e deficientes auditivos", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul8 %>% filter(is.na(IN_RECURSO_PROVA_PORTUGUES)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova em vídeo Libras", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul8 %>% filter(is.na(IN_RECURSO_VIDEO_LIBRAS)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Material didático e Prova em Braille", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul8 %>% filter(is.na(IN_RECURSO_BRAILLE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_sul8)

matriculas_sul9 <- fread("matricula_sul.csv", select=c("IN_RECURSO_NENHUM","IN_AEE_LIBRAS","IN_AEE_LINGUA_PORTUGUESA","IN_AEE_INFORMATICA_ACESSIVEL","IN_AEE_BRAILLE"))
tbcompletude_sul9 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Nenhum", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul9 %>% filter(is.na(IN_RECURSO_NENHUM)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da Língua Brasileira de Sinais - LIBRAS", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul9 %>% filter(is.na(IN_AEE_LIBRAS)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da Língua Portuguesa como segunda língua", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul9 %>% filter(is.na(IN_AEE_LINGUA_PORTUGUESA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da informática acessível", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul9 %>% filter(is.na(IN_AEE_INFORMATICA_ACESSIVEL)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do Sistema Braille", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul9 %>% filter(is.na(IN_AEE_BRAILLE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_sul9)

matriculas_sul10 <- fread("matricula_sul.csv", select=c("IN_AEE_CAA","IN_AEE_SOROBAN","IN_AEE_VIDA_AUTONOMA","IN_AEE_OPTICOS_NAO_OPTICOS","IN_AEE_ENRIQ_CURRICULAR"))
tbcompletude_sul10 <- data.frame(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do uso da Comunicação Alternativa e Aumentativa (CAA)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul10 %>% filter(is.na(IN_AEE_CAA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino das técnicas do cálculo no Soroban", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul10 %>% filter(is.na(IN_AEE_SOROBAN)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Desenvolvimento de vida autônoma", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul10 %>% filter(is.na(IN_AEE_VIDA_AUTONOMA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do uso de recursos ópticos e não ópticos", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul10 %>% filter(is.na(IN_AEE_OPTICOS_NAO_OPTICOS)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Enriquecimento curricular", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul10 %>% filter(is.na(IN_AEE_ENRIQ_CURRICULAR)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_sul10)

matriculas_sul11 <- fread("matricula_sul.csv", select=c("IN_AEE_DESEN_COGNITIVO","IN_AEE_MOBILIDADE","TP_OUTRO_LOCAL_AULA","IN_TRANSPORTE_PUBLICO","TP_RESPONSAVEL_TRANSPORTE"))
tbcompletude_sul11 <- data.frame(var = "Tipo de Atendimento Educacional Especializado (AEE) - Desenvolvimento de funções cognitivas", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul11 %>% filter(is.na(IN_AEE_DESEN_COGNITIVO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino de técnicas para orientação e mobilidade", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul11 %>% filter(is.na(IN_AEE_MOBILIDADE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recebe escolarização em outro local (diferente da escola)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul11 %>% filter(is.na(TP_OUTRO_LOCAL_AULA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Transporte escolar público", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul11 %>% filter(is.na(IN_TRANSPORTE_PUBLICO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Poder público responsável pelo transporte escolar", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul11 %>% filter(is.na(TP_RESPONSAVEL_TRANSPORTE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_sul11)

matriculas_sul12 <- fread("matricula_sul.csv", select=c("IN_TRANSP_BICICLETA","IN_TRANSP_MICRO_ONIBUS","IN_TRANSP_ONIBUS","IN_TRANSP_TR_ANIMAL","IN_TRANSP_VANS_KOMBI"))
tbcompletude_sul12 <- data.frame(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Bicicleta)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul12 %>% filter(is.na(IN_TRANSP_BICICLETA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Micro-ônibus)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul12 %>% filter(is.na(IN_TRANSP_MICRO_ONIBUS)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Ônibus)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul12 %>% filter(is.na(IN_TRANSP_ONIBUS)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público -  Rodoviário (Tração Animal)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul12 %>% filter(is.na(IN_TRANSP_TR_ANIMAL)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Vans/Kombi)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul12 %>% filter(is.na(IN_TRANSP_VANS_KOMBI)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_sul12)

matriculas_sul13 <- fread("matricula_sul.csv", select=c("IN_TRANSP_OUTRO_VEICULO","IN_TRANSP_EMBAR_ATE5","IN_TRANSP_EMBAR_5A15","IN_TRANSP_EMBAR_15A35","IN_TRANSP_EMBAR_35"))
tbcompletude_sul13 <- data.frame(var = "Tipo de veículo utilizado no transporte escolar público -  Rodoviário (Outro tipo de veículo rodoviário)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul13 %>% filter(is.na(IN_TRANSP_OUTRO_VEICULO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de até 5 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul13 %>% filter(is.na(IN_TRANSP_EMBAR_ATE5)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de 5 a 15 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul13 %>% filter(is.na(IN_TRANSP_EMBAR_5A15)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de 15 a 35 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul13 %>% filter(is.na(IN_TRANSP_EMBAR_15A35)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade acima de 35 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul13 %>% filter(is.na(IN_TRANSP_EMBAR_35)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))

rm(matriculas_sul13)

matriculas_sul14 <- fread("matricula_sul.csv", select=c("TP_ETAPA_ENSINO","IN_ESPECIAL_EXCLUSIVA","IN_REGULAR","IN_EJA","IN_PROFISSIONALIZANTE"))
tbcompletude_sul14 <- data.frame(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sul14 %>% filter(is.na(TP_ETAPA_ENSINO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1))) %>% 
  add_row(var = "Aluno de turma exclusiva de alunos com deficiência, transtorno do espectro autista ou altas habilidades/superdotação (Classes Especiais)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul14 %>% filter(is.na(IN_ESPECIAL_EXCLUSIVA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas com etapas de escolarização consecutivas, Creche ao Ensino Médio. Etapas consideradas (nas antigas modalidades 1 ou 2): TP_ETAPA_ENSINO igual a 1,2,4,5,6,7,8,9,10,11,14, 15,16,17,18,19,20,21,41,25,26,27,28,29,30,31, 32,33,34,35,36,37 ou 38.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul14 %>% filter(is.na(IN_REGULAR)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas destinadas a pessoas que não cursaram o ensino fundamental e/ou médio em idade própria. Etapas consideradas (nas antigas modalidades 2 ou 3): TP_ETAPA_ENSINO igual a 65,67,69,70,71,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul14 %>% filter(is.na(IN_EJA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Modo profissionalizante de ensino correspondente às turmas de cursos de formação inicial e continuada ou de qualificação profissional (Cursos FIC) articulados à EJA ou concomitantes; ou de cursos técnicos de nível médio nas formas articulada (integrada ou concomitante) ou subsequente ao ensino médio e de normal/magistério. Etapas consideradas (nas antigas modalidades 1, 2 ou 3): TP_ETAPA_ENSINO igual a 30,31,32,33,34, 35,36,37,38,39,40,65,67,68,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sul14 %>% filter(is.na(IN_PROFISSIONALIZANTE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_sul14)

# JUNTA AS 15 TABELAS EM UMA TABELA DE COMPLETUDE PARA SUL, REVOME 15 TABELAS PARCIAIS ####
tbcompletude_sul <-rbind(tbcompletude_suldesc, tbcompletude_sul1, tbcompletude_sul2, tbcompletude_sul3, tbcompletude_sul4, tbcompletude_sul5, tbcompletude_sul6,
                         tbcompletude_sul7, tbcompletude_sul8, tbcompletude_sul9, tbcompletude_sul10, tbcompletude_sul11, tbcompletude_sul12,
                         tbcompletude_sul13, tbcompletude_sul14)

rm(tbcompletude_suldesc, tbcompletude_sul1, tbcompletude_sul2, tbcompletude_sul3, tbcompletude_sul4, tbcompletude_sul5, tbcompletude_sul6,
   tbcompletude_sul7, tbcompletude_sul8, tbcompletude_sul9, tbcompletude_sul10, tbcompletude_sul11, tbcompletude_sul12,
   tbcompletude_sul13, tbcompletude_sul14)



# IMPORTA MATRÍCULAS DO NORDESTE E CONTA NA's (NOT AVAIABLE) EM 15 PARTES ####
# VARIÁVEIS USADAS NO DESCRITIVO
matriculas_nedesc <- fread("matricula_nordeste.csv", select=c("TP_TIPO_ATENDIMENTO_TURMA", "TP_SEXO", "TP_COR_RACA", "CO_UF", "TP_ETAPA_ENSINO", 
                                                          "CO_CURSO_EDUC_PROFISSIONAL", "IN_PROFISSIONALIZANTE"))

tbcompletude_nedesc <- data.frame(var = "Sexo",`n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_nedesc %>% filter(is.na(TP_SEXO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))  %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_nedesc %>% filter( TP_COR_RACA ==0| is.na(TP_COR_RACA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>% 
  
  add_row(var = "Unidade da Federação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_nedesc %>% filter( CO_UF ==0| is.na(CO_UF)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_nedesc %>% filter(is.na(TP_ETAPA_ENSINO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_nedesc %>% filter (TP_ETAPA_ENSINO==30 | TP_ETAPA_ENSINO==31 | TP_ETAPA_ENSINO==32 |
                                      TP_ETAPA_ENSINO==33 | TP_ETAPA_ENSINO==34 | TP_ETAPA_ENSINO==35 |
                                      TP_ETAPA_ENSINO==36 | TP_ETAPA_ENSINO==37 | TP_ETAPA_ENSINO==38 |
                                      TP_ETAPA_ENSINO==39 | TP_ETAPA_ENSINO==40 | TP_ETAPA_ENSINO==65 |
                                      TP_ETAPA_ENSINO==67 | TP_ETAPA_ENSINO==68 | TP_ETAPA_ENSINO==73 |
                                      TP_ETAPA_ENSINO==74 | IN_PROFISSIONALIZANTE==1) %>% 
            filter(is.na(CO_CURSO_EDUC_PROFISSIONAL)==T) %>% count() %>% mutate(n = 594138-n)%>% mutate(prop = round((n)/594138*100, 1)))
rm(matriculas_nedesc)

matriculas_ne1 <- fread("matricula_nordeste.csv", select=c("ID_ALUNO","ID_MATRICULA","NU_MES","NU_ANO"))
tbcompletude_ne1 <- data.frame(var = "Código do aluno (ID_INEP)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne1 %>% filter(is.na(ID_ALUNO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Código único da matrícula", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne1 %>% filter(is.na(ID_MATRICULA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Data de nascimento do aluno - mês", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne1 %>% filter(is.na(NU_MES)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Data de nascimento do aluno - ano", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne1 %>% filter(is.na(NU_ANO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))
rm(matriculas_ne1)

matriculas_ne2 <- fread("matricula_nordeste.csv", select=c("NU_IDADE_REFERENCIA","NU_IDADE","TP_SEXO","TP_COR_RACA","TP_NACIONALIDADE"))
tbcompletude_ne2 <- data.frame(var = "Idade do aluno no mês de referência do Censo Escolar (31 de maio)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne2 %>% filter(is.na(NU_IDADE_REFERENCIA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Idade calculada pelo ano de nascimento do aluno", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne2 %>% filter(is.na(NU_IDADE)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Sexo", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne2 %>% filter(is.na(TP_SEXO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Cor/raça", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne2 %>% filter(is.na(TP_COR_RACA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Nacionalidade", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne2 %>% filter(is.na(TP_NACIONALIDADE)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))
rm(matriculas_ne2)

matriculas_ne3 <- fread("matricula_nordeste.csv", select=c("CO_PAIS_ORIGEM","CO_UF_NASC","CO_MUNICIPIO_NASC","CO_UF_END"))
tbcompletude_ne3 <- data.frame(var = "Código País da nacionalidade", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne3 %>% filter(is.na(CO_PAIS_ORIGEM)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Código UF de nascimento", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne3 %>% filter(is.na(CO_UF_NASC)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Código Município de nascimento", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne3 %>% filter(is.na(CO_MUNICIPIO_NASC)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Código UF de residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne3 %>% filter(is.na(CO_UF_END)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))

rm(matriculas_ne3)

matriculas_ne4 <- fread("matricula_nordeste.csv", select=c("CO_MUNICIPIO_END","TP_ZONA_RESIDENCIAL","TP_LOCAL_RESID_DIFERENCIADA","IN_NECESSIDADE_ESPECIAL","IN_BAIXA_VISAO"))
tbcompletude_ne4 <- data.frame(var = "Código Município de residência", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne4 %>% filter(is.na(CO_MUNICIPIO_END)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Localização/Zona de residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne4 %>% filter(is.na(TP_ZONA_RESIDENCIAL)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Localização diferenciada da residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne4 %>% filter(is.na(TP_LOCAL_RESID_DIFERENCIADA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Aluno (a) com deficiência, transtorno do espectro autista ou altas habilidades/superdotação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne4 %>% filter(is.na(IN_NECESSIDADE_ESPECIAL)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Baixa visão", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne4 %>% filter(is.na(IN_BAIXA_VISAO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))
rm(matriculas_ne4)

matriculas_ne5 <- fread("matricula_nordeste.csv", select=c("IN_CEGUEIRA","IN_DEF_AUDITIVA","IN_DEF_FISICA","IN_DEF_INTELECTUAL","IN_SURDEZ"))
tbcompletude_ne5 <- data.frame(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Cegueira", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne5 %>% filter(is.na(IN_CEGUEIRA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação (deficiência auditiva)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne5 %>% filter(is.na(IN_DEF_AUDITIVA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação  - Deficiência física", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne5 %>% filter(is.na(IN_DEF_FISICA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Deficiência intelectual", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne5 %>% filter(is.na(IN_DEF_INTELECTUAL)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Surdez", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne5 %>% filter(is.na(IN_SURDEZ)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))
rm(matriculas_ne5)

matriculas_ne6 <- fread("matricula_nordeste.csv", select=c("IN_SURDOCEGUEIRA","IN_DEF_MULTIPLA","IN_AUTISMO","IN_SUPERDOTACAO","IN_RECURSO_LEDOR"))
tbcompletude_ne6 <- data.frame(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Surdocegueira", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne6 %>% filter(is.na(IN_SURDOCEGUEIRA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Deficiência múltipla", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne6 %>% filter(is.na(IN_DEF_MULTIPLA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Transtorno do Espectro Autista", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne6 %>% filter(is.na(IN_AUTISMO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação  - Altas habilidades/superdotação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne6 %>% filter(is.na(IN_SUPERDOTACAO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Auxílio Ledor", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne6 %>% filter(is.na(IN_RECURSO_LEDOR)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))
rm(matriculas_ne6)

matriculas_ne7 <- fread("matricula_nordeste.csv", select=c("IN_RECURSO_TRANSCRICAO","IN_RECURSO_INTERPRETE","IN_RECURSO_LIBRAS","IN_RECURSO_LABIAL","IN_RECURSO_AMPLIADA_18"))
tbcompletude_ne7 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Auxílio Transcrição", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne7 %>% filter(is.na(IN_RECURSO_TRANSCRICAO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Guia-Intérprete", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne7 %>% filter(is.na(IN_RECURSO_INTERPRETE)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Tradutor e Intérprete de Libras", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne7 %>% filter(is.na(IN_RECURSO_LIBRAS)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Leitura Labial", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne7 %>% filter(is.na(IN_RECURSO_LABIAL)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova Ampliada (Fonte tamanho 18)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne7 %>% filter(is.na(IN_RECURSO_AMPLIADA_18)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))
rm(matriculas_ne7)

matriculas_ne8 <- fread("matricula_nordeste.csv", select=c("IN_RECURSO_AMPLIADA_24","IN_RECURSO_CD_AUDIO","IN_RECURSO_PROVA_PORTUGUES","IN_RECURSO_VIDEO_LIBRAS","IN_RECURSO_BRAILLE"))
tbcompletude_ne8 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova superampliada (Fonte tamanho 24)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne8 %>% filter(is.na(IN_RECURSO_AMPLIADA_24)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - CD com áudio para deficiente visual", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne8 %>% filter(is.na(IN_RECURSO_CD_AUDIO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova de Língua Portuguesa como segunda língua para surdos e deficientes auditivos", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne8 %>% filter(is.na(IN_RECURSO_PROVA_PORTUGUES)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova em vídeo Libras", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne8 %>% filter(is.na(IN_RECURSO_VIDEO_LIBRAS)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Material didático e Prova em Braille", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne8 %>% filter(is.na(IN_RECURSO_BRAILLE)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))
rm(matriculas_ne8)

matriculas_ne9 <- fread("matricula_nordeste.csv", select=c("IN_RECURSO_NENHUM","IN_AEE_LIBRAS","IN_AEE_LINGUA_PORTUGUESA","IN_AEE_INFORMATICA_ACESSIVEL","IN_AEE_BRAILLE"))
tbcompletude_ne9 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Nenhum", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne9 %>% filter(is.na(IN_RECURSO_NENHUM)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da Língua Brasileira de Sinais - LIBRAS", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne9 %>% filter(is.na(IN_AEE_LIBRAS)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da Língua Portuguesa como segunda língua", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne9 %>% filter(is.na(IN_AEE_LINGUA_PORTUGUESA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da informática acessível", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne9 %>% filter(is.na(IN_AEE_INFORMATICA_ACESSIVEL)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do Sistema Braille", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne9 %>% filter(is.na(IN_AEE_BRAILLE)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))
rm(matriculas_ne9)

matriculas_ne10 <- fread("matricula_nordeste.csv", select=c("IN_AEE_CAA","IN_AEE_SOROBAN","IN_AEE_VIDA_AUTONOMA","IN_AEE_OPTICOS_NAO_OPTICOS","IN_AEE_ENRIQ_CURRICULAR"))
tbcompletude_ne10 <- data.frame(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do uso da Comunicação Alternativa e Aumentativa (CAA)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne10 %>% filter(is.na(IN_AEE_CAA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino das técnicas do cálculo no Soroban", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne10 %>% filter(is.na(IN_AEE_SOROBAN)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Desenvolvimento de vida autônoma", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne10 %>% filter(is.na(IN_AEE_VIDA_AUTONOMA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do uso de recursos ópticos e não ópticos", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne10 %>% filter(is.na(IN_AEE_OPTICOS_NAO_OPTICOS)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Enriquecimento curricular", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne10 %>% filter(is.na(IN_AEE_ENRIQ_CURRICULAR)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))
rm(matriculas_ne10)

matriculas_ne11 <- fread("matricula_nordeste.csv", select=c("IN_AEE_DESEN_COGNITIVO","IN_AEE_MOBILIDADE","TP_OUTRO_LOCAL_AULA","IN_TRANSPORTE_PUBLICO","TP_RESPONSAVEL_TRANSPORTE"))
tbcompletude_ne11 <- data.frame(var = "Tipo de Atendimento Educacional Especializado (AEE) - Desenvolvimento de funções cognitivas", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne11 %>% filter(is.na(IN_AEE_DESEN_COGNITIVO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino de técnicas para orientação e mobilidade", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne11 %>% filter(is.na(IN_AEE_MOBILIDADE)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Recebe escolarização em outro local (diferente da escola)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne11 %>% filter(is.na(TP_OUTRO_LOCAL_AULA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Transporte escolar público", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne11 %>% filter(is.na(IN_TRANSPORTE_PUBLICO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Poder público responsável pelo transporte escolar", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne11 %>% filter(is.na(TP_RESPONSAVEL_TRANSPORTE)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))
rm(matriculas_ne11)

matriculas_ne12 <- fread("matricula_nordeste.csv", select=c("IN_TRANSP_BICICLETA","IN_TRANSP_MICRO_ONIBUS","IN_TRANSP_ONIBUS","IN_TRANSP_TR_ANIMAL","IN_TRANSP_VANS_KOMBI"))
tbcompletude_ne12 <- data.frame(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Bicicleta)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne12 %>% filter(is.na(IN_TRANSP_BICICLETA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Micro-ônibus)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne12 %>% filter(is.na(IN_TRANSP_MICRO_ONIBUS)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Ônibus)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne12 %>% filter(is.na(IN_TRANSP_ONIBUS)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público -  Rodoviário (Tração Animal)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne12 %>% filter(is.na(IN_TRANSP_TR_ANIMAL)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Vans/Kombi)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne12 %>% filter(is.na(IN_TRANSP_VANS_KOMBI)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))
rm(matriculas_ne12)

matriculas_ne13 <- fread("matricula_nordeste.csv", select=c("IN_TRANSP_OUTRO_VEICULO","IN_TRANSP_EMBAR_ATE5","IN_TRANSP_EMBAR_5A15","IN_TRANSP_EMBAR_15A35","IN_TRANSP_EMBAR_35"))
tbcompletude_ne13 <- data.frame(var = "Tipo de veículo utilizado no transporte escolar público -  Rodoviário (Outro tipo de veículo rodoviário)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne13 %>% filter(is.na(IN_TRANSP_OUTRO_VEICULO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de até 5 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne13 %>% filter(is.na(IN_TRANSP_EMBAR_ATE5)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de 5 a 15 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne13 %>% filter(is.na(IN_TRANSP_EMBAR_5A15)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de 15 a 35 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne13 %>% filter(is.na(IN_TRANSP_EMBAR_15A35)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade acima de 35 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne13 %>% filter(is.na(IN_TRANSP_EMBAR_35)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))

rm(matriculas_ne13)

matriculas_ne14 <- fread("matricula_nordeste.csv", select=c("TP_ETAPA_ENSINO","IN_ESPECIAL_EXCLUSIVA","IN_REGULAR","IN_EJA","IN_PROFISSIONALIZANTE"))
tbcompletude_ne14 <- data.frame(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_ne14 %>% filter(is.na(TP_ETAPA_ENSINO)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1))) %>% 
  add_row(var = "Aluno de turma exclusiva de alunos com deficiência, transtorno do espectro autista ou altas habilidades/superdotação (Classes Especiais)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne14 %>% filter(is.na(IN_ESPECIAL_EXCLUSIVA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas com etapas de escolarização consecutivas, Creche ao Ensino Médio. Etapas consideradas (nas antigas modalidades 1 ou 2): TP_ETAPA_ENSINO igual a 1,2,4,5,6,7,8,9,10,11,14, 15,16,17,18,19,20,21,41,25,26,27,28,29,30,31, 32,33,34,35,36,37 ou 38.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne14 %>% filter(is.na(IN_REGULAR)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas destinadas a pessoas que não cursaram o ensino fundamental e/ou médio em idade própria. Etapas consideradas (nas antigas modalidades 2 ou 3): TP_ETAPA_ENSINO igual a 65,67,69,70,71,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne14 %>% filter(is.na(IN_EJA)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))%>%
  add_row(var = "Modo profissionalizante de ensino correspondente às turmas de cursos de formação inicial e continuada ou de qualificação profissional (Cursos FIC) articulados à EJA ou concomitantes; ou de cursos técnicos de nível médio nas formas articulada (integrada ou concomitante) ou subsequente ao ensino médio e de normal/magistério. Etapas consideradas (nas antigas modalidades 1, 2 ou 3): TP_ETAPA_ENSINO igual a 30,31,32,33,34, 35,36,37,38,39,40,65,67,68,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_ne14 %>% filter(is.na(IN_PROFISSIONALIZANTE)==T) %>% count() %>% mutate(n = 14323438-n)%>% mutate(prop = round((n)/14323438*100, 1)))
rm(matriculas_ne14)


# JUNTA OS 14 TABELAS EM UMA TABELA DE COMPLETUDE, REVOME 14 ####
tbcompletude_ne <-rbind(tbcompletude_nedesc, tbcompletude_ne1, tbcompletude_ne2, tbcompletude_ne3, tbcompletude_ne4, tbcompletude_ne5, tbcompletude_ne6,
                        tbcompletude_ne7, tbcompletude_ne8, tbcompletude_ne9, tbcompletude_ne10, tbcompletude_ne11, tbcompletude_ne12,
                        tbcompletude_ne13, tbcompletude_ne14)

rm(tbcompletude_nedesc, tbcompletude_ne1, tbcompletude_ne2, tbcompletude_ne3, tbcompletude_ne4, tbcompletude_ne5, tbcompletude_ne6,
   tbcompletude_ne7, tbcompletude_ne8, tbcompletude_ne9, tbcompletude_ne10, tbcompletude_ne11, tbcompletude_ne12,
   tbcompletude_ne13, tbcompletude_ne14)


# IMPORTA MATRÍCULAS DO NORTE E CONTA NA's (NOT AVAIABLE) EM 15 PARTES ####
# VARIAVEIS USADAS NO DESCRITIVO
matriculas_nodesc <- fread("matricula_norte.csv", select=c("TP_TIPO_ATENDIMENTO_TURMA", "TP_SEXO", "TP_COR_RACA", "CO_UF", "TP_ETAPA_ENSINO", 
                                                       "CO_CURSO_EDUC_PROFISSIONAL", "IN_PROFISSIONALIZANTE"))

tbcompletude_nodesc <- data.frame(var = "Sexo",`n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_nodesc %>% filter(is.na(TP_SEXO)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))  %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_nodesc %>% filter( TP_COR_RACA ==0| is.na(TP_COR_RACA)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>% 
  
  add_row(var = "Unidade da Federação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_nodesc %>% filter( CO_UF ==0| is.na(CO_UF)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_nodesc %>% filter(is.na(TP_ETAPA_ENSINO)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_nodesc %>% filter (TP_ETAPA_ENSINO==30 | TP_ETAPA_ENSINO==31 | TP_ETAPA_ENSINO==32 |
                                      TP_ETAPA_ENSINO==33 | TP_ETAPA_ENSINO==34 | TP_ETAPA_ENSINO==35 |
                                      TP_ETAPA_ENSINO==36 | TP_ETAPA_ENSINO==37 | TP_ETAPA_ENSINO==38 |
                                      TP_ETAPA_ENSINO==39 | TP_ETAPA_ENSINO==40 | TP_ETAPA_ENSINO==65 |
                                      TP_ETAPA_ENSINO==67 | TP_ETAPA_ENSINO==68 | TP_ETAPA_ENSINO==73 |
                                      TP_ETAPA_ENSINO==74 | IN_PROFISSIONALIZANTE==1) %>% 
            filter(is.na(CO_CURSO_EDUC_PROFISSIONAL)==T) %>% count() %>% mutate(n = 107742-n)%>% mutate(prop = round((n)/107742*100, 1)))
rm(matriculas_nodesc)

matriculas_no1 <- fread("matricula_norte.csv", select=c("ID_ALUNO","ID_MATRICULA","NU_MES","NU_ANO"))
tbcompletude_no1 <- data.frame(var = "Código do aluno (ID_INEP)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no1 %>% filter(is.na(ID_ALUNO)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Código Único da matrícula", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no1 %>% filter(is.na(ID_MATRICULA)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Data de nascimento do aluno - mês", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no1 %>% filter(is.na(NU_MES)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Data de nascimento do aluno - ano", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no1 %>% filter(is.na(NU_ANO)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))
rm(matriculas_no1)

matriculas_no2 <- fread("matricula_norte.csv", select=c("NU_IDADE_REFERENCIA","NU_IDADE","TP_SEXO","TP_COR_RACA","TP_NACIONALIDADE"))
tbcompletude_no2 <- data.frame(var = "Idade do aluno no mês de referência do Censo Escolar (31 de maio)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no2 %>% filter(is.na(NU_IDADE_REFERENCIA)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Idade calculada pelo ano de nascimento do aluno", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no2 %>% filter(is.na(NU_IDADE)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Sexo", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no2 %>% filter(is.na(TP_SEXO)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Cor/raça", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no2 %>% filter(is.na(TP_COR_RACA)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Nacionalidade", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no2 %>% filter(is.na(TP_NACIONALIDADE)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))
rm(matriculas_no2)

matriculas_no3 <- fread("matricula_norte.csv", select=c("CO_PAIS_ORIGEM","CO_UF_NASC","CO_MUNICIPIO_NASC","CO_UF_END"))
tbcompletude_no3 <- data.frame(var = "Código País da nacionalidade", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no3 %>% filter(is.na(CO_PAIS_ORIGEM)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Código UF de nascimento", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no3 %>% filter(is.na(CO_UF_NASC)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Código Município de nascimento", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no3 %>% filter(is.na(CO_MUNICIPIO_NASC)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Código UF de residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no3 %>% filter(is.na(CO_UF_END)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))

rm(matriculas_no3)

matriculas_no4 <- fread("matricula_norte.csv", select=c("CO_MUNICIPIO_END","TP_ZONA_RESIDENCIAL","TP_LOCAL_RESID_DIFERENCIADA","IN_NECESSIDADE_ESPECIAL","IN_BAIXA_VISAO"))
tbcompletude_no4 <- data.frame(var = "Código Município de residência", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no4 %>% filter(is.na(CO_MUNICIPIO_END)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Localização/Zona de residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no4 %>% filter(is.na(TP_ZONA_RESIDENCIAL)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Localização diferenciada da residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no4 %>% filter(is.na(TP_LOCAL_RESID_DIFERENCIADA)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Aluno (a) com deficiência, transtorno do espectro autista ou altas habilidades/superdotação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no4 %>% filter(is.na(IN_NECESSIDADE_ESPECIAL)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Baixa visão", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no4 %>% filter(is.na(IN_BAIXA_VISAO)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))
rm(matriculas_no4)

matriculas_no5 <- fread("matricula_norte.csv", select=c("IN_CEGUEIRA","IN_DEF_AUDITIVA","IN_DEF_FISICA","IN_DEF_INTELECTUAL","IN_SURDEZ"))
tbcompletude_no5 <- data.frame(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Cegueira", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no5 %>% filter(is.na(IN_CEGUEIRA)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação (deficiência auditiva)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no5 %>% filter(is.na(IN_DEF_AUDITIVA)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação  - Deficiência física", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no5 %>% filter(is.na(IN_DEF_FISICA)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Deficiência intelectual", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no5 %>% filter(is.na(IN_DEF_INTELECTUAL)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Surdez", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no5 %>% filter(is.na(IN_SURDEZ)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))
rm(matriculas_no5)

matriculas_no6 <- fread("matricula_norte.csv", select=c("IN_SURDOCEGUEIRA","IN_DEF_MULTIPLA","IN_AUTISMO","IN_SUPERDOTACAO","IN_RECURSO_LEDOR"))
tbcompletude_no6 <- data.frame(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Surdocegueira", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no6 %>% filter(is.na(IN_SURDOCEGUEIRA)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Deficiência múltipla", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no6 %>% filter(is.na(IN_DEF_MULTIPLA)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Transtorno do Espectro Autista", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no6 %>% filter(is.na(IN_AUTISMO)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação  - Altas habilidades/superdotação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no6 %>% filter(is.na(IN_SUPERDOTACAO)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Auxílio Ledor", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no6 %>% filter(is.na(IN_RECURSO_LEDOR)==T) %>% count() %>% mutate(n = 4987790-n)%>% mutate(prop = round((n)/4987790*100, 1)))
rm(matriculas_no6)

matriculas_no7 <- fread("matricula_sul.csv", select=c("IN_RECURSO_TRANSCRICAO","IN_RECURSO_INTERPRETE","IN_RECURSO_LIBRAS","IN_RECURSO_LABIAL","IN_RECURSO_AMPLIADA_18"))
tbcompletude_no7 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Auxílio Transcrição", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no7 %>% filter(is.na(IN_RECURSO_TRANSCRICAO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Guia-Intérprete", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no7 %>% filter(is.na(IN_RECURSO_INTERPRETE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Tradutor e Intérprete de Libras", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no7 %>% filter(is.na(IN_RECURSO_LIBRAS)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Leitura Labial", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no7 %>% filter(is.na(IN_RECURSO_LABIAL)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova Ampliada (Fonte tamanho 18)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no7 %>% filter(is.na(IN_RECURSO_AMPLIADA_18)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_no7)

matriculas_no8 <- fread("matricula_sul.csv", select=c("IN_RECURSO_AMPLIADA_24","IN_RECURSO_CD_AUDIO","IN_RECURSO_PROVA_PORTUGUES","IN_RECURSO_VIDEO_LIBRAS","IN_RECURSO_BRAILLE"))
tbcompletude_no8 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova superampliada (Fonte tamanho 24)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no8 %>% filter(is.na(IN_RECURSO_AMPLIADA_24)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - CD com áudio para deficiente visual", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no8 %>% filter(is.na(IN_RECURSO_CD_AUDIO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova de Língua Portuguesa como segunda língua para surdos e deficientes auditivos", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no8 %>% filter(is.na(IN_RECURSO_PROVA_PORTUGUES)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova em vídeo Libras", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no8 %>% filter(is.na(IN_RECURSO_VIDEO_LIBRAS)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Material didático e Prova em Braille", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no8 %>% filter(is.na(IN_RECURSO_BRAILLE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_no8)

matriculas_no9 <- fread("matricula_sul.csv", select=c("IN_RECURSO_NENHUM","IN_AEE_LIBRAS","IN_AEE_LINGUA_PORTUGUESA","IN_AEE_INFORMATICA_ACESSIVEL","IN_AEE_BRAILLE"))
tbcompletude_no9 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Nenhum", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no9 %>% filter(is.na(IN_RECURSO_NENHUM)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da Língua Brasileira de Sinais - LIBRAS", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no9 %>% filter(is.na(IN_AEE_LIBRAS)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da Língua Portuguesa como segunda língua", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no9 %>% filter(is.na(IN_AEE_LINGUA_PORTUGUESA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da informática acessível", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no9 %>% filter(is.na(IN_AEE_INFORMATICA_ACESSIVEL)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do Sistema Braille", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no9 %>% filter(is.na(IN_AEE_BRAILLE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_no9)

matriculas_no10 <- fread("matricula_sul.csv", select=c("IN_AEE_CAA","IN_AEE_SOROBAN","IN_AEE_VIDA_AUTONOMA","IN_AEE_OPTICOS_NAO_OPTICOS","IN_AEE_ENRIQ_CURRICULAR"))
tbcompletude_no10 <- data.frame(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do uso da Comunicação Alternativa e Aumentativa (CAA)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no10 %>% filter(is.na(IN_AEE_CAA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino das técnicas do cálculo no Soroban", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no10 %>% filter(is.na(IN_AEE_SOROBAN)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Desenvolvimento de vida autônoma", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no10 %>% filter(is.na(IN_AEE_VIDA_AUTONOMA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do uso de recursos ópticos e não ópticos", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no10 %>% filter(is.na(IN_AEE_OPTICOS_NAO_OPTICOS)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Enriquecimento curricular", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no10 %>% filter(is.na(IN_AEE_ENRIQ_CURRICULAR)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_no10)

matriculas_no11 <- fread("matricula_sul.csv", select=c("IN_AEE_DESEN_COGNITIVO","IN_AEE_MOBILIDADE","TP_OUTRO_LOCAL_AULA","IN_TRANSPORTE_PUBLICO","TP_RESPONSAVEL_TRANSPORTE"))
tbcompletude_no11 <- data.frame(var = "Tipo de Atendimento Educacional Especializado (AEE) - Desenvolvimento de funções cognitivas", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no11 %>% filter(is.na(IN_AEE_DESEN_COGNITIVO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino de técnicas para orientação e mobilidade", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no11 %>% filter(is.na(IN_AEE_MOBILIDADE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Recebe escolarização em outro local (diferente da escola)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no11 %>% filter(is.na(TP_OUTRO_LOCAL_AULA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Transporte escolar público", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no11 %>% filter(is.na(IN_TRANSPORTE_PUBLICO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Poder público responsável pelo transporte escolar", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no11 %>% filter(is.na(TP_RESPONSAVEL_TRANSPORTE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_no11)

matriculas_no12 <- fread("matricula_sul.csv", select=c("IN_TRANSP_BICICLETA","IN_TRANSP_MICRO_ONIBUS","IN_TRANSP_ONIBUS","IN_TRANSP_TR_ANIMAL","IN_TRANSP_VANS_KOMBI"))
tbcompletude_no12 <- data.frame(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Bicicleta)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no12 %>% filter(is.na(IN_TRANSP_BICICLETA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Micro-ônibus)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no12 %>% filter(is.na(IN_TRANSP_MICRO_ONIBUS)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Ônibus)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no12 %>% filter(is.na(IN_TRANSP_ONIBUS)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público -  Rodoviário (Tração Animal)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no12 %>% filter(is.na(IN_TRANSP_TR_ANIMAL)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Vans/Kombi)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no12 %>% filter(is.na(IN_TRANSP_VANS_KOMBI)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_no12)

matriculas_no13 <- fread("matricula_sul.csv", select=c("IN_TRANSP_OUTRO_VEICULO","IN_TRANSP_EMBAR_ATE5","IN_TRANSP_EMBAR_5A15","IN_TRANSP_EMBAR_15A35","IN_TRANSP_EMBAR_35"))
tbcompletude_no13 <- data.frame(var = "Tipo de veículo utilizado no transporte escolar público -  Rodoviário (Outro tipo de veículo rodoviário)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no13 %>% filter(is.na(IN_TRANSP_OUTRO_VEICULO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de até 5 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no13 %>% filter(is.na(IN_TRANSP_EMBAR_ATE5)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de 5 a 15 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no13 %>% filter(is.na(IN_TRANSP_EMBAR_5A15)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de 15 a 35 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no13 %>% filter(is.na(IN_TRANSP_EMBAR_15A35)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade acima de 35 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no13 %>% filter(is.na(IN_TRANSP_EMBAR_35)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))

rm(matriculas_no13)

matriculas_no14 <- fread("matricula_sul.csv", select=c("TP_ETAPA_ENSINO","IN_ESPECIAL_EXCLUSIVA","IN_REGULAR","IN_EJA","IN_PROFISSIONALIZANTE"))
tbcompletude_no14 <- data.frame(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_no14 %>% filter(is.na(TP_ETAPA_ENSINO)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1))) %>% 
  add_row(var = "Aluno de turma exclusiva de alunos com deficiência, transtorno do espectro autista ou altas habilidades/superdotação (Classes Especiais)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no14 %>% filter(is.na(IN_ESPECIAL_EXCLUSIVA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas com etapas de escolarização consecutivas, Creche ao Ensino Médio. Etapas consideradas (nas antigas modalidades 1 ou 2): TP_ETAPA_ENSINO igual a 1,2,4,5,6,7,8,9,10,11,14, 15,16,17,18,19,20,21,41,25,26,27,28,29,30,31, 32,33,34,35,36,37 ou 38.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no14 %>% filter(is.na(IN_REGULAR)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas destinadas a pessoas que não cursaram o ensino fundamental e/ou médio em idade própria. Etapas consideradas (nas antigas modalidades 2 ou 3): TP_ETAPA_ENSINO igual a 65,67,69,70,71,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no14 %>% filter(is.na(IN_EJA)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))%>%
  add_row(var = "Modo profissionalizante de ensino correspondente às turmas de cursos de formação inicial e continuada ou de qualificação profissional (Cursos FIC) articulados à EJA ou concomitantes; ou de cursos técnicos de nível médio nas formas articulada (integrada ou concomitante) ou subsequente ao ensino médio e de normal/magistério. Etapas consideradas (nas antigas modalidades 1, 2 ou 3): TP_ETAPA_ENSINO igual a 30,31,32,33,34, 35,36,37,38,39,40,65,67,68,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_no14 %>% filter(is.na(IN_PROFISSIONALIZANTE)==T) %>% count() %>% mutate(n = 6825690-n)%>% mutate(prop = round((n)/6825690*100, 1)))
rm(matriculas_no14)

# JUNTA AS 15 TABELAS EM UMA TABELA DE COMPLETUDE PARA NORTE, REVOME 15 TABELAS PARCIAIS ####
tbcompletude_no <-rbind(tbcompletude_nodesc, tbcompletude_no1, tbcompletude_no2, tbcompletude_no3, tbcompletude_no4, tbcompletude_no5, tbcompletude_no6,
                        tbcompletude_no7, tbcompletude_no8, tbcompletude_no9, tbcompletude_no10, tbcompletude_no11, tbcompletude_no12,
                        tbcompletude_no13, tbcompletude_no14)

rm(tbcompletude_nodesc, tbcompletude_no1, tbcompletude_no2, tbcompletude_no3, tbcompletude_no4, tbcompletude_no5, tbcompletude_no6,
   tbcompletude_no7, tbcompletude_no8, tbcompletude_no9, tbcompletude_no10, tbcompletude_no11, tbcompletude_no12,
   tbcompletude_no13, tbcompletude_no14)


# IMPORTA MATRÍCULAS DO SUDESTE E CONTA NA's (NOT AVAIABLE) EM 15 PARTES ####
# VARIÁVEIS DO DESCRITIVO
matriculas_sedesc <- fread("matricula_sudeste.csv", select=c("TP_TIPO_ATENDIMENTO_TURMA", "TP_SEXO", "TP_COR_RACA", "CO_UF", "TP_ETAPA_ENSINO", 
                                                             "CO_CURSO_EDUC_PROFISSIONAL", "IN_PROFISSIONALIZANTE"))

tbcompletude_sedesc <- data.frame(var = "Sexo",`n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_sedesc %>% filter(is.na(TP_SEXO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))  %>% 
  
  add_row(var = "Raça/Cor da pele", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sedesc %>% filter( TP_COR_RACA ==0| is.na(TP_COR_RACA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>% 
  
  add_row(var = "Unidade da Federação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sedesc %>% filter( CO_UF ==0| is.na(CO_UF)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>% 
  
  add_row(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sedesc %>% filter(is.na(TP_ETAPA_ENSINO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>% 
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_sedesc %>% filter (TP_ETAPA_ENSINO==30 | TP_ETAPA_ENSINO==31 | TP_ETAPA_ENSINO==32 |
                                      TP_ETAPA_ENSINO==33 | TP_ETAPA_ENSINO==34 | TP_ETAPA_ENSINO==35 |
                                      TP_ETAPA_ENSINO==36 | TP_ETAPA_ENSINO==37 | TP_ETAPA_ENSINO==38 |
                                      TP_ETAPA_ENSINO==39 | TP_ETAPA_ENSINO==40 | TP_ETAPA_ENSINO==65 |
                                      TP_ETAPA_ENSINO==67 | TP_ETAPA_ENSINO==68 | TP_ETAPA_ENSINO==73 |
                                      TP_ETAPA_ENSINO==74 | IN_PROFISSIONALIZANTE==1) %>% 
            filter(is.na(CO_CURSO_EDUC_PROFISSIONAL)==T) %>% count() %>% mutate(n = 815029-n)%>% mutate(prop = round((n)/815029*100, 1)))
rm(matriculas_sedesc)

# OUTRAS VARIÁVEIS
matriculas_se1 <- fread("matricula_sudeste.csv", select=c("ID_ALUNO","ID_MATRICULA","NU_MES","NU_ANO"))
tbcompletude_se1 <- data.frame(var = "Código do aluno (ID_INEP)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se1 %>% filter(is.na(ID_ALUNO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Código único da matrícula", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se1 %>% filter(is.na(ID_MATRICULA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Data de nascimento do aluno - mês", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se1 %>% filter(is.na(NU_MES)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Data de nascimento do aluno - ano", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se1 %>% filter(is.na(NU_ANO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))
rm(matriculas_se1)

matriculas_se2 <- fread("matricula_sudeste.csv", select=c("NU_IDADE_REFERENCIA","NU_IDADE","TP_SEXO","TP_COR_RACA","TP_NACIONALIDADE"))
tbcompletude_se2 <- data.frame(var = "Idade do aluno no mês de referência do Censo Escolar (31 de maio)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se2 %>% filter(is.na(NU_IDADE_REFERENCIA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Idade calculada pelo ano de nascimento do aluno", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se2 %>% filter(is.na(NU_IDADE)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Sexo", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se2 %>% filter(is.na(TP_SEXO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Cor/raça", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se2 %>% filter(is.na(TP_COR_RACA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Nacionalidade", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se2 %>% filter(is.na(TP_NACIONALIDADE)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))
rm(matriculas_se2)

matriculas_se3 <- fread("matricula_sudeste.csv", select=c("CO_PAIS_ORIGEM","CO_UF_NASC","CO_MUNICIPIO_NASC","CO_UF_END"))
tbcompletude_se3 <- data.frame(var = "Código País da nacionalidade", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se3 %>% filter(is.na(CO_PAIS_ORIGEM)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Código UF de nascimento", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se3 %>% filter(is.na(CO_UF_NASC)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Código Município de nascimento", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se3 %>% filter(is.na(CO_MUNICIPIO_NASC)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Código UF de residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se3 %>% filter(is.na(CO_UF_END)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))

rm(matriculas_se3)

matriculas_se4 <- fread("matricula_sudeste.csv", select=c("CO_MUNICIPIO_END","TP_ZONA_RESIDENCIAL","TP_LOCAL_RESID_DIFERENCIADA","IN_NECESSIDADE_ESPECIAL","IN_BAIXA_VISAO"))
tbcompletude_se4 <- data.frame(var = "Código Município de residência", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se4 %>% filter(is.na(CO_MUNICIPIO_END)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Localização/Zona de residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se4 %>% filter(is.na(TP_ZONA_RESIDENCIAL)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Localização diferenciada da residência", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se4 %>% filter(is.na(TP_LOCAL_RESID_DIFERENCIADA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Aluno (a) com deficiência, transtorno do espectro autista ou altas habilidades/superdotação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se4 %>% filter(is.na(IN_NECESSIDADE_ESPECIAL)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Baixa visão", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se4 %>% filter(is.na(IN_BAIXA_VISAO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))
rm(matriculas_se4)

matriculas_se5 <- fread("matricula_sudeste.csv", select=c("IN_CEGUEIRA","IN_DEF_AUDITIVA","IN_DEF_FISICA","IN_DEF_INTELECTUAL","IN_SURDEZ"))
tbcompletude_se5 <- data.frame(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Cegueira", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se5 %>% filter(is.na(IN_CEGUEIRA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação (deficiência auditiva)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se5 %>% filter(is.na(IN_DEF_AUDITIVA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação  - Deficiência física", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se5 %>% filter(is.na(IN_DEF_FISICA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Deficiência intelectual", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se5 %>% filter(is.na(IN_DEF_INTELECTUAL)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Surdez", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se5 %>% filter(is.na(IN_SURDEZ)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))
rm(matriculas_se5)

matriculas_se6 <- fread("matricula_sudeste.csv", select=c("IN_SURDOCEGUEIRA","IN_DEF_MULTIPLA","IN_AUTISMO","IN_SUPERDOTACAO","IN_RECURSO_LEDOR"))
tbcompletude_se6 <- data.frame(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Surdocegueira", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se6 %>% filter(is.na(IN_SURDOCEGUEIRA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Deficiência múltipla", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se6 %>% filter(is.na(IN_DEF_MULTIPLA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação - Transtorno do Espectro Autista", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se6 %>% filter(is.na(IN_AUTISMO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de deficiência, transtorno do espectro autista ou altas habilidades/superdotação  - Altas habilidades/superdotação", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se6 %>% filter(is.na(IN_SUPERDOTACAO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Auxílio Ledor", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se6 %>% filter(is.na(IN_RECURSO_LEDOR)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))
rm(matriculas_se6)

matriculas_se7 <- fread("matricula_sudeste.csv", select=c("IN_RECURSO_TRANSCRICAO","IN_RECURSO_INTERPRETE","IN_RECURSO_LIBRAS","IN_RECURSO_LABIAL","IN_RECURSO_AMPLIADA_18"))
tbcompletude_se7 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Auxílio Transcrição", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se7 %>% filter(is.na(IN_RECURSO_TRANSCRICAO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Guia-Intérprete", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se7 %>% filter(is.na(IN_RECURSO_INTERPRETE)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Tradutor e Intérprete de Libras", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se7 %>% filter(is.na(IN_RECURSO_LIBRAS)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Leitura Labial", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se7 %>% filter(is.na(IN_RECURSO_LABIAL)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova Ampliada (Fonte tamanho 18)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se7 %>% filter(is.na(IN_RECURSO_AMPLIADA_18)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))
rm(matriculas_se7)

matriculas_se8 <- fread("matricula_sudeste.csv", select=c("IN_RECURSO_AMPLIADA_24","IN_RECURSO_CD_AUDIO","IN_RECURSO_PROVA_PORTUGUES","IN_RECURSO_VIDEO_LIBRAS","IN_RECURSO_BRAILLE"))
tbcompletude_se8 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova superampliada (Fonte tamanho 24)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se8 %>% filter(is.na(IN_RECURSO_AMPLIADA_24)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - CD com áudio para deficiente visual", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se8 %>% filter(is.na(IN_RECURSO_CD_AUDIO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova de Língua Portuguesa como segunda língua para surdos e deficientes auditivos", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se8 %>% filter(is.na(IN_RECURSO_PROVA_PORTUGUES)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Prova em vídeo Libras", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se8 %>% filter(is.na(IN_RECURSO_VIDEO_LIBRAS)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Material didático e Prova em Braille", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se8 %>% filter(is.na(IN_RECURSO_BRAILLE)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))
rm(matriculas_se8)

matriculas_se9 <- fread("matricula_sudeste.csv", select=c("IN_RECURSO_NENHUM","IN_AEE_LIBRAS","IN_AEE_LINGUA_PORTUGUESA","IN_AEE_INFORMATICA_ACESSIVEL","IN_AEE_BRAILLE"))
tbcompletude_se9 <- data.frame(var = "Recursos necessários para uso do(a) aluno(a) e para a participação em avaliações do Inep (Saeb) - Nenhum", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se9 %>% filter(is.na(IN_RECURSO_NENHUM)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da Língua Brasileira de Sinais - LIBRAS", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se9 %>% filter(is.na(IN_AEE_LIBRAS)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da Língua Portuguesa como segunda língua", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se9 %>% filter(is.na(IN_AEE_LINGUA_PORTUGUESA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino da informática acessível", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se9 %>% filter(is.na(IN_AEE_INFORMATICA_ACESSIVEL)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do Sistema Braille", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se9 %>% filter(is.na(IN_AEE_BRAILLE)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))
rm(matriculas_se9)

matriculas_se10 <- fread("matricula_sudeste.csv", select=c("IN_AEE_CAA","IN_AEE_SOROBAN","IN_AEE_VIDA_AUTONOMA","IN_AEE_OPTICOS_NAO_OPTICOS","IN_AEE_ENRIQ_CURRICULAR"))
tbcompletude_se10 <- data.frame(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do uso da Comunicação Alternativa e Aumentativa (CAA)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se10 %>% filter(is.na(IN_AEE_CAA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino das técnicas do cálculo no Soroban", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se10 %>% filter(is.na(IN_AEE_SOROBAN)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Desenvolvimento de vida autônoma", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se10 %>% filter(is.na(IN_AEE_VIDA_AUTONOMA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino do uso de recursos ópticos e não ópticos", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se10 %>% filter(is.na(IN_AEE_OPTICOS_NAO_OPTICOS)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Enriquecimento curricular", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se10 %>% filter(is.na(IN_AEE_ENRIQ_CURRICULAR)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))
rm(matriculas_se10)

matriculas_se11 <- fread("matricula_sudeste.csv", select=c("IN_AEE_DESEN_COGNITIVO","IN_AEE_MOBILIDADE","TP_OUTRO_LOCAL_AULA","IN_TRANSPORTE_PUBLICO","TP_RESPONSAVEL_TRANSPORTE"))
tbcompletude_se11 <- data.frame(var = "Tipo de Atendimento Educacional Especializado (AEE) - Desenvolvimento de funções cognitivas", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se11 %>% filter(is.na(IN_AEE_DESEN_COGNITIVO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de Atendimento Educacional Especializado (AEE) - Ensino de técnicas para orientação e mobilidade", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se11 %>% filter(is.na(IN_AEE_MOBILIDADE)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Recebe escolarização em outro local (diferente da escola)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se11 %>% filter(is.na(TP_OUTRO_LOCAL_AULA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Transporte escolar público", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se11 %>% filter(is.na(IN_TRANSPORTE_PUBLICO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Poder público responsável pelo transporte escolar", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se11 %>% filter(is.na(TP_RESPONSAVEL_TRANSPORTE)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))
rm(matriculas_se11)

matriculas_se12 <- fread("matricula_sudeste.csv", select=c("IN_TRANSP_BICICLETA","IN_TRANSP_MICRO_ONIBUS","IN_TRANSP_ONIBUS","IN_TRANSP_TR_ANIMAL","IN_TRANSP_VANS_KOMBI"))
tbcompletude_se12 <- data.frame(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Bicicleta)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se12 %>% filter(is.na(IN_TRANSP_BICICLETA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Micro-ônibus)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se12 %>% filter(is.na(IN_TRANSP_MICRO_ONIBUS)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Ônibus)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se12 %>% filter(is.na(IN_TRANSP_ONIBUS)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público -  Rodoviário (Tração Animal)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se12 %>% filter(is.na(IN_TRANSP_TR_ANIMAL)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Rodoviário (Vans/Kombi)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se12 %>% filter(is.na(IN_TRANSP_VANS_KOMBI)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))
rm(matriculas_se12)

matriculas_se13 <- fread("matricula_sudeste.csv", select=c("IN_TRANSP_OUTRO_VEICULO","IN_TRANSP_EMBAR_ATE5","IN_TRANSP_EMBAR_5A15","IN_TRANSP_EMBAR_15A35","IN_TRANSP_EMBAR_35"))
tbcompletude_se13 <- data.frame(var = "Tipo de veículo utilizado no transporte escolar público -  Rodoviário (Outro tipo de veículo rodoviário)", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se13 %>% filter(is.na(IN_TRANSP_OUTRO_VEICULO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de até 5 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se13 %>% filter(is.na(IN_TRANSP_EMBAR_ATE5)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de 5 a 15 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se13 %>% filter(is.na(IN_TRANSP_EMBAR_5A15)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade de 15 a 35 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se13 %>% filter(is.na(IN_TRANSP_EMBAR_15A35)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Tipo de veículo utilizado no transporte escolar público - Aquaviário/Embarcação (Capacidade acima de 35 alunos)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se13 %>% filter(is.na(IN_TRANSP_EMBAR_35)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))

rm(matriculas_se13)

matriculas_se14 <- fread("matricula_sudeste.csv", select=c("TP_ETAPA_ENSINO","IN_ESPECIAL_EXCLUSIVA","IN_REGULAR","IN_EJA","IN_PROFISSIONALIZANTE"))
tbcompletude_se14 <- data.frame(var = "Etapa de ensino da matrícula", `n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(matriculas_se14 %>% filter(is.na(TP_ETAPA_ENSINO)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1))) %>% 
  add_row(var = "Aluno de turma exclusiva de alunos com deficiência, transtorno do espectro autista ou altas habilidades/superdotação (Classes Especiais)", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se14 %>% filter(is.na(IN_ESPECIAL_EXCLUSIVA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas com etapas de escolarização consecutivas, Creche ao Ensino Médio. Etapas consideradas (nas antigas modalidades 1 ou 2): TP_ETAPA_ENSINO igual a 1,2,4,5,6,7,8,9,10,11,14, 15,16,17,18,19,20,21,41,25,26,27,28,29,30,31, 32,33,34,35,36,37 ou 38.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se14 %>% filter(is.na(IN_REGULAR)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas destinadas a pessoas que não cursaram o ensino fundamental e/ou médio em idade própria. Etapas consideradas (nas antigas modalidades 2 ou 3): TP_ETAPA_ENSINO igual a 65,67,69,70,71,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se14 %>% filter(is.na(IN_EJA)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))%>%
  add_row(var = "Modo profissionalizante de ensino correspondente às turmas de cursos de formação inicial e continuada ou de qualificação profissional (Cursos FIC) articulados à EJA ou concomitantes; ou de cursos técnicos de nível médio nas formas articulada (integrada ou concomitante) ou subsequente ao ensino médio e de normal/magistério. Etapas consideradas (nas antigas modalidades 1, 2 ou 3): TP_ETAPA_ENSINO igual a 30,31,32,33,34, 35,36,37,38,39,40,65,67,68,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(matriculas_se14 %>% filter(is.na(IN_PROFISSIONALIZANTE)==T) %>% count() %>% mutate(n = 19428185-n)%>% mutate(prop = round((n)/19428185*100, 1)))
rm(matriculas_se14)

tbcompletude_se <-rbind(tbcompletude_sedesc, tbcompletude_se1, tbcompletude_se2, tbcompletude_se3, tbcompletude_se4, tbcompletude_se5, tbcompletude_se6,
                        tbcompletude_se7, tbcompletude_se8, tbcompletude_se9, tbcompletude_se10, tbcompletude_se11, tbcompletude_se12,
                        tbcompletude_se13, tbcompletude_se14)

rm(tbcompletude_sedesc, tbcompletude_se1, tbcompletude_se2, tbcompletude_se3, tbcompletude_se4, tbcompletude_se5, tbcompletude_se6,
   tbcompletude_se7, tbcompletude_se8, tbcompletude_se9, tbcompletude_se10, tbcompletude_se11, tbcompletude_se12,
   tbcompletude_se13, tbcompletude_se14)



# IMPORTA ESCOLAS (BRASIL) E CONTA NA's (NOT AVAIABLE) ####
escolas <- read.table("escolas.csv", sep="|", header=T)

tbcompletude_escolas <- data.frame(var = "Ano do Censo",`n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(escolas %>% filter(is.na(NU_ANO_CENSO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))  %>% 
  
  add_row(var = "Código da Escola", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(CO_ENTIDADE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>% 
  
  add_row(var = "Nome da Escola", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(NO_ENTIDADE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>% 
  
  add_row(var = "Situação de funcionamento", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_SITUACAO_FUNCIONAMENTO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1))) %>% 
  
  add_row(var = "Início do ano letivo", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(DT_ANO_LETIVO_INICIO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Término (Previsão) do ano letivo", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(DT_ANO_LETIVO_TERMINO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Código da região geográfica", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(CO_REGIAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Código da mesorregião", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(CO_MESORREGIAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Código da microrregião", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(CO_MICRORREGIAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Código da UF", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(CO_UF)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Código do Município", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(CO_MUNICIPIO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Código completo do Distrito da escola", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(CO_DISTRITO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependência Administrativa", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_DEPENDENCIA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Localização", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_LOCALIZACAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Localização diferenciada da escola", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_LOCALIZACAO_DIFERENCIADA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Órgão que a escola pública está vinculada - Secretaria de Educação/Ministério da Educação", `n`=NA, `prop`=NA) %>%    
  add_row(escolas %>% filter(is.na(IN_VINCULO_SECRETARIA_EDUCACAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Secretaria de Segurança Pública/Forças Armadas/Militar", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_VINCULO_SEGURANCA_PUBLICA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Secretaria de Saúde/Ministério da Saúde", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_VINCULO_SECRETARIA_SAUDE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Outro órgão da administração pública", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_VINCULO_OUTRO_ORGAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Categoria da escola privada", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_CATEGORIA_ESCOLA_PRIVADA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Conveniada com o poder público", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_CONVENIADA_PP)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependência do convênio com o poder público", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_CONVENIO_PODER_PUBLICO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Mantenedora da escola privada - Empresa ou grupo empresarial do setor privado ou pessoa física", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MANT_ESCOLA_PRIVADA_EMP)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Mantenedora da escola privada - Organização não governamental (ONG) - internacional ou nacional.", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MANT_ESCOLA_PRIVADA_ONG)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Mantenedora da escola privada - Organização da Sociedade Civil de Interesse Público (Oscip)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MANT_ESCOLA_PRIVADA_OSCIP)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Mantenedora da escola privada - Organização não governamental (ONG) - internacional ou nacional. Organização da Sociedade Civil de Interesse Público (Oscip)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MANT_ESCOLA_PRIV_ONG_OSCIP)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Mantenedora da escola privada - Sindicatos de trabalhadores ou patronais, associações e cooperativas", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MANT_ESCOLA_PRIVADA_SIND)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Mantenedora da escola privada - Sistema S (Sesi, Senai, Sesc, Outros)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MANT_ESCOLA_PRIVADA_SIST_S)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Mantenedora da escola privada - Instituições sem fins lucrativos", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MANT_ESCOLA_PRIVADA_S_FINS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  #add_row(var = "Número do CNPJ da escola privada", `n`=NA, `prop`=NA) %>% 
  #add_row(escolas %>% filter(is.na(NU_CNPJ_ESCOLA_PRIVADA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  #add_row(var = "Número do CNPJ da mantenedora principal da escola privada", `n`=NA, `prop`=NA) %>% 
  #add_row(escolas %>% filter(is.na(NU_CNPJ_MANTENEDORA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "A escola apresenta regulamentação/autorização no conselho ou órgão municipal, estadual ou federal de educação", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_REGULAMENTACAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Esfera administrativa do conselho ou órgão responsável pela Regulamentação/Autorização", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_RESPONSAVEL_REGULAMENTACAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Código da escola sede", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(CO_ESCOLA_SEDE_VINCULADA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Código da IES vinculada à escola", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(CO_IES_OFERTANTE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Local de funcionamento da escola - Prédio Escolar", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LOCAL_FUNC_PREDIO_ESCOLAR)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Forma de ocupação do Prédio escolar", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_OCUPACAO_PREDIO_ESCOLAR)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Local de funcionamento da escola - Unidade de Atendimento socioeducativo", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LOCAL_FUNC_SOCIOEDUCATIVO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Local de funcionamento da escola - Unidade Prisional", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LOCAL_FUNC_UNID_PRISIONAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Local de funcionamento da escola - Unidade Prisional ou Unidade de atendimento socioeducativo", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LOCAL_FUNC_PRISIONAL_SOCIO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Local de funcionamento da escola - Galpão/Rancho/Paiol/Barracão", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LOCAL_FUNC_GALPAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Forma de ocupação do Galpão/Rancho/Paiol/Barracão", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_OCUPACAO_GALPAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Local de funcionamento da escola - Salas em outra escola", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LOCAL_FUNC_SALAS_OUTRA_ESC)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Local de funcionamento da escola - Outros", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LOCAL_FUNC_OUTROS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Prédio compartilhado com outra escola", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_PREDIO_COMPARTILHADO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Fornece água potável para o consumo humano", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_AGUA_POTAVEL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Abastecimento de água - Rede pública", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_AGUA_REDE_PUBLICA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Abastecimento de água - Poço artesiano", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_AGUA_POCO_ARTESIANO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Abastecimento de água - Cacimba/Cisterna/Poço", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_AGUA_CACIMBA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Abastecimento de água - Fonte/Rio/Igarapé/Riacho/Córrego", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_AGUA_FONTE_RIO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Abastecimento de água - Não há abastecimento de água", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_AGUA_INEXISTENTE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Abastecimento de energia elétrica - Rede pública", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ENERGIA_REDE_PUBLICA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Abastecimento de energia elétrica - Gerador movido a combustível fóssil", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ENERGIA_GERADOR_FOSSIL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Abastecimento de energia elétrica - Fontes de energia renováveis ou alternativas (gerador a biocombustível e/ou biodigestores, eólica, solar, outras)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ENERGIA_RENOVAVEL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Abastecimento de energia elétrica - Não há energia elétrica", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ENERGIA_INEXISTENTE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Esgoto sanitário - Rede pública", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESGOTO_REDE_PUBLICA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Esgoto sanitário - Fossa Séptica", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESGOTO_FOSSA_SEPTICA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Esgoto sanitário - Fossa rudimentar/comum", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESGOTO_FOSSA_COMUM)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Esgoto sanitário - Fossa", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESGOTO_FOSSA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Esgoto sanitário - Não há esgotamento sanitário", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESGOTO_INEXISTENTE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Destinação do lixo - Serviço de coleta", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LIXO_SERVICO_COLETA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Destinação do lixo - Queima", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LIXO_QUEIMA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Destinação do lixo - Enterra", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LIXO_ENTERRA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Destinação do lixo - Leva a uma destinação final financiada pelo poder público", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LIXO_DESTINO_FINAL_PUBLICO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Destinação do lixo - Descarta em outra área", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LIXO_DESCARTA_OUTRA_AREA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Tratamento do lixo/resíduos que a escola realiza - Separação do lixo/resíduos", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_TRATAMENTO_LIXO_SEPARACAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Tratamento do lixo/resíduos que a escola realiza - Reaproveitamento/reutilização", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_TRATAMENTO_LIXO_REUTILIZA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Tratamento do lixo/resíduos que a escola realiza - Reciclagem", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_TRATAMENTO_LIXO_RECICLAGEM)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Tratamento do lixo/resíduos que a escola realiza - Não faz tratamento", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_TRATAMENTO_LIXO_INEXISTENTE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Almoxarifado", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ALMOXARIFADO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Área Verde", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_AREA_VERDE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Auditório", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_AUDITORIO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Banheiro ", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_BANHEIRO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Banheiro adequado à educação infantil", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_BANHEIRO_EI)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Banheiro acessível, adequado ao uso de pessoas com deficiência ou mobilidade reduzida", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_BANHEIRO_PNE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Banheiro exclusivo para os funcionários", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_BANHEIRO_FUNCIONARIOS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Banheiro ou vestiário com chuveiro", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_BANHEIRO_CHUVEIRO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Biblioteca", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_BIBLIOTECA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Biblioteca e/ou Sala de leitura", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_BIBLIOTECA_SALA_LEITURA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Cozinha", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COZINHA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Despensa", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_DESPENSA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Dormitório de Aluno (a)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_DORMITORIO_ALUNO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Dormitório de professor (a)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_DORMITORIO_PROFESSOR)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Laboratório de ciências", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LABORATORIO_CIENCIAS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Laboratório de informática", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_LABORATORIO_INFORMATICA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Pátio Coberto", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_PATIO_COBERTO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Pátio Descoberto", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_PATIO_DESCOBERTO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Parque infantil", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_PARQUE_INFANTIL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Piscina", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_PISCINA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Quadra de esportes coberta ou descoberta", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_QUADRA_ESPORTES)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Quadra de esportes Coberta", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_QUADRA_ESPORTES_COBERTA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Quadra de esportes Descoberta", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_QUADRA_ESPORTES_DESCOBERTA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Refeitório", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_REFEITORIO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Sala/ateliê de artes", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_SALA_ATELIE_ARTES)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola -  Sala de música/coral", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_SALA_MUSICA_CORAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Sala/estúdio de dança", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_SALA_ESTUDIO_DANCA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Sala multiuso (música, dança e artes)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_SALA_MULTIUSO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Sala de Diretoria", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_SALA_DIRETORIA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Sala de Leitura", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_SALA_LEITURA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Sala de professores", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_SALA_PROFESSOR)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Sala de repouso para aluno(a)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_SALA_REPOUSO_ALUNO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Sala de secretaria", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_SECRETARIA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Sala de recursos Multifuncionais para Atendimento Educacional Especializado (AEE)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_SALA_ATENDIMENTO_ESPECIAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola - Terreirão (área para prática desportiva e recreação sem cobertura, sem piso e sem edificações)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_TERREIRAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências físicas existentes e utilizadas na escola -  Viveiro/criação de animais", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_VIVEIRO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Dependências existentes na escola - Nenhuma das dependência relacionadas", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_DEPENDENCIAS_OUTRAS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Recursos de acessibilidade para pessoas com deficiência ou mobilidade reduzida nas vias de circulação interna na escola - Corrimão e guarda corpos", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ACESSIBILIDADE_CORRIMAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Recursos de acessibilidade para pessoas com deficiência ou mobilidade reduzida nas vias de circulação interna na escola - Elevador", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ACESSIBILIDADE_ELEVADOR)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Recursos de acessibilidade para pessoas com deficiência ou mobilidade reduzida nas vias de circulação interna na escola - Pisos táteis", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ACESSIBILIDADE_PISOS_TATEIS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Recursos de acessibilidade para pessoas com deficiência ou mobilidade reduzida nas vias de circulação interna na escola - Portas com vão livre de no mínimo 80 cm", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ACESSIBILIDADE_VAO_LIVRE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Recursos de acessibilidade para pessoas com deficiência ou mobilidade reduzida nas vias de circulação interna na escola - Rampas", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ACESSIBILIDADE_RAMPAS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Recursos de acessibilidade para pessoas com deficiência ou mobilidade reduzida nas vias de circulação interna na escola - Sinalização sonora", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ACESSIBILIDADE_SINAL_SONORO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Recursos de acessibilidade para pessoas com deficiência ou mobilidade reduzida nas vias de circulação interna na escola - Sinalização tátil (piso/paredes)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ACESSIBILIDADE_SINAL_TATIL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Recursos de acessibilidade para pessoas com deficiência ou mobilidade reduzida nas vias de circulação interna na escola - Sinalização visual (piso/paredes)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ACESSIBILIDADE_SINAL_VISUAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Recursos de acessibilidade para pessoas com deficiência ou mobilidade reduzida nas vias de circulação interna na escola - Nenhum dos recursos de acessibilidade listado", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ACESSIBILIDADE_INEXISTENTE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Número de salas de aula utilizadas na escola - Dentro do prédio", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_SALAS_UTILIZADAS_DENTRO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Número de salas de aula utilizadas na escola - Fora do prédio", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_SALAS_UTILIZADAS_FORA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Número de salas de aula utilizadas na escola (dentro e fora do prédio)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_SALAS_UTILIZADAS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Condições das salas de aula utilizadas na escola (dentro e fora do prédio escolar) - Número de salas de aula climatizadas", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_SALAS_UTILIZA_CLIMATIZADAS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Condições das salas de aula utilizadas na escola (dentro e fora do prédio escolar) - Número de salas de aula com acessibilidade para pessoas com deficiência ou mobilidade reduzida.", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_SALAS_UTILIZADAS_ACESSIVEIS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos existentes na escola para uso técnico e administrativo - Antena parabólica", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EQUIP_PARABOLICA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos existentes na escola para uso técnico e administrativo - Computador", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMPUTADOR)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos existentes na escola para uso técnico e administrativo - Copiadora", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EQUIP_COPIADORA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos existentes na escola para uso técnico e administrativo - Impressora", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EQUIP_IMPRESSORA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos existentes na escola para uso técnico e administrativo - Impressora Multifuncional", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EQUIP_IMPRESSORA_MULT)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos existentes na escola para uso técnico e administrativo - Scanner", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EQUIP_SCANNER)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Nenhum dos equipamentos listados para uso técnico e administrativo - Antena parabólica, Computador, Copiadora, Impressora, Impressora Multifuncional ou Scanner", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EQUIP_NENHUM)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos existentes na escola para o processo ensino aprendizagem - DVD/Blu-ray", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EQUIP_DVD)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Quantidade de Aparelhos de DVD/Blue-ray", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_EQUIP_DVD)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos existentes na escola para o processo ensino aprendizagem - Aparelho de som", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EQUIP_SOM)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Quantidade de Aparelhos de som", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_EQUIP_SOM)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos existentes na escola para o processo ensino aprendizagem - Aparelho de televisão", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EQUIP_TV)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Quantidade de Aparelhos de televisão", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_EQUIP_TV)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos existentes na escola para o processo ensino aprendizagem - Lousa digital", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EQUIP_LOUSA_DIGITAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Quantidade de Lousas digitais", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_EQUIP_LOUSA_DIGITAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos existentes na escola para o processo ensino aprendizagem - Projetor Multimídia (Datashow)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EQUIP_MULTIMIDIA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Quantidade de Projetores Multimídias (Datashow)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_EQUIP_MULTIMIDIA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Computadores em uso pelos alunos (as) - Computador de mesa (desktop)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_DESKTOP_ALUNO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Quantidade de computadores em uso pelos alunos (as) - Computador de mesa (desktop)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_DESKTOP_ALUNO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Tablet", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMP_PORTATIL_ALUNO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Quantidade de computadores em uso pelos alunos (as) - Computador portátil", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_COMP_PORTATIL_ALUNO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Computadores em uso pelos alunos (as) - Tablet", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_TABLET_ALUNO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Quantidade de computadores em uso pelos alunos (as) - Tablet", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_TABLET_ALUNO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Acesso à Internet", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_INTERNET)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Acesso à Internet - Para uso dos alunos", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_INTERNET_ALUNOS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Acesso à Internet - Para uso administrativo", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_INTERNET_ADMINISTRATIVO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Acesso à Internet - Para uso nos processos de ensino e aprendizagem", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_INTERNET_APRENDIZAGEM)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Acesso à Internet - Para uso da comunidade", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_INTERNET_COMUNIDADE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos que os alunos usam para acessar a internet da escola - Computadores de mesa, portáteis e tablets da escola (no laboratório de informática, biblioteca, sala de aula, etc.)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ACESSO_INTERNET_COMPUTADOR)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Equipamentos que os alunos usam para acessar a internet da escola - Dispositivos pessoais (computadores portáteis, celulares, tablets, etc.)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ACES_INTERNET_DISP_PESSOAIS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Rede local de interligação de computadores", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_REDE_LOCAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Banda Larga", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_BANDA_LARGA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Auxiliares de secretaria ou auxiliares administrativos, atendentes", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_ADMINISTRATIVOS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Auxiliar de serviços gerais, porteiro(a), zelador(a), faxineiro(a), horticultor(a), jardineiro(a)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_SERVICOS_GERAIS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Bibliotecário(a), auxiliar de biblioteca ou monitor(a) da sala de leitura", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_BIBLIOTECARIO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Bombeiro(a) brigadista, profissionais de assistência a saúde (urgência e emergência), Enfermeiro(a), Técnico(a) de enfermagem e socorrista", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_SAUDE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Coordenador(a) de turno/disciplina", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_COORDENADOR)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Fonoaudiólogo(a)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_FONAUDIOLOGO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola -  Nutricionista", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_NUTRICIONISTA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Psicólogo(a) Escolar", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_PSICOLOGO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola -  Profissionais de preparação e segurança alimentar, cozinheiro(a), merendeira e auxiliar de cozinha", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_ALIMENTACAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Profissionais de apoio e supervisão pedagógica: pedagogo(a), coordenador(a) pedagógico(a), orientador(a) educacional, supervisor(a) escolar e coordenador(a) de área de ensino", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_PEDAGOGIA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Secretário(a) escolar", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_SECRETARIO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Seguranças, guarda ou segurança patrimonial", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_SEGURANCA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Técnicos(as), monitores(as), supervisores(as) ou auxiliares de laboratório(s), de apoio a tecnologias educacionais ou em multimeios/multimídias eletrônico/digitais", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_MONITORES)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Vice-diretor(a) ou diretor(a) adjunto(a), profissionais responsáveis pela gestão administrativa e/ou financeira", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_GESTAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Total de profissionais que atuam na escola - Orientador(a) comunitário(a) ou assistente social", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(QT_PROF_ASSIST_SOCIAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Alimentação escolar para os alunos - PNAE/FNDE", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ALIMENTACAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Forma de organização do ensino - Série/Ano (séries anuais)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_SERIE_ANO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Forma de organização do ensino -  Períodos semestrais", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_PERIODOS_SEMESTRAIS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Forma de organização do ensino - Ciclo(s) do Ensino Fundamental", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_FUNDAMENTAL_CICLOS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Forma de organização do ensino - Grupos não-seriados com base na idade ou competência (art. 23 LDB)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_GRUPOS_NAO_SERIADOS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Forma de organização do ensino - Módulos", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MODULOS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Forma de organização do ensino - Alternância regular de períodos de estudos (proposta pedagógica de formação por alternância com tempo-escola e tempo-comunidade)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_FORMACAO_ALTERNANCIA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Instrumentos, materiais socioculturais e/ou pedagógicos em uso na escola para o desenvolvimento de atividades de ensino aprendizagem - Acervo multimídia", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MATERIAL_PED_MULTIMIDIA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Instrumentos, materiais socioculturais e/ou pedagógicos em uso na escola para o desenvolvimento de atividades de ensino aprendizagem - Brinquedos para Educação Infantil", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MATERIAL_PED_INFANTIL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Instrumentos, materiais socioculturais e/ou pedagógicos em uso na escola para o desenvolvimento de atividades de ensino aprendizagem - Conjunto de materiais científicos", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MATERIAL_PED_CIENTIFICO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Instrumentos, materiais socioculturais e/ou pedagógicos em uso na escola para o desenvolvimento de atividades de ensino aprendizagem - Equipamento para amplificação e difusão de som/áudio", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MATERIAL_PED_DIFUSAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Instrumentos, materiais socioculturais e/ou pedagógicos em uso na escola para o desenvolvimento de atividades de ensino aprendizagem - Instrumentos musicais para conjunto, banda/fanfarra e/ou aulas de música", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MATERIAL_PED_MUSICAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Instrumentos, materiais socioculturais e/ou pedagógicos em uso na escola para o desenvolvimento de atividades de ensino aprendizagem - Jogos Educativos", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MATERIAL_PED_JOGOS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Instrumentos, materiais socioculturais e/ou pedagógicos em uso na escola para o desenvolvimento de atividades de ensino aprendizagem - Materiais para atividades culturais e artísticas", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MATERIAL_PED_ARTISTICAS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Instrumentos, materiais socioculturais e/ou pedagógicos em uso na escola para o desenvolvimento de atividades de ensino aprendizagem - Materiais para prática desportiva e recreação", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MATERIAL_PED_DESPORTIVA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Instrumentos, materiais socioculturais e/ou pedagógicos em uso na escola para o desenvolvimento de atividades de ensino aprendizagem - Indígena", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MATERIAL_PED_INDIGENA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Instrumentos, materiais socioculturais e/ou pedagógicos em uso na escola para o desenvolvimento de atividades de ensino aprendizagem - Materiais pedagógicos para a educação das Relações Étnicos Raciais", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MATERIAL_PED_ETNICO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Instrumentos, materiais socioculturais e/ou pedagógicos em uso na escola para o desenvolvimento de atividades de ensino aprendizagem - Materiais pedagógicos para a educação do campo", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MATERIAL_PED_CAMPO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Instrumentos, materiais socioculturais e/ou pedagógicos em uso na escola para o desenvolvimento de atividades de ensino aprendizagem - Nenhum", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MATERIAL_PED_NENHUM)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Educação Escolar Indígena", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EDUCACAO_INDIGENA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Educação Indígena - Língua em que o ensino é ministrado", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_INDIGENA_LINGUA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Educação Indígena - Língua em que o ensino é ministrado - Língua Indígena - Código da língua Indígena 1", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(CO_LINGUA_INDIGENA_1)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Educação Indígena - Língua em que o ensino é ministrado - Língua Indígena - Código da língua Indígena 2", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(CO_LINGUA_INDIGENA_2)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Educação Indígena - Língua em que o ensino é ministrado - Língua Indígena - Código da língua Indígena 3", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(CO_LINGUA_INDIGENA_3)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "A escola faz exame de seleção para ingresso de seus alunos (Avaliação por prova e /ou análise curricular)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EXAME_SELECAO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Reserva de vagas por sistema de cotas para grupos específicos de alunos - Autodeclarado preto, pardo ou indígena (PPI)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_RESERVA_PPI)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Reserva de vagas por sistema de cotas para grupos específicos de alunos - Condição de Renda", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_RESERVA_RENDA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Reserva de vagas por sistema de cotas para grupos específicos de alunos - Oriundo de escola pública", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_RESERVA_PUBLICA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Reserva de vagas por sistema de cotas para grupos específicos de alunos - Pessoa com deficiência (PCD)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_RESERVA_PCD)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Reserva de vagas por sistema de cotas para grupos específicos de alunos - Outros grupos", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_RESERVA_OUTROS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Reserva de vagas por sistema de cotas para grupos específicos de alunos - Sem reservas de vagas para sistema de cotas (ampla concorrência)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_RESERVA_NENHUMA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "A escola possui site ou blog ou página em redes sociais para comunicação institucional", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_REDES_SOCIAIS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "A escola compartilha espaços para atividades de integração escola-comunidade", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESPACO_ATIVIDADE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "A escola usa espaços e equipamentos do entorno escolar para atividades regulares com os alunos", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESPACO_EQUIPAMENTO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Órgãos colegiados em funcionamento na escola - Associação de Pais", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ORGAO_ASS_PAIS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Órgãos colegiados em funcionamento na escola - Associação de Pais e Mestres", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ORGAO_ASS_PAIS_MESTRES)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Órgãos colegiados em funcionamento na escola - Conselho Escolar", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ORGAO_CONSELHO_ESCOLAR)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Órgãos colegiados em funcionamento na escola - Grêmio Estudantil", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ORGAO_GREMIO_ESTUDANTIL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Órgãos colegiados em funcionamento na escola - Outros", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ORGAO_OUTROS)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Órgãos colegiados em funcionamento na escola - Não há órgãos colegiados em funcionamento", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ORGAO_NENHUM)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Projeto político pedagógico ou a proposta pedagógica da escola (conforme art. 12 da LDB) atualizado nos últimos 12 meses até a data de referência", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_PROPOSTA_PEDAGOGICA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Atendimento Educacional Especializado (AEE)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_AEE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Atividade Complementar", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(TP_ATIVIDADE_COMPLEMENTAR)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Mediação didático-pedagógica oferecida pela escola - Presencial", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MEDIACAO_PRESENCIAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Mediação didático-pedagógica oferecida pela escola - Semipresencial", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MEDIACAO_SEMIPRESENCIAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Mediação didático-pedagógica oferecida pela escola - Educação a Distância - EAD", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_MEDIACAO_EAD)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola possui classe especial exclusiva (ao menos uma turma exclusiva de alunos com deficiência, transtorno global do desenvolvimento ou altas habilidades/superdotação - Classes Especiais)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESPECIAL_EXCLUSIVA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas com etapas de escolarização consecutivas, Creche ao Ensino Médio. Etapas consideradas (nas antigas modalidades 1 ou 2): TP_DEPENDENCIA igual a 1,2,4,5,6,7,8,9,10,11,14, 15,16,17,18,19,20,21,41,25,26,27,28,29,30,31, 32,33,34,35,36,37 ou 38.", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_REGULAR)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas destinadas a pessoas que não cursaram o ensino fundamental e/ou médio em idade própria. Etapas consideradas (nas antigas modalidades 2 ou 3): TP_DEPENDENCIA igual a 65,67,69,70,71,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_EJA)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Modo profissionalizante de ensino correspondente às turmas de cursos de formação inicial e continuada ou de qualificação profissional (Cursos FIC) articulados à EJA ou concomitantes; ou de cursos técnicos de nível médio nas formas articulada (integrada ou concomitante) ou subsequente ao ensino médio e de normal/magistério. Etapas consideradas (nas antigas modalidades 1, 2 ou 3): TP_DEPENDENCIA igual a 30,31,32,33,34, 35,36,37,38,39,40,65,67,68,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_PROFISSIONALIZANTE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Creche em classes comuns do ensino regular (TP_DEPENDENCIA=1 e IN_ESPECIAL_EXCLUSIVA=0)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMUM_CRECHE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Pré-escola em classes comuns do ensino regular (TP_DEPENDENCIA=2 e IN_ESPECIAL_EXCLUSIVA=0)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMUM_PRE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Anos Iniciais do Ensino Fundamental (8 e 9 anos) em classes comuns do ensino regular (TP_DEPENDENCIA igual a 4,5,6,7,14,15,16,17 ou 18 e  IN_ESPECIAL_EXCLUSIVA=0)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMUM_FUND_AI)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Anos Finais do Ensino Fundamental (8 e 9 anos) em classes comuns do ensino regular (TP_DEPENDENCIA igual a 8,9,10,11,19,20,21 ou 41 e IN_ESPECIAL_EXCLUSIVA=0)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMUM_FUND_AF)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Ensino Médio - Médio Propedêutico em classes comuns do ensino regular (TP_DEPENDENCIA igual a 25,26,27,28 ou 29 e IN_ESPECIAL_EXCLUSIVA=0)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMUM_MEDIO_MEDIO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Curso Técnico Integrado ao Ensino Médio em classes comuns do ensino regular (TP_DEPENDENCIA igual a  30,31,32,33 ou 34 e IN_ESPECIAL_EXCLUSIVA=0)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMUM_MEDIO_INTEGRADO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Ensino Médio - Normal/ Magistério em classes comuns do ensino regular (TP_DEPENDENCIA igual a  35,36,37 ou 38 e IN_ESPECIAL_EXCLUSIVA=0)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMUM_MEDIO_NORMAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Creche em classes especiais exclusivas do ensino regular (TP_DEPENDENCIA=1 e IN_ESPECIAL_EXCLUSIVA=1)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESP_EXCLUSIVA_CRECHE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Pré-escola em classes especiais exclusivas do ensino regular (TP_DEPENDENCIA=2 e IN_ESPECIAL_EXCLUSIVA=1)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESP_EXCLUSIVA_PRE)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Anos Iniciais do Ensino Fundamental (8 e 9 anos) em classes especiais exclusivas do ensino regular (TP_DEPENDENCIA igual a 4,5,6,7,14,15,16,17 ou 18 e  IN_ESPECIAL_EXCLUSIVA=1)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESP_EXCLUSIVA_FUND_AI)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Anos Finais do Ensino Fundamental (8 e 9 anos) em classes especiais exclusivas do ensino regular (TP_DEPENDENCIA igual a 8,9,10,11,19,20,21 ou 41 e IN_ESPECIAL_EXCLUSIVA=1)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESP_EXCLUSIVA_FUND_AF)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Ensino Médio - Médio Propedêutico em classes especiais exclusivas do ensino regular (TP_DEPENDENCIA igual a 25,26,27,28 ou 29 e IN_ESPECIAL_EXCLUSIVA=1)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESP_EXCLUSIVA_MEDIO_MEDIO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Curso Técnico Integrado ao Ensino Médio em classes especiais exclusivas do ensino regular (TP_DEPENDENCIA igual a  30,31,32,33 ou 34 e IN_ESPECIAL_EXCLUSIVA=1)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESP_EXCLUSIVA_MEDIO_INTEGR)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Ensino Médio - Normal/ Magistério em classes especiais exclusivas do ensino regular (TP_DEPENDENCIA igual a  35,36,37 ou 38 e IN_ESPECIAL_EXCLUSIVA=1)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESP_EXCLUSIVA_MEDIO_NORMAL)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Ensino Fundamental em classes comuns da EJA (TP_DEPENDENCIA igual a 65,69,70 ou 73 e IN_ESPECIAL_EXCLUSIVA=0)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMUM_EJA_FUND)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Ensino Médio em classes comuns da EJA (TP_DEPENDENCIA igual a 67,71 ou 74 e IN_ESPECIAL_EXCLUSIVA=0)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMUM_EJA_MEDIO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece educação profissional integrada em classes comuns da EJA (TP_DEPENDENCIA igual a 65,67,73 ou 74 e IN_ESPECIAL_EXCLUSIVA=0)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMUM_EJA_PROF)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Ensino Fundamental em classes especiais exclusivas da EJA(TP_DEPENDENCIA igual a 65,69,70 ou 73 e IN_ESPECIAL_EXCLUSIVA=1)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESP_EXCLUSIVA_EJA_FUND)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece Ensino Médio em classes especiais exclusivas da EJA (TP_DEPENDENCIA igual a 67,71 ou 74 e IN_ESPECIAL_EXCLUSIVA=1)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESP_EXCLUSIVA_EJA_MEDIO)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Escola oferece educação profissional integrada em classes especiais exclusivas da EJA (TP_DEPENDENCIA igual a 65,67,73 ou 74 e IN_ESPECIAL_EXCLUSIVA=1)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESP_EXCLUSIVA_EJA_PROF)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Educação Profissional - Curso Técnico Concomitante e/ou Subsequente e/ou Curso FIC Concomitante em classes comuns (TP_DEPENDENCIA igual a 39, 40 ou 68 e IN_ESPECIAL_EXCLUSIVA=0)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_COMUM_PROF)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))%>%
  
  add_row(var = "Educação Profissional - Curso Técnico Concomitante e/ou Subsequente e/ou Curso FIC Concomitante em classes especiais exclusivas (TP_DEPENDENCIA igual a 39, 40 ou 68 e IN_ESPECIAL_EXCLUSIVA=1)", `n`=NA, `prop`=NA) %>% 
  add_row(escolas %>% filter(is.na(IN_ESP_EXCLUSIVA_PROF)==T) %>% count() %>% mutate(n = 224229-n)%>% mutate(prop = round((n)/224229*100, 1)))

rm(escolas)
# IMPORTA TURMAS (BRASIL) E CONTA NA's (NOT AVAIABLE) ####
turmas <- fread("turmas.csv")
                
tbcompletude_turmas <- data.frame(var = "Código único da Turma",`n`=NA, `prop`=NA, check.names = F)  %>% 
  add_row(turmas %>% filter(is.na(ID_TURMA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1)))  %>% 
  
  add_row(var = "Nome da Turma", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(NO_TURMA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Tipo de mediação didático-pedagógica", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(TP_MEDIACAO_DIDATICO_PEDAGO)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Hora Inicial", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(TX_HR_INICIAL)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Hora Inicial - Minuto", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(TX_MI_INICIAL)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Dias da semana da Turma - Domingo", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DIA_SEMANA_DOMINGO)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Dias da semana da Turma - Segunda-Feira", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DIA_SEMANA_SEGUNDA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Dias da semana da Turma - Terça-Feira", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DIA_SEMANA_TERCA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Dias da semana da Turma - Quarta-Feira", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DIA_SEMANA_QUARTA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Dias da semana da Turma - Quinta_Feira", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DIA_SEMANA_QUINTA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Dias da semana da Turma - Sexta-Feira", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DIA_SEMANA_SEXTA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Dias da semana da Turma - Sábado", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DIA_SEMANA_SABADO)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Número de dias por semana em que são realizadas as atividades da turma", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(NU_DIAS_ATIVIDADE)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Duração de funcionamento da Turma - Minutos", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(NU_DURACAO_TURMA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Tipo de atendimento", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(TP_TIPO_ATENDIMENTO_TURMA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Local de funcionamento diferenciado da turma", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(TP_TIPO_LOCAL_TURMA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Código do tipo de atividade complementar - Atividade 1", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(CO_TIPO_ATIVIDADE_1)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Código do tipo de atividade complementar - Atividade 2", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(CO_TIPO_ATIVIDADE_2)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Código do tipo de atividade complementar - Atividade 3", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(CO_TIPO_ATIVIDADE_3)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Código do tipo de atividade complementar - Atividade 4", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(CO_TIPO_ATIVIDADE_4)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Código do tipo de atividade complementar - Atividade 5", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(CO_TIPO_ATIVIDADE_5)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Código do tipo de atividade complementar - Atividade 6", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(CO_TIPO_ATIVIDADE_6)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Etapa de ensino da turma", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(TP_ETAPA_ENSINO)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Curso da Educação Profissional Técnica", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(CO_CURSO_EDUC_PROFISSIONAL)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Turma exclusiva de alunos com deficiência, transtorno global do desenvolvimento ou altas habilidades/superdotação (Classes Especiais)", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_ESPECIAL_EXCLUSIVA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas com etapas de escolarização consecutivas, Creche ao Ensino Médio. Etapas consideradas (nas antigas modalidades 1 ou 2): TP_ETAPA_ENSINO igual a 1,2,3,56,4,5,6,7,8,9,10,11,12,13,14, 15,16,17,18,19,20,21,22,23,24,41,25,26,27,28,29,30,31, 32,33,34,35,36,37 ou 38.", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_REGULAR)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Modo, maneira ou metodologia de ensino correspondente às turmas destinadas a pessoas que não cursaram o ensino fundamental e/ou médio em idade própria. Etapas consideradas (nas antigas modalidades 2 ou 3): TP_ETAPA_ENSINO igual a 65,67,69,70,71,72,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_EJA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Modo profissionalizante de ensino correspondente às turmas de cursos de formação inicial e continuada ou de qualificação profissional (Cursos FIC) articulados à EJA ou concomitantes; ou de cursos técnicos de nível médio nas formas articulada (integrada ou concomitante) ou subsequente ao ensino médio e de normal/magistério. Etapas consideradas (nas antigas modalidades 1, 2 ou 3): TP_ETAPA_ENSINO igual a 30,31,32,33,34, 35,36,37,38,39,40,64,65,67,68,73 ou 74.", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_PROFISSIONALIZANTE)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Número de matrículas", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(QT_MATRICULAS)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Língua/ Literatura Portuguesa", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_LINGUA_PORTUGUESA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Educação Física", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_EDUCACAO_FISICA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Artes (Educação Artística, Teatro, Dança, Música, Artes Plásticas e outras)", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_ARTES)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Língua/ Literatura estrangeira - Inglês", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_LINGUA_INGLES)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Língua/ Literatura estrangeira - Espanhol", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_LINGUA_ESPANHOL)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Língua/ Literatura estrangeira - Francês", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_LINGUA_FRANCES)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Língua/ Literatura estrangeira - Outra", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_LINGUA_OUTRA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Libras", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_LIBRAS)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Língua Indígena", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_LINGUA_INDIGENA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Língua Portuguesa como segunda língua", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_PORT_SEGUNDA_LINGUA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Matemática", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_MATEMATICA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Ciências", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_CIENCIAS)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Física", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_FISICA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Química", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_QUIMICA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Biologia", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_BIOLOGIA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - História", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_HISTORIA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Geografia", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_GEOGRAFIA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Sociologia", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_SOCIOLOGIA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Filosofia", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_FILOSOFIA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Estudos Sociais", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_ESTUDOS_SOCIAIS)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Estudos Sociais ou Sociologia", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_EST_SOCIAIS_SOCIOLOGIA)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Informática / Computação", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_INFORMATICA_COMPUTACAO)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Ensino Religioso", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_ENSINO_RELIGIOSO)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Disciplinas dos cursos técnicos profissionais", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_PROFISSIONALIZANTE)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Estágio curricular supervisionado", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_ESTAGIO_SUPERVISIONADO)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Disciplinas pedagógicas", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_PEDAGOGICAS)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1))) %>%
  
  add_row(var = "Áreas do conhecimento/Componentes curriculares - Outras disciplinas", `n`=NA, `prop`=NA) %>% 
  add_row(turmas %>% filter(is.na(IN_DISC_OUTRAS)==T) %>% count() %>% mutate(n = 2353351-n)%>% mutate(prop = round((n)/2353351*100, 1)))

rm(turmas)
