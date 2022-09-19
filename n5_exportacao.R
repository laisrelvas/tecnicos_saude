# CARREGAR BIBLIOTECAS ####

library(data.table)
library(tidyverse)

# CRIA WORKBOOK PARA EXPORTAÇÃO DAS TABELAS ####
wb = createWorkbook()
wb2 = createWorkbook()

# CRIA ABAS PARA ESCOLAS DO BRASIL ####
addWorksheet(wb, sheetName = "Escolas")
writeData(wb, sheet = "Escolas", x = tb_escolas) 

addWorksheet(wb, sheetName = "Escolas - Estados")
writeData(wb, sheet = "Escolas - Estados", x = tb_estados) 


# JUNTA TABELAS DE ESCOLAS E MATRICULAS PARA GOIÁS ####
tb_go_enf <- rbind(tb_matriculas_GO_Enfermagem, tb_escolas_GO_enf)
tb_go_radio <- rbind(tb_matriculas_GO_Radiologia, tb_escolas_GO_radio)
tb_go_analises <- rbind(tb_matriculas_GO_Analises, tb_escolas_GO_anal_cli)
tb_go_sbucal <- rbind(tb_matriculas_GO_saudebucal, tb_escolas_GO_sbucal)
tb_go_nut <- rbind(tb_matriculas_GO_nutricao, tb_escolas_GO_nut)
tb_go_prot <- rbind(tb_matriculas_GO_protese, tb_escolas_GO_prot)
tb_go_vig <- rbind(tb_matriculas_GO_Vigilancia, tb_escolas_GO_VIG)
tb_go_necro <- rbind(tb_matriculas_GO_necro, tb_escolas_GO_necro)
tb_go_farmacia <- rbind(tb_matriculas_GO_farmacia, tb_escolas_GO_far)

rm(tb_matriculas_GO_Enfermagem, tb_escolas_GO_enf, tb_matriculas_GO_Radiologia, tb_escolas_GO_radio,
   tb_matriculas_GO_Analises, tb_escolas_GO_anal_cli, tb_matriculas_GO_saudebucal, tb_escolas_GO_sbucal,
   tb_matriculas_GO_nutricao, tb_escolas_GO_nut,
   tb_matriculas_GO_protese, tb_escolas_GO_prot,
   tb_matriculas_GO_Vigilancia, tb_escolas_GO_VIG,
   tb_matriculas_GO_necro, tb_escolas_GO_necro,
   tb_matriculas_GO_farmacia, tb_escolas_GO_far)

# CRIA ABAS PARA GOIÁS ####
addWorksheet(wb, sheetName = "Matrículas - GO")
writeData(wb, sheet = "Matrículas - GO", x = tb_matriculas_GO)

addWorksheet(wb, sheetName = "Turmas - GO")
writeData(wb, sheet = "Turmas - GO", x = tb_turmas_GO)

addWorksheet(wb, sheetName = "Escolas - GO")
writeData(wb, sheet = "Escolas - GO", x = tb_escolas_GO)

addWorksheet(wb, sheetName = "GO Enf")
writeData(wb, sheet = "GO Enf", x = tb_go_enf)

addWorksheet(wb, sheetName = "GO Radio")
writeData(wb, sheet = "GO Radio", x = tb_go_radio)

addWorksheet(wb, sheetName = "GO Análises")
writeData(wb, sheet = "GO Análises", x = tb_go_analises)

addWorksheet(wb, sheetName = "GO SBucal")
writeData(wb, sheet = "GO SBucal", x = tb_go_sbucal)

addWorksheet(wb, sheetName = "GO Nutrição")
writeData(wb, sheet = "GO Nutrição", x = tb_go_nut)

addWorksheet(wb, sheetName = "GO Prótese")
writeData(wb, sheet = "GO Prótese", x = tb_go_prot)

addWorksheet(wb, sheetName = "GO Vigilância")
writeData(wb, sheet = "GO Vigilância", x = tb_go_vig)

addWorksheet(wb, sheetName = "GO Necropsia")
writeData(wb, sheet = "GO Necropsia", x = tb_go_necro)

addWorksheet(wb, sheetName = "GO Farmácia")
writeData(wb, sheet = "GO Farmácia", x = tb_go_farmacia)

# JUNTA TABELAS DE ESCOLAS E MATRICULAS PARA PERNAMBUCO ####
tb_pe_enf <- rbind(tb_matriculas_PE_Enfermagem, tb_escolas_PE_enf)
tb_pe_radio <- rbind(tb_matriculas_PE_Radiologia, tb_escolas_PE_radio)
tb_pe_analises <- rbind(tb_matriculas_PE_Analises, tb_escolas_PE_anal_cli)
tb_pe_nutr <- rbind(tb_matriculas_PE_Nutricao, tb_escolas_PE_nut)
tb_pe_far <- rbind(tb_matriculas_PE_Farmacia, tb_escolas_PE_far)
tb_pe_sbucal <- rbind(tb_matriculas_PE_SBucal, tb_escolas_PE_sbucal)
tb_pe_vig <- rbind(tb_matriculas_PE_VigSaude, tb_escolas_PE_VIG)
tb_pe_ger <- rbind(tb_matriculas_PE_Gerencia, tb_escolas_PE_ger)
tb_pe_hemo <- rbind(tb_matriculas_PE_Hemo, tb_escolas_PE_hemo)

rm(tb_matriculas_PE_Enfermagem, tb_escolas_PE_enf,
   tb_matriculas_PE_Radiologia, tb_escolas_PE_radio,
   tb_matriculas_PE_Analises, tb_escolas_PE_anal_cli,
   tb_matriculas_PE_Nutricao, tb_escolas_PE_nut,
   tb_matriculas_PE_Farmacia, tb_escolas_PE_far,
   tb_matriculas_PE_SBucal, tb_escolas_PE_sbucal,
   tb_matriculas_PE_VigSaude, tb_escolas_PE_VIG,
   tb_matriculas_PE_Gerencia, tb_escolas_PE_ger,
   tb_matriculas_PE_Hemo, tb_escolas_PE_hemo)

# CRIA ABAS PARA PERNAMBUCO ####
addWorksheet(wb, sheetName = "Matrículas - PE ")
writeData(wb, sheet = "Matrículas - PE ", x = tb_matriculas_PE)

addWorksheet(wb, sheetName = "Turmas - PE")
writeData(wb, sheet = "Turmas - PE", x = tb_turmas_PE)

addWorksheet(wb, sheetName = "Escolas - PE")
writeData(wb, sheet = "Escolas - PE", x = tb_escolas_PE)

addWorksheet(wb, sheetName = "PE - Enf")
writeData(wb, sheet = "PE - Enf", x = tb_pe_enf)

addWorksheet(wb, sheetName = "PE - Radio")
writeData(wb, sheet = "PE - Radio", x = tb_pe_radio)

addWorksheet(wb, sheetName = "PE - Análises")
writeData(wb, sheet = "PE - Análises", x = tb_pe_analises)

addWorksheet(wb, sheetName = "PE - Nutrição")
writeData(wb, sheet = "PE - Nutrição", x = tb_pe_nutr)

addWorksheet(wb, sheetName = "PE - Farmácia")
writeData(wb, sheet = "PE - Farmácia", x = tb_pe_far)

addWorksheet(wb, sheetName = "PE - SBucal")
writeData(wb, sheet = "PE - SBucal", x = tb_pe_sbucal)

addWorksheet(wb, sheetName = "PE - Vigilância")
writeData(wb, sheet = "PE - Vigilância", x = tb_pe_vig)

addWorksheet(wb, sheetName = "PE - Gerência")
writeData(wb, sheet = "PE - Gerência", x = tb_pe_ger)

addWorksheet(wb, sheetName = "PE - Hemoterapia")
writeData(wb, sheet = "PE - Hemoterapia", x = tb_pe_hemo)



# JUNTA TABELAS DE ESCOLAS E MATRICULAS PARA RIO GRANDE DO SUL ####
tb_rs_enf <- rbind(tb_matriculas_RS_Enfermagem, tb_escolas_RS_enf)
tb_RS_radio <- rbind(tb_matriculas_RS_Radiologia, tb_escolas_RS_radio)
tb_RS_nut <- rbind(tb_matriculas_RS_nutricao, tb_escolas_RS_nut)
tb_RS_analises <- rbind(tb_matriculas_RS_Analises, tb_escolas_RS_anal_cli)
tb_RS_protese <- rbind(tb_matriculas_RS_protese, tb_escolas_RS_prot)
tb_RS_farmacia <- rbind(tb_matriculas_RS_farmacia, tb_escolas_RS_far)
tb_RS_gerencia <- rbind(tb_matriculas_RS_gerencia, tb_escolas_RS_ger)
tb_RS_masso <- rbind(tb_matriculas_RS_massot, tb_escolas_RS_masso)
tb_RS_sbucal <- rbind(tb_matriculas_RS_saudebucal, tb_escolas_RS_sbucal)
tb_RS_cIdosos <- rbind(tb_matriculas_RS_CuIdosos, tb_escolas_RS_cuidoso)
tb_RS_cito <- rbind(tb_matriculas_RS_citop, tb_escolas_RS_cito)

rm(tb_matriculas_RS_Enfermagem, tb_escolas_RS_enf,
   tb_matriculas_RS_Radiologia, tb_escolas_RS_radio,
   tb_matriculas_RS_nutricao, tb_escolas_RS_nut,
   tb_matriculas_RS_Analises, tb_escolas_RS_anal_cli,
   tb_matriculas_RS_protese, tb_escolas_RS_prot,
   tb_matriculas_RS_farmacia, tb_escolas_RS_far,
   tb_matriculas_RS_gerencia, tb_escolas_RS_ger,
   tb_matriculas_RS_massot, tb_escolas_RS_masso,
   tb_matriculas_RS_saudebucal, tb_escolas_RS_sbucal,
   tb_matriculas_RS_CuIdosos, tb_escolas_RS_cuidoso,
   tb_matriculas_RS_citop, tb_escolas_RS_cito)

# CRIA ABAS PARA RIO GRANDE DO SUL ####

addWorksheet(wb, sheetName = "Matrículas - RS")
writeData(wb, sheet = "Matrículas - RS", x = tb_matriculas_RS)

addWorksheet(wb, sheetName = "Turmas - RS")
writeData(wb, sheet = "Turmas - RS", x = tb_turmas_RS)

addWorksheet(wb, sheetName = "Escolas - RS")
writeData(wb, sheet = "Escolas - RS", x = tb_escolas_RS)

addWorksheet(wb, sheetName = "RS - Enf")
writeData(wb, sheet = "RS - Enf", x = tb_rs_enf)

addWorksheet(wb, sheetName = "RS - Radio")
writeData(wb, sheet = "RS - Radio", x = tb_RS_radio)

addWorksheet(wb, sheetName = "RS - Nutrição")
writeData(wb, sheet = "RS - Nutrição", x = tb_RS_nut)

addWorksheet(wb, sheetName = "RS - Análises")
writeData(wb, sheet = "RS - Análises", x = tb_RS_analises)

addWorksheet(wb, sheetName = "RS - Prótese")
writeData(wb, sheet = "RS - Prótese", x = tb_RS_protese)

addWorksheet(wb, sheetName = "RS - Farmácia")
writeData(wb, sheet = "RS - Farmácia", x = tb_RS_farmacia)

addWorksheet(wb, sheetName = "RS - Gerência")
writeData(wb, sheet = "RS - Gerência", x = tb_RS_gerencia)

addWorksheet(wb, sheetName = "RS - Massoterapia")
writeData(wb, sheet = "RS - Massoterapia", x = tb_RS_masso)

addWorksheet(wb, sheetName = "RS - Saúde Bucal")
writeData(wb, sheet = "RS - Saúde Bucal", x = tb_RS_sbucal)

addWorksheet(wb, sheetName = "RS - CIdosos")
writeData(wb, sheet = "RS - CIdosos", x = tb_RS_cIdosos)

addWorksheet(wb, sheetName = "RS - Citopatologia")
writeData(wb, sheet = "RS - Citopatologia", x = tb_RS_cito)

# JUNTA TABELAS DE ESCOLAS E MATRICULAS PARA MARANHÃO ####
tb_ma_analises <- rbind(tb_matriculas_MA_Analises, tb_escolas_MA_anal_cli)
tb_ma_enf <- rbind(tb_matriculas_MA_Enfermagem, tb_escolas_MA_enf)
tb_ma_equipbio <- rbind(tb_matriculas_MA_equipebiom, tb_escolas_MA_equip_biom)
tb_ma_farmacia <- rbind(tb_matriculas_MA_farmacia, tb_escolas_MA_far)
tb_ma_ger <- rbind(tb_matriculas_MA_gerencia, tb_escolas_MA_ger)
tb_ma_nut <- rbind(tb_matriculas_MA_nutricao, tb_escolas_MA_nut)
tb_ma_radio <- rbind(tb_matriculas_MA_Radiologia, tb_escolas_MA_radio)
tb_ma_reg <- rbind(tb_matriculas_MA_registros, tb_escolas_MA_reg)
tb_ma_sbucal <- rbind(tb_matriculas_MA_saudebucal, tb_escolas_MA_sbucal)

rm(tb_matriculas_MA_Analises, tb_escolas_MA_anal_cli,
   tb_matriculas_MA_Enfermagem, tb_escolas_MA_enf,
   tb_matriculas_MA_equipebiom, tb_escolas_MA_equip_biom,
   tb_matriculas_MA_farmacia, tb_escolas_MA_far,
   tb_matriculas_MA_gerencia, tb_escolas_MA_ger,
   tb_matriculas_MA_nutricao, tb_escolas_MA_nut,
   tb_matriculas_MA_Radiologia, tb_escolas_MA_radio,
   tb_matriculas_MA_registros, tb_escolas_MA_reg,
   tb_matriculas_MA_saudebucal, tb_escolas_MA_sbucal)

# CRIA ABAS PARA MARANHÃO ####
addWorksheet(wb, sheetName = "Matrículas - MA")
writeData(wb, sheet = "Matrículas - MA", x = tb_matriculas_MA)

addWorksheet(wb, sheetName = "Turmas - MA")
writeData(wb, sheet = "Turmas - MA", x = tb_turmas_MA)

addWorksheet(wb, sheetName = "Escolas - MA")
writeData(wb, sheet = "Escolas - MA", x = tb_escolas_MA)

addWorksheet(wb, sheetName = "MA - Enf")
writeData(wb, sheet = "MA - Enf", x = tb_ma_enf)

addWorksheet(wb, sheetName = "MA - Radio")
writeData(wb, sheet = "MA - Radio", x = tb_ma_radio)

addWorksheet(wb, sheetName = "MA - Análises")
writeData(wb, sheet = "MA - Análises", x = tb_ma_analises)

addWorksheet(wb, sheetName = "MA - Farmácia")
writeData(wb, sheet = "MA - Farmácia", x = tb_ma_farmacia)

addWorksheet(wb, sheetName = "MA - Nutrição")
writeData(wb, sheet = "MA - Nutrição", x = tb_ma_nut)

addWorksheet(wb, sheetName = "MA - S Bucal")
writeData(wb, sheet = "MA - S Bucal", x = tb_ma_sbucal)

addWorksheet(wb, sheetName = "MA - Gerência")
writeData(wb, sheet = "MA - Gerência", x = tb_ma_ger)

addWorksheet(wb, sheetName = "MA - Equip Biom")
writeData(wb, sheet = "MA - Equip Biom", x = tb_ma_equipbio)

addWorksheet(wb, sheetName = "MA - Registros")
writeData(wb, sheet = "MA - Registros", x = tb_ma_reg)

# JUNTA TABELAS DE ESCOLAS E MATRICULAS PARA RONDÔNIA ####
tb_ro_enf <- rbind(tb_matriculas_RO_Enfermagem, tb_escolas_RO_enf)
tb_ro_analises <- rbind(tb_matriculas_RO_Analises, tb_escolas_RO_anal_cli)
tb_ro_nut <- rbind(tb_matriculas_RO_nutricao, tb_escolas_RO_nut)
tb_ro_prot <- rbind(tb_matriculas_RO_protese, tb_escolas_RO_prot)
tb_ro_radio <- rbind(tb_matriculas_RO_Radiologia, tb_escolas_RO_radio)

rm(tb_matriculas_RO_Enfermagem, tb_escolas_RO_enf,
   tb_matriculas_RO_Analises, tb_escolas_RO_anal_cli,
   tb_matriculas_RO_nutricao, tb_escolas_RO_nut, 
   tb_matriculas_RO_protese, tb_escolas_RO_prot, 
   tb_matriculas_RO_Radiologia, tb_escolas_RO_radio)

# CRIA ABAS PARA RONDÔNIA ####
addWorksheet(wb, sheetName = "Matrículas - RO")
writeData(wb, sheet = "Matrículas - RO", x = tb_matriculas_RO)

addWorksheet(wb, sheetName = "Turmas - RO")
writeData(wb, sheet = "Turmas - RO", x = tb_turmas_RO)

addWorksheet(wb, sheetName = "Escolas - RO")
writeData(wb, sheet = "Escolas - RO", x = tb_escolas_RO) 

addWorksheet(wb, sheetName = "RO - Enf")
writeData(wb, sheet = "RO - Enf", x = tb_ro_enf) 

addWorksheet(wb, sheetName = "RO - Radiologia")
writeData(wb, sheet = "RO - Radiologia", x = tb_ro_radio) 

addWorksheet(wb, sheetName = "RO - Análises")
writeData(wb, sheet = "RO - Análises", x = tb_ro_analises) 

addWorksheet(wb, sheetName = "RO - Nutrição")
writeData(wb, sheet = "RO - Nutrição", x = tb_ro_nut) 

addWorksheet(wb, sheetName = "RO - Prótese")
writeData(wb, sheet = "RO - Prótese", x = tb_ro_prot) 

# CRIA ABAS PARA COMPLETUDE ####
addWorksheet(wb2, sheetName = "Completude - Escolas")
writeData(wb2, sheet = "Completude - Escolas", x = tbcompletude_escolas)

addWorksheet(wb2, sheetName = "Completude - Turmas")
writeData(wb2, sheet = "Completude - Turmas", x = tbcompletude_turmas) 

addWorksheet(wb2, sheetName = "Completude - Matrículas - SE")
writeData(wb2, sheet = "Completude - Matrículas - SE", x = tbcompletude_se) 

addWorksheet(wb2, sheetName = "Completude - Matrículas - N")
writeData(wb2, sheet = "Completude - Matrículas - N", x = tbcompletude_no) 

addWorksheet(wb2, sheetName = "Completude - Matrículas - NE")
writeData(wb2, sheet = "Completude - Matrículas - NE", x = tbcompletude_ne) 

addWorksheet(wb2, sheetName = "Completude - Matrículas - S")
writeData(wb2, sheet = "Completude - Matrículas - S", x = tbcompletude_sul) 

addWorksheet(wb2, sheetName = "Completude - Matrículas - CO")
writeData(wb2, sheet = "Completude - Matrículas - CO", x = tbcompletude_co) 

# EXPORTAR TABELAS ####
# DO DESCRITVO 
saveWorkbook(wb, "C:/Users/laisr/OneDrive/Documentos/Área de Trabalho/CENSO ESCOLAR/analise/github/relatorio_raw.xlsx.xlsx", overwrite = T)
# DA COMPLETUDE
saveWorkbook(wb, "C:/Users/laisr/OneDrive/Documentos/Área de Trabalho/CENSO ESCOLAR/analise/github/resultados/relatorio_raw.xlsx", overwrite = T)