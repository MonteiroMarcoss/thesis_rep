library(tidyverse)
library(plm)
library(stargazer)
library(tsibble)

# variaveis macro ---------------------------------------------------------

ids <- c('pib' = 24363, #índice
         'inflacao' = 433, #v% mensal
         'juros' = 4189, #v% anual
         'cambio' = 3698) # umc/dólar

cov_macro <- GetBCBData::gbcbd_get_series(
 id = ids,
 first.date = '2004-01-01',
 last.date = Sys.Date(),
 format.data = 'wide') |>
 transmute(Data = ref.date,
           ibc = 100*difference(log(pib)), 
           ipca = inflacao,
           selic = juros,
           taxa_cambio = 100*difference(log(cambio))) 

# dados --------------------------------------------------------------------

base <- read_csv("base_balancetes.csv") %>%
  filter(DOCUMENTO == 4010, ) %>%
 mutate(DATA = as.Date(paste0(as.character(DATA), "01"), format = "%Y%m%d"))

cadastro <- read_csv("cadastro_ifs.csv") |> filter(Tcb == 'B1')

bancos <- cadastro %>%
  filter(Situacao == "A") %>%
  pull(CnpjInstituicaoLider) %>%
  unique()

base <- base %>%
  filter(CNPJ %in% bancos) %>%
  drop_na()

#dic <- base %>%
 # distinct(CONTA, `NOME CONTA`)

#dic %>% filter(grepl("11000006", `NOME CONTA`))

painel_full <- base %>%
  select(-`NOME CONTA`, -DOCUMENTO) %>%
  pivot_wider(names_from = "CONTA", values_from = "SALDO") %>%
  transmute(
    Data = DATA,
    CNPJ = CNPJ,
    Banco = `NOME INSTITUICAO`,
    roa = 100*(-`89000007`/(`10000007` + `20000004`)), # multiplicar por cem
    roe = 100*((-`89000007`/`61000001`)), # multiplicar por cem
    ativo = log(`10000007` + `20000004`), # nao multiplicar por cem
    pl_sobre_ativo = 100*(`61000001`/(`10000007` + `20000004`)), # multiplicar por cem
    receitas_sobre_ativo = 100*(`71000008`/(`10000007` + `20000004`)), # multiplicar por cem
    lucro.prejuizo_acumulado = 100*(`61800005`/(`10000007` + `20000004`)), # multiplicar por cem
    ativos_liquidos = 100*((`11000006` + `12000005`)/(`10000007` + `20000004`)), # multiplicar por cem
    depositos = 100*(`41000007`/(`10000007` + `20000004`)) # multiplicar por cem
  )


# imapp -------------------------------------------------------------------

imapp <- read.csv('Brazil_imap.csv')|>
 mutate(Data = as.Date(DATA)) |> filter(DATA >= '2003-12-31') |>
 select(-DATA, -ifscode, -iso3,-iso2,-AE,-EMDE,-Country) |> 
 rename('CCoB' = 'Conservation')

# Taylor Gap --------------------------------------------------------------

df_TG <- readxl::read_xlsx('df_TG.xlsx')|>
 mutate(Data = as.Date(Data)) |> select(-selic) |>
 mutate(dummy_tg = ifelse(taylor_gap > 0, 1, 0))


# diferença meta - inflação -----------------------------------------------

#meta_inflacao <- readxl::read_xlsx('meta.xlsx')

# Painel completo -------------------------------------------------------------------

painel_full <- painel_full |> 
 inner_join(cov_macro, by = 'Data') |>
 inner_join(imapp, by = 'Data') |>
 inner_join(df_TG, by = 'Data') 

painel_full <- pdata.frame(painel_full, index = c('CNPJ', 'Data'))

# número de bancos
#n_b <- painel_full |> pull(Banco) |> unique() 
#glimpse(n_b)

# Painel bancos S1 (banco grande) --------------------------------------------

bancos_s1 <- cadastro|>
 filter(Situacao == "A", Sr == 'S1') |>
 pull(CnpjInstituicaoLider) |> 
 unique()

base_s1 <- base |>
 filter(CNPJ %in% bancos_s1) |>
 na.omit() 

painel_s1 <- base_s1 |>
 select(-`NOME CONTA`, -DOCUMENTO) |>
 pivot_wider(names_from = 'CONTA',
             values_from = 'SALDO') |>
 transmute(
  Data = DATA,
  CNPJ = CNPJ,
  Banco = `NOME INSTITUICAO`,
  roa = 100*(-`89000007`/(`10000007` + `20000004`)), # multiplicar por cem
  roe = 100*((-`89000007`/`61000001`)), # multiplicar por cem
  ativo = log(`10000007` + `20000004`), # nao multiplicar por cem
  pl_sobre_ativo = 100*(`61000001`/(`10000007` + `20000004`)), # multiplicar por cem
  receitas_sobre_ativo = 100*(`71000008`/(`10000007` + `20000004`)), # multiplicar por cem
  lucro.prejuizo_acumulado = 100*(`61800005`/(`10000007` + `20000004`)), # multiplicar por cem
  ativos_liquidos = 100*((`11000006` + `12000005`)/(`10000007` + `20000004`)), # multiplicar por cem
  depositos = 100*(`41000007`/(`10000007` + `20000004`)) # multiplicar por cem
 ) 

painel_s1 <- painel_s1 |> 
 inner_join(cov_macro) |>
 inner_join(imapp)|>
 inner_join(df_TG)

painel_s1 <- pdata.frame(painel_s1, index = c('CNPJ', 'Data'))


# Painel S2 ---------------------------------------------------------------

bancos_s2 <- cadastro|>
 filter(Situacao == 'A',Sr == 'S2') |>
 pull(CnpjInstituicaoLider) |> 
 unique() 

base_s2 <- base |>
 filter(CNPJ %in% bancos_s2)

painel_s2 <- base_s2 |>
 select(-DOCUMENTO, -`NOME CONTA`) |>
 pivot_wider(names_from = CONTA,
             values_from = SALDO) |>
 transmute(
  Data = DATA,
  CNPJ = CNPJ,
  Banco = `NOME INSTITUICAO`,
  roa = 100*(-`89000007`/(`10000007` + `20000004`)), # multiplicar por cem
  roe = 100*((-`89000007`/`61000001`)), # multiplicar por cem
  ativo = log(`10000007` + `20000004`), # nao multiplicar por cem
  pl_sobre_ativo = 100*(`61000001`/(`10000007` + `20000004`)), # multiplicar por cem
  receitas_sobre_ativo = 100*(`71000008`/(`10000007` + `20000004`)), # multiplicar por cem
  lucro.prejuizo_acumulado = 100*(`61800005`/(`10000007` + `20000004`)), # multiplicar por cem
  ativos_liquidos = 100*((`11000006` + `12000005`)/(`10000007` + `20000004`)), # multiplicar por cem
  depositos = 100*(`41000007`/(`10000007` + `20000004`)) # multiplicar por cem
 )

painel_s2 <- painel_s2 |> 
 inner_join(cov_macro) |>
 inner_join(imapp) |>
 inner_join(df_TG)

painel_s2 <- pdata.frame(painel_s2, index = c('CNPJ', 'Data'))


# Bancos s3 ---------------------------------------------------------------

bancos_s3 <- cadastro|>
 filter(Situacao == "A", Sr == 'S3') |>
 pull(CnpjInstituicaoLider) |> 
 unique()

base_s3 <- base |>
 filter(CNPJ %in% bancos_s3) |>
 na.omit() 

painel_s3 <- base_s3 |>
 select(-`NOME CONTA`, -DOCUMENTO) |>
 pivot_wider(names_from = 'CONTA',
             values_from = 'SALDO') |>
 transmute(
  Data = DATA,
  CNPJ = CNPJ,
  Banco = `NOME INSTITUICAO`,
  roa = 100*(-`89000007`/(`10000007` + `20000004`)), # LUCRO / ativo
  roe = 100*((-`89000007`/`61000001`)), #  LUCRO / PL
  ativo = log(`10000007` + `20000004`), # nao multiplicar por cem
  pl_sobre_ativo = 100*(`61000001`/(`10000007` + `20000004`)), # multiplicar por cem
  receitas_sobre_ativo = 100*(`71000008`/(`10000007` + `20000004`)), # multiplicar por cem
  lucro.prejuizo_acumulado = 100*(`61800005`/(`10000007` + `20000004`)), # multiplicar por cem
  ativos_liquidos = 100*((`11000006` + `12000005`)/(`10000007` + `20000004`)), # multiplicar por cem
  depositos = 100*(`41000007`/(`10000007` + `20000004`)) # multiplicar por cem
 ) 

painel_s3 <- painel_s3 |> 
 inner_join(cov_macro) |>
 inner_join(imapp)|>
 inner_join(df_TG)

painel_s3 <- pdata.frame(painel_s3, index = c('CNPJ', 'Data'))

# bancos s4 ---------------------------------------------------------------

bancos_s4 <- cadastro|>
 filter(Situacao == "A", Sr == 'S4') |>
 pull(CnpjInstituicaoLider) |> 
 unique()

base_s4 <- base |>
 filter(CNPJ %in% bancos_s4) |>
 na.omit() 

painel_s4 <- base_s4 |>
 select(-`NOME CONTA`, -DOCUMENTO) |>
 pivot_wider(names_from = 'CONTA',
             values_from = 'SALDO') |>
 transmute(
  Data = DATA,
  CNPJ = CNPJ,
  Banco = `NOME INSTITUICAO`,
  roa = 100*(-`89000007`/(`10000007` + `20000004`)), # multiplicar por cem
  roe = 100*((-`89000007`/`61000001`)), # multiplicar por cem
  ativo = log(`10000007` + `20000004`), # nao multiplicar por cem
  pl_sobre_ativo = 100*(`61000001`/(`10000007` + `20000004`)), # multiplicar por cem
  receitas_sobre_ativo = 100*(`71000008`/(`10000007` + `20000004`)), # multiplicar por cem
  lucro.prejuizo_acumulado = 100*(`61800005`/(`10000007` + `20000004`)), # multiplicar por cem
  ativos_liquidos = 100*((`11000006` + `12000005`)/(`10000007` + `20000004`)), # multiplicar por cem
  depositos = 100*(`41000007`/(`10000007` + `20000004`)) # multiplicar por cem
 ) 

painel_s4 <- painel_s4 |> 
 inner_join(cov_macro) |>
 inner_join(imapp) |>
 inner_join(df_TG)

painel_s4 <- pdata.frame(painel_s4, index = c('CNPJ', 'Data'))

# tabela descritiva -------------------------------------------------------

tab <- painel_full |>
 select(roa, roe, ativo, ativos_liquidos, receitas_sobre_ativo,
        lucro.prejuizo_acumulado, depositos)

tab_desc <- skim(tab)
tab_desc <- tab_desc |> select(skim_variable,
                               numeric.mean,
                               numeric.sd,
                               numeric.p0, 
                               numeric.p25,
                               numeric.p50,
                               numeric.p75,
                               numeric.p100) |>
 mutate(max = max(painel_full$roa, na.rm = T))


a <- tab_desc |> pivot_longer(
 names_to = 'estatisticas',
 values_to = 'valores',
 cols = everything())

xtable::xtable(tab_desc)
# desc_omitir -------------------------------------------------------------


tab_desc <- painel_full |>
 summarise(roa_media = mean(roa, na.rm = T),
           roa_desviop = sd(roa, na.rm = T),
           roa_min = min(roa, na.rm = T),
           roa_max = max(roa, na.rm = T),
           roe_media = mean(roe, na.rm = T),
           roe_desviop = sd(roe, na.rm = T),
           roe_min = min(roe, na.rm = T),
           roe_max = max(roe, na.rm = T),
           ativo_media = mean(ativo, na.rm = T),
           ativo_desviop = sd(ativo, na.rm = T),
           ativo_min = min(ativo, na.rm = T),
           ativo_max = max(ativo, na.rm = T),
           pl_media = mean(pl_sobre_ativo, na.rm = T),
           pl_desviop = sd(pl_sobre_ativo, na.rm = T),
           pl_min = min(pl_sobre_ativo, na.rm = T),
           pl_max = max(pl_sobre_ativo, na.rm = T),
           rt_media = mean(receitas_sobre_ativo, na.rm = T),
           rt_desviop = sd(receitas_sobre_ativo, na.rm = T),
           rt_min = min(receitas_sobre_ativo, na.rm = T),
           rt_max = max(receitas_sobre_ativo, na.rm = T),
           lucro_media = mean(lucro.prejuizo_acumulado, na.rm = T),
           lucro_desviop = sd(lucro.prejuizo_acumulado, na.rm = T),
           lucro_min = min(lucro.prejuizo_acumulado, na.rm = T),
           lucro_max = max(lucro.prejuizo_acumulado, na.rm = T),
           ativos_liquidos_media = mean(ativos_liquidos, na.rm = T),
           ativos_liquidos_desviop = sd(ativos_liquidos, na.rm = T),
           ativos_liquidos_min = min(ativos_liquidos, na.rm = T),
           ativos_liquidos_max = max(ativos_liquidos, na.rm = T),
           depositos_media = mean(depositos, na.rm = T),
           depositos_desviop = sd(depositos, na.rm = T),
           depositos_min = min(depositos, na.rm = T),
           depositos_max = max(depositos, na.rm = T)
           ) |>
 pivot_longer(
  cols = everything(),
  names_to = c("variable", ".value"),
  names_pattern = "(.*)_(.*)"
 )

#writexl::write_xlsx(tab_desc, 'tab_desc.xlsx')


