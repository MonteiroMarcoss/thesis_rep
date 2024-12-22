library(tidyverse)
library(plm)
library(stargazer)
library(tsibble)

# variaveis macro ---------------------------------------------------------

ids <- c('pib' = 24363, # em nível
         'inflacao' = 433, #v% mensal
         'juros' = 4189, #v% anual
         'cambio' = 3698) # umc/dólar, em nível

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

# Painel completo -------------------------------------------------------------------

painel_full <- painel_full |> 
 inner_join(cov_macro, by = 'Data') |>
 inner_join(imapp, by = 'Data') |>
 inner_join(df_TG, by = 'Data') 

painel_full <- pdata.frame(painel_full, index = c('CNPJ', 'Data'))

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
