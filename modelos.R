
# Modelo - painel completo -----------------------------------------------------------------

## Roa - return on assets = RT/Investimento
#
roa1 <- plm(roa ~ CCoB +
                  Capital +
                  Tax +
                  Liquidity +
                  RR +
                  RR_FCD +
                  ativo + 
                  pl_sobre_ativo + 
                  receitas_sobre_ativo + 
                  depositos +
                  lucro.prejuizo_acumulado +
                  ativos_liquidos +
                  ibc +
                  ipca +
                  taxa_cambio,
                  data = painel_full, model = 'within')


roa2 <- plm(roa ~ SUM_17 +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
             data = painel_full, model = 'within')

## Roe - Revenue on equity = RT/P.liquido
roe1 <- plm(roe ~ CCoB +
                  Capital +
                  Tax +
                  Liquidity +
                  RR +
                  RR_FCD +
                  ativo + 
                  pl_sobre_ativo + 
                  receitas_sobre_ativo + 
                  depositos +
                  lucro.prejuizo_acumulado +
                  ativos_liquidos +
                  ibc +
                  ipca +
                  taxa_cambio, data = painel_full, model = 'within')


roe2 <- plm(roe ~ SUM_17 + 
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos + 
             lucro.prejuizo_acumulado + 
             ativos_liquidos + 
             ibc + 
             ipca + 
             taxa_cambio, 
            data = painel_full, model = 'within')


# Modelo Painel - S1 --------------------------------------------------

## Roa
roa3 <- plm(roa ~ CCoB +
             Capital +
             Tax +
             Liquidity +
             RR +
             RR_FCD +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
             data = painel_s1, model = 'within')


roa4 <- plm(roa ~ SUM_17 + 
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos + 
             lucro.prejuizo_acumulado + 
             ativos_liquidos + 
             ibc + 
             ipca + 
             taxa_cambio,
             data = painel_s1, model = 'within')

## Roe - Revenue on equity = RT/P.liquido
roe3 <- plm(roe ~ CCoB +
             Capital +
             Tax +
             Liquidity +
             RR +
             RR_FCD +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
             data = painel_s1, model = 'within')


roe4 <- plm(roe ~ SUM_17 + 
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos + 
             lucro.prejuizo_acumulado + 
             ativos_liquidos + 
             ibc + 
             ipca + 
             taxa_cambio,
             data = painel_s1, model = 'within')


# Modelo Painel S2 - bancos menores ---------------------------------------------------

## Roa
roa5 <- plm(roa ~ CCoB +
             Capital +
             Tax +
             Liquidity +
             RR +
             RR_FCD +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
             data = painel_s2, model = 'within')


roa6 <- plm(roa ~  SUM_17 +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
             data = painel_s2, model = 'within')

## Roe - Revenue on equity = RT/P.liquido
roe5 <- plm(roe ~ CCoB +
             Capital +
             Tax +
             Liquidity +
             RR +
             RR_FCD +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
             data = painel_s2, model = 'within')


roe6 <- plm(roe ~  SUM_17 +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
             data = painel_s2, model = 'within')

# Modelo Painel s3 - bancos menores ainda ----------------------------------------
## Roa
roa7 <- plm(roa ~ CCoB +
             Capital +
             Tax +
             Liquidity +
             RR +
             RR_FCD +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
             data = painel_s3, model = 'within')


roa8 <- plm(roa ~  SUM_17 +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
             data = painel_s3, model = 'within')

## Roe - Revenue on equity = RT/P.liquido
roe7 <- plm(roe ~ CCoB +
             Capital +
             Tax +
             Liquidity +
             RR +
             RR_FCD +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
             data = painel_s3, model = 'within')


roe8 <- plm(roe ~  SUM_17 +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
             data = painel_s3, model = 'within')

# S4 ----------------------------------------------------------------------

roa9 <- plm(roa ~ CCoB +
             Capital +
             Tax +
             Liquidity +
             RR +
             RR_FCD +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
            data = painel_s4,
            model = 'within')


roa10 <- plm(roa ~  SUM_17 +
              ativo + 
              pl_sobre_ativo + 
              receitas_sobre_ativo + 
              depositos +
              lucro.prejuizo_acumulado +
              ativos_liquidos +
              ibc +
              ipca +
              taxa_cambio,
             data = painel_s4,
             model = 'within')

## Roe - Revenue on equity = RT/P.liquido
roe9 <- plm(roe ~ CCoB +
             Capital +
             Tax +
             Liquidity +
             RR +
             RR_FCD +
             ativo + 
             pl_sobre_ativo + 
             receitas_sobre_ativo + 
             depositos +
             lucro.prejuizo_acumulado +
             ativos_liquidos +
             ibc +
             ipca +
             taxa_cambio,
            data = painel_s4,
            model = 'within')


roe10 <- plm(roe ~  SUM_17 +
              ativo + 
              pl_sobre_ativo + 
              receitas_sobre_ativo + 
              depositos +
              lucro.prejuizo_acumulado +
              ativos_liquidos +
              ibc +
              ipca +
              taxa_cambio,
             data = painel_s4,
             model = 'within')