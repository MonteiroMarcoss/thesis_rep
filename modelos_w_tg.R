
# Modelo - painel completo -----------------------------------------------------------------

## Roa - revenue on assets = RT/Investimento
#
roa1_tg <- plm(roa ~ dummy_tg:CCoB + 
                dummy_tg:Capital +
                dummy_tg:Tax +
                dummy_tg:Liquidity +
                dummy_tg:RR +
                dummy_tg:RR_FCD +
                dummy_tg +
                CCoB +
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

roa2_tg <- plm(roa ~ dummy_tg:SUM_17 +
                dummy_tg +
                SUM_17 +
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
roe1_tg <- plm(roe ~ dummy_tg:CCoB + 
                dummy_tg:Capital +
                dummy_tg:Tax +
                dummy_tg:Liquidity +
                dummy_tg:RR +
                dummy_tg:RR_FCD +
                dummy_tg +
                CCoB +
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


roe2_tg <- plm(roe ~ dummy_tg:SUM_17 +
                dummy_tg +
                SUM_17 +
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
roa3_tg <- plm(roa ~ dummy_tg:CCoB + 
                dummy_tg:Capital +
                dummy_tg:Tax +
                dummy_tg:Liquidity +
                dummy_tg:RR +
                dummy_tg:RR_FCD +
                dummy_tg +
                CCoB +
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


roa4_tg <- plm(roa ~ dummy_tg:SUM_17 +
                dummy_tg +
                SUM_17 +
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
roe3_tg <- plm(roe ~ dummy_tg:CCoB + 
                dummy_tg:Capital +
                dummy_tg:Tax +
                dummy_tg:Liquidity +
                dummy_tg:RR +
                dummy_tg:RR_FCD +
                dummy_tg +
                CCoB +
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


roe4_tg <- plm(roe ~ dummy_tg:SUM_17 +
                dummy_tg +
                SUM_17 +
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
roa5_tg <- plm(roa ~ dummy_tg:CCoB + 
                dummy_tg:Capital +
                dummy_tg:Tax +
                dummy_tg:Liquidity +
                dummy_tg:RR +
                dummy_tg:RR_FCD +
                dummy_tg +
                CCoB +
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


roa6_tg <- plm(roa ~ dummy_tg:SUM_17 +
                dummy_tg +
                SUM_17 +
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
roe5_tg <- plm(roe ~ dummy_tg:CCoB + 
                dummy_tg:Capital +
                dummy_tg:Tax +
                dummy_tg:Liquidity +
                dummy_tg:RR +
                dummy_tg:RR_FCD +
                dummy_tg +
                CCoB +
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


roe6_tg <- plm(roe ~ dummy_tg:SUM_17 +
                dummy_tg +
                SUM_17 +
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
roa7_tg <- plm(roa ~ dummy_tg:CCoB + 
                dummy_tg:Capital +
                dummy_tg:Tax +
                dummy_tg:Liquidity +
                dummy_tg:RR +
                dummy_tg:RR_FCD +
                dummy_tg +
                CCoB +
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


roa8_tg <- plm(roa ~ dummy_tg:SUM_17 +
                dummy_tg +
                SUM_17 +
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
roe7_tg <- plm(roe ~ dummy_tg:CCoB + 
                dummy_tg:Capital +
                dummy_tg:Tax +
                dummy_tg:Liquidity +
                dummy_tg:RR +
                dummy_tg:RR_FCD +
                dummy_tg +
                CCoB +
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


roe8_tg <- plm(roe ~ dummy_tg:SUM_17 +
                dummy_tg +
                SUM_17 +
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

roa9_tg <- plm(roa ~ dummy_tg:CCoB + 
                dummy_tg:Capital +
                dummy_tg:Tax +
                dummy_tg:Liquidity +
                dummy_tg:RR +
                dummy_tg:RR_FCD +
                dummy_tg +
                CCoB +
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


roa10_tg <- plm(roa ~ dummy_tg:SUM_17 +
                 dummy_tg +
                 SUM_17 +
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
roe9_tg <- plm(roe ~ dummy_tg:CCoB + 
                dummy_tg:Capital +
                dummy_tg:Tax +
                dummy_tg:Liquidity +
                dummy_tg:RR +
                dummy_tg:RR_FCD +
                dummy_tg +
                CCoB +
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


roe10_tg <- plm(roe ~ dummy_tg:SUM_17 +
                 dummy_tg +
                 SUM_17 +
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

