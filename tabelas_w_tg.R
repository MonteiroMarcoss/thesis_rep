
# vetores de ordenamento --------------------------------------------------


pm_5 <- c('dummy_tg:CCoB','dummy_tg:Capital', 'dummy_tg:Tax', 'dummy_tg:Liquidity',
          'dummy_tg:RR', 'dummy_tg:RR_FCD', 'dummy_tg', 'CCoB', 'Capital', 'Tax',
          'Liquidity', 'RR', 'RR_FCD', 'ativo', 'pl_sobre_ativo', 'receitas_sobre_ativo', 
          'depositos', 'lucro.prejuizo_acumulado', 'ativos_liquidos', 'ibc', 'ipca',
          'taxa_cambio')

pm_sum12 <- c('dummy_tg:SUM_17', 'dummy_tg', 'SUM_17', 'ativo', 'pl_sobre_ativo',
              'receitas_sobre_ativo', 'depositos', 'lucro.prejuizo_acumulado',
              'ativos_liquidos', 'ibc', 'ipca', 'taxa_cambio')


# Tabelas -----------------------------------------------------------------

tabela_6 <- stargazer(
 roa1_tg, roa2_tg, roe1_tg, roe2_tg, type = 'text' ,title = 'Painel completo - Interação com P. Monetária'
)

tabela_7 <- stargazer(
 roa3_tg, roa4_tg, roe3_tg, roe4_tg, type = 'text',title = 'Painel Bancos S1 - tg' #sem significância
)

tabela_8 <- stargazer(
 roa5_tg, roa6_tg, roe5_tg, roe6_tg, title = "Painel Bancos S2 - tg" # significância apenas para liquidez
)

tabela_9 <- stargazer(
 roa7_tg, roa8_tg, roe7_tg, roe8_tg, type = 'text' ,title = "Painel Bancos S3-tg" # liquidez, CCoB e algo pra Tax
)

tabela_10 <- stargazer(
 roa9_tg, roa10_tg, roe9_tg, roe10_tg, title = "Painel Bancos S4 - tg" #liquidez, ccob, tax e etc
)
