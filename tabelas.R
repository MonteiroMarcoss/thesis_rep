
tabela_1 <- stargazer::stargazer(
 roa1, roa2,roe1, roe2, title = 'Painel completo'
)

tabela_2 <- stargazer(
 roa3, roa4, roe3, roe4, title = 'Painel Bancos S1'
)

tabela_3 <- stargazer::stargazer(
 roa5, roa6, roe5, roe6, title = "Painel Bancos S2"
)

tabela_s4 <- stargazer(
 roa7, roa8, roe7, roe8, title = "Painel Bancos S3"
)

tabela_5 <- stargazer(
 roa9, roa10, roe9, roe10, title = "Painel Bancos S4"
)
