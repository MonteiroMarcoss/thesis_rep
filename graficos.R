library(tidyverse)
library(viridis)
library(hrbrthemes)
library(fmsb)

# tabela imapp ------------------------------------------------------------

imapp <- read_csv('Brazil_imap.csv') |> select(-Country, -ifscode, -iso3, -iso2, -AE, -EMDE, 
                                               DATA) |>
 filter(DATA >'2003-12-01')

imapp_graf1 <- imapp |>
 transmute(CCB = CCB,
           CCoB = Conservation,
           CT =  Capital,
           LVR = LVR,
           LLP = LLP,
           LCG = LCG, # + LCG_Gen + LCG_HH + LCG_Corp,
           LR = LoanR, # + LoanR_HH + LoanR_Corp,
           LFC = LFC,
           LTV = LTV,
           DSTI = DSTI,
           TAX = Tax,
           LIQ = Liquidity,
           LTD = LTD,
           LFX = LFX,
           RR = RR, #+ RR_FCD,
           SIFI = SIFI,
           OT = OT)


# gráfico de barra --------------------------------------------------------

soma_colunas_tightening <- map_dbl(imapp_graf1, ~sum(.x == 1))

soma_colunas_loosening <- map_dbl(imapp_graf1, ~sum(.x == -1))

soma_colunas_abs <- colSums(abs(imapp_graf1))

df_sum <- tibble(MP = names(soma_colunas_abs),
                 Restringir = soma_colunas_tightening,
                 Afrouxar = soma_colunas_loosening)

df_sum_longer <- df_sum |>
 pivot_longer(cols = c(Restringir, Afrouxar),
              names_to = "Objetivo",
              values_to = "valor")

x11();ggplot(df_sum_longer, aes(fill=Objetivo, y=valor, x=MP)) + 
 geom_bar(position="stack", stat="identity") + 
 scale_fill_manual(values = c('#003366', '#FF3333'))+
 ggplot2::ylab('Soma dos instrumentos implementados')+
 ggplot2::xlab('Políticas Macroprudenciais') +
#ggtitle("Instrumentos macroprudenciais implementados no Brasil de 2004 a 2021") +
 coord_flip()+
 theme_minimal() 

# gráfico no tempo ------------------------------------------

imapp <- read_csv('Brazil_imap.csv') |> 
 select(-Country,
        -ifscode,
        -iso3,
        -iso2,
        -AE,
        -EMDE,
        DATA) |>
 filter(DATA >'2003-12-01')

imapp_mp <- imapp |>
 transmute(DATA = DATA,
           CCB = CCB,
           CCoB = Conservation,
           CT =  Capital,
           LVR = LVR,
           LLP = LLP,
           LCG = LCG, # + LCG_Gen + LCG_HH + LCG_Corp,
           LR = LoanR, # + LoanR_HH + LoanR_Corp,
           LFC = LFC,
           LTV = LTV,
           DSTI = DSTI,
           TAX = Tax,
           LIQ = Liquidity,
           LTD = LTD,
           LFX = LFX,
           RR = RR, #+ RR_FCD,
           SIFI = SIFI,
           OT = OT)

imapp_mp <- imapp_mp |>
 mutate(Ano = format(as.Date(imapp_mp$DATA, format = "%Y-%m-%d"), "%Y"))
imapp_mp <- imapp_mp |> select(-DATA)

imapp_mp <- imapp_mp |>
 group_by(Ano) |> summarise(across(everything(), ~ sum(abs(.), na.rm = TRUE)))


# LINHA DO TEMPO ----------------------------------------------------------


df_pivotado <- imapp_mp |>
 pivot_longer(cols = -Ano,
              names_to = 'PM',
              values_to = 'Valor'
              )
df_pivotado <- df_pivotado |>
 filter(PM == 'CCoB'|
         PM == 'CT'|
         PM == 'LIQ'|
         PM == 'RR'|
         PM == 'RR_FCD'|
         PM == 'TAX')

x11();ggplot(df_pivotado, aes(fill = PM,
                        x = Ano,
                        y = Valor)) + 
 geom_bar(position="fill", stat="identity") +
 ylab('Proporção de instrumentos macroprudenciais implementados')+
 guides(x = guide_axis(angle = 45)) +
 theme_minimal()
 