

library(tidyverse)
library(resample) # para bootstrap
library(lubridate)
library(here)
#library(magrittr)
library(xts)

onerun = function(dado, n) {
  experiment = sample(dado$sucesso, n, replace=TRUE)
  b = bootstrap(experiment, mean, R = 2000)
  ci.from.bca = CI.bca(b, probs = c(.025, .975))
  ci.from.percentile = CI.percentile(b, probs = c(.025, .975))
  return(tibble(type = c("bca", "percentile"),
                lower = c(ci.from.bca[1], ci.from.percentile[1]), 
                upper = c(ci.from.bca[2], ci.from.percentile[2])))
}
cobertura = function(dado, sample_size, experiments = 2000){
  cis = tibble(dado) %>% 
    group_by(honestidade, tem_validador) %>% 
    do(onerun(dado, sample_size))
}

#grandes cluster tem seu maior impacto a 40% de honestidade, ou seja, 60% dos agentes se envolvem em clusters e assim, a maioria das MGIs são selecinadas por estes agentes maliciosos. Estas MGIs admitem clusaters
#à 60% de honestidade (40% dos agentes envolvendo-se em cluster) o resultado é um certo prejuízo para a rede, mas bem menos pois a maior parte dad população gera ciclos reais e promove MGIs que identificam clusters
#quando a honestidade é muito baixa, a maioria dos agentes se envolvem em clusters e a maioria das MGIs admite clusters. Contudo, cada cluster designará suas MGIs e a diversidade de MGIs será alta, reduzindo o valor de promover sua reputação em cada uma delas, levando os agentes a escolher MGIs mais gerais. MGIs mais gerais são escolhidas por todos e tendem a não aceitar conluios. Embora tais também não avaliem os agentes com tanta justiça quanto um ambiente mais diverso de MGIs, ainda são suficientes para estabilizar a rede de transações
#grandes clusters devem ser os mais perigosos (devem impactar bem situações com pouca honestidade, mas assim como as demais, não atrapalha muito a rede acima e 50%) pois algumas MGIs generalistas ainda caem na armadilha
#relembre que as demais soluções estão neste gráfico apenas para comparação, mas HS está lidando com um modelo muito mais sofisticado de desonestidade, o conluio. A comparação em si não é totalmente justa em nenhum cenário, pois a desonestidade unilateral é impossível na hs, uma vez que a vantagem do agente passivo sobre uma dada transação só vem após a confirmação de seu sucesso.
#considere piorar os resultados das demais para incluir conluio, que no caso corresponde a favorecer seu grupo de conluio propagando depoimentos falsos a favor da honestidade de agentes desonestos do seu grupo
#os grandes clusters de conluio levem os agentes a não ter escolha a não ser aderir a eles, sendo que nos pequenos clusters também tem conluio. No entanto, ainda sim, com a maior parte da população envolvida em conluios, sobra uma pequena parcela da população para ser lesada pelos grandes closters de conluio, levando a uma melhora no resultado quando a população desoneste cresce muito
#CENÁRIO COM GRANDES E PEQUERNOS CLUSTERS DE CONLUIO
#moral da história, quando todos são desonestos a não colaboração entre eles os isola, obrigando-os a agir honestamente para continuar negociando
ecomony <-
  list.files(path = "../economy_simulation/output-cluster2/", pattern = "data*.csv", full.names = TRUE) %>% 
  lapply(function(x) read_csv(x)) %>%                              # Store all files in list
  bind_rows 

colnames(ecomony) <- c("sucesso","memoria","tem_validador","typeAgnostic","securityDeposit","hasFeedback",
                       "feedbackPercent","honestidade","stepCount","totalTransactions","transactionFail",
                       "AvoidedFailTransactions","uniqueValidators","terminate")

as.data.frame(ecomony)

ecomony %>% 
  mutate(profile = case_when(
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "A",
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ "B",
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ "C",
    tem_validador == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ "D",
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ "E",
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ "F",
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ "G",
    tem_validador == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ "H",
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "I",
  ) ,
  profile_count = case_when(
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ 1.4,
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ 1.2,
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ 1.0,
    tem_validador == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ 0.8,
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ 0.6,
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ 0.4,
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ 0.2,
    tem_validador == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ 0.0,
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ -0.2,
  ) 
  )%>% 
  group_by(profile,honestidade) %>% 
  summarise(med = mean(sucesso),total = mean(stepCount))

#ecomony = read_csv("../Traders/data.csv",
#                   col_names = TRUE, col_types = cols(honestidade = col_number(),
#                                                      sucesso = col_integer(),
#                                                      memoria = col_integer(),
#                                                      tem_validador = col_character(),
#                                                      validador_n_eh_conhecido = col_integer()))
#
# Remove jobs idênticos no mesmo build
ecomony_2 = ecomony %>% 
  mutate(profile = case_when(
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ " HS",
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "A",
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ "B",
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ "C",
    tem_validador == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ "D",
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ "E",
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ "F",
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ "G",
    tem_validador == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ "H",
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "I",
  ) ,
  profile_count = case_when(
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ 1.6,
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ 1.4,
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ 1.4,
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ 1.2,
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 0 ~ 1.0,
    tem_validador == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 0 ~ 0.8,
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ 0.6,
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ 0.4,
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 1 & hasFeedback == 1 ~ 0.2,
    tem_validador == 0 & typeAgnostic == 1 & securityDeposit == 1 & hasFeedback == 1 ~ 0.0,
    tem_validador == 0 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ -0.2,
  ) 
  )%>% 
  filter(honestidade <= 0.9) %>%
  group_by(honestidade, profile,profile_count) %>% 
  dplyr::summarise(acertou = (sum(sucesso)/n()),profile_count = first(profile_count), total = n())  %>%
  mutate(acertou_linear = na.approx(acertou))

ecomony_3 = ecomony %>% 
  mutate(profile = case_when(
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ "A",
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ "B",
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ "E",
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ "F",
  ) ,
  profile_count = case_when(
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 0 ~ 1.35,
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 0 ~ 0.6,
    tem_validador == 1 & typeAgnostic == 0 & securityDeposit == 0 & hasFeedback == 1 ~ 0.6,
    tem_validador == 1 & typeAgnostic == 1 & securityDeposit == 0 & hasFeedback == 1 ~ -0.35,
  ) 
  )%>% 
  filter(honestidade <= 0.9, !is.na(profile)) %>%
  group_by(honestidade, profile, profile_count) %>% 
  dplyr::summarise(uniq = (mean(uniqueValidators)),total = n()) %>%
  mutate(uniq_linear = na.approx(uniq))

ecomony_3
ggplot(data=ecomony_3, aes(y=uniq_linear,x=honestidade*100,ymax = if_else(uniq==100,uniq,uniq*0.95), ymin = if_else(uniq*1.05+0.05<100,if_else(uniq==0,0,uniq*1.05+0.05),100), fill=profile))  +
  geom_bar(stat="identity", position=position_dodge(),color="white")+
  geom_text(aes(label=format(round(uniq, 1), nsmall = 1),hjust=profile_count    ), vjust=-0.2, color="black", size=3.5)+
  geom_errorbar(position=position_dodge(8.6))  +
  labs(x='Honesty rate (%)',  
       y="Diversity of arbitrators (%)", 
       #title="Comparativo de Correlação entre Honestidade e Sucesso ", 
       #subtitle="(Sucesso da População, Taxa de Honestidade na População)",
       fill="Solutions") +
  theme_minimal()+
  theme(plot.title = element_text(face="bold",size = "15"),
        plot.subtitle = element_text(size = "10"),
        plot.caption = element_text(size="10"),
        axis.title.y = element_text(size="12"),
        axis.text.x = element_text(size="10"),
        axis.text.y = element_text(size="12"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        #panel.background = element_blank(),
        legend.position = "bottom",
        panel.border=element_blank())+
  scale_x_continuous(breaks = seq(10, 90, by = 10),limits=c(0, 100)) +
  scale_fill_grey()


ecomony_2
ggplot(data=ecomony_2, aes(y=acertou_linear*100,x=honestidade*100,ymax = if_else(acertou==1,acertou*100,acertou*97.5), ymin = if_else(acertou*102.5+2.5<100,if_else(acertou==0,0,acertou*102.5+2.5),100),  fill =profile))  +
  geom_bar(stat="identity", position=position_dodge(),color="white")+
  #geom_smooth(method = loess, span = 0.01, se = FALSE, size=0.8)+
  #geom_line(size=0.8)+
  #geom_area(aes(fill = profile, group = profile), alpha = 0.5, position = 'identity')
  geom_text(aes(label=if_else(profile==" HS" | profile=="A",format(round(acertou*100, 1), nsmall = 1),"")  ),  vjust=-0.1, color="black", size=3.5)+
  geom_errorbar(position=position_dodge(8.6))  +
  labs(x='Taxa de honestidade (%)',  
       y="Sucesso da população (%)", 
       #title="Comparativo de Correlação entre Honestidade e Sucesso ", 
       #subtitle="(Sucesso da População, Taxa de Honestidade na População)",
       fill="Soluções") +
  theme_minimal()+
  theme(plot.title = element_text(face="bold",size = "15"),
        plot.subtitle = element_text(size = "10"),
        plot.caption = element_text(size="10"),
        axis.title.y = element_text(size="12"),
        axis.text.x = element_text(size="10"),
        axis.text.y = element_text(size="12"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        panel.border=element_blank())+
  scale_x_continuous(breaks = seq(10, 90, by = 10),limits=c(0, 100)) +
  scale_fill_manual(values=c(alpha("#0000FF",1.0), 
                             alpha("#00FF00",1.0), 
                             alpha("#DDDDDD",1.0), 
                             alpha("#BBBBBB",1.0),
                             alpha("#999999",1.0),
                             alpha("#777777",1.0),
                             alpha("#555555",1.0),
                             alpha("#333333",1.0),
                             alpha("#111111",1.0),
                             alpha("#FF2233",1.0)))
