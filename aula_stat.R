Dataset <- read.csv("dataset_mapfre.csv", encoding = "UTF-8", stringsAsFactors = TRUE)
read.csv
glimpse(Dataset)

# Estatística descritiva - Frequências

Dataset %>% 
  group_by(sex) %>% 
  count(country) %>% 
  mutate(porc = n/sum(n)*100) %>% 
  pander()

# usando a função tabyl() do pacote janitor

Dataset %>% 
  tabyl(country) %>%
  pander()

Dataset %>% 
  tabyl(country) %>% 
  adorn_totals() %>% 
  pander()

Dataset %>% 
  filter(!is.na(sex)) %>% 
  tabyl(country,sex) %>% 
  adorn_totals() %>% 
  pander()

Dataset %>% 
  filter(!is.na(sex)) %>% 
  tabyl(country,sex) %>% 
  adorn_totals(c("row","col")) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>% 
  adorn_ns() %>% 
  pander()

# Medidas de tendência central e de dispersão

# média
mean(Dataset$age, na.rm = TRUE) # a função na.rm() remove os NA's da variável

# desvio padrão
sd(Dataset$age, na.rm = TRUE)

# limites de um desvio em relação à média
mean(Dataset$age, na.rm = TRUE) + sd(Dataset$age, na.rm = TRUE)
mean(Dataset$age, na.rm = TRUE) - sd(Dataset$age, na.rm = TRUE)

# mediana
median(Dataset$age, na.rm = TRUE)

# mínimo
min(Dataset$age, na.rm = TRUE)

# máxima
max(Dataset$age, na.rm = TRUE)

# amplitude
range(Dataset$age, na.rm = TRUE)

# simetria
moments::skewness(Dataset$age, na.rm = TRUE)

# curtose
moments::kurtosis(Dataset$age, na.rm = TRUE)

hist(Dataset$age, breaks = 50)

data.frame(Estatísticas =       # data.frame() é a função para criar uma tabela ou planilha
             c("Média",         # eu quero três colunas: estatísticas, depressão e ansiedade
               "Desvio Padrão", # então, precisamos definir o que vai aparecer em cada coluna   
               "Mínimo",
               "Máximo",
               "Assimetria",
               "Curtose"),
           Depressão = 
             c(mean(Dataset$bdi_sum, na.rm = TRUE),
               sd(Dataset$bdi_sum, na.rm = TRUE),
               min(Dataset$bdi_sum, na.rm = TRUE),
               max(Dataset$bdi_sum, na.rm = TRUE),
               moments::skewness(Dataset$bdi_sum, na.rm = TRUE),
               moments::kurtosis(Dataset$bdi_sum, na.rm = TRUE)),
           Ansiedade = 
             c(mean(Dataset$bai_sum, na.rm = TRUE),
               sd(Dataset$bai_sum, na.rm = TRUE),
               min(Dataset$bai_sum, na.rm = TRUE),
               max(Dataset$bai_sum, na.rm = TRUE),
               moments::skewness(Dataset$bai_sum, na.rm = TRUE),
               moments::kurtosis(Dataset$bai_sum, na.rm = TRUE))) %>% 
  pander()

# Funções que sumariam dados

# describe() do pacote psych
install.packages("psych")
library(psych)

describe(Dataset$age, na.rm = TRUE) %>% pander()

# summary() da base do R
Dataset %>% select(age) %>% summary()

# tableby() do pacote arsenal
install.packages("arsenal")
library(arsenal)

tableby(country ~ bdi_sum + bai_sum,
        test = FALSE,
        data = Dataset) %>% 
  summary(text = TRUE)

names(Dataset)

# descritivas de depressão e ansiedade em função do sexo
tableby(sex ~ bdi_sum + bai_sum,
        test = FALSE,
        data = Dataset) %>% 
  summary(text = TRUE)

# descritiva de idade em função do sexo
tableby(sex ~ age + bdi_sum + bai_sum,
        test = FALSE,
        data = Dataset) %>% 
  summary(text = TRUE)


# só as descritivas para a amostra completa
tableby(~ age + bdi_sum + bai_sum,
        test = FALSE,
        data = Dataset) %>% 
  summary(text = TRUE)

# descrever as médias em função do sexo, estratificado por país

tableby(sex ~ age + bdi_sum + bai_sum,
        test = FALSE,
        data=Dataset, 
        strata = country) %>% 
  summary(text = TRUE)

# mesma tabela anterior mas invertendo as variáveis categóricas
tableby(country ~ age + bdi_sum + bai_sum,
        test = FALSE,
        data = Dataset, 
        strata = sex) %>% 
  summary(text = TRUE)

# interação entre as variáveis categóricas

tableby(interaction(sex,country) ~ bdi_sum + bai_sum,
        test = FALSE,
        data = Dataset) %>% 
  summary(text = TRUE)

# Análises de associação entre variáveis

# qui-quadrado

## gerar uma tabela de contingência

tabcont_sex_country <- table(Dataset$sex,Dataset$country)

options(scipen = 999)
chisq.test(tabcont_sex_country)

options(scipen = 0)

# calcula da correção do valor de p
0.05/(nrow(tabcont_sex_country)*ncol(tabcont_sex_country))
0.05/(2*3)

# tamanho do efeito (v de Cramer)
install.packages("rstatix")
library(rstatix)

cramer_v(tabcont_sex_country)

data.frame("Graus de Liberdade" = c(1,2,3),
           Pequeno = c(0.1,0.07,0.06),
           Médio = c(0.3,0.21,0.17),
           Grande = c(0.5,0.35,0.29)) %>% pander()

# representação gráfica

ggplot(Dataset, aes(x = country, fill = sex)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Proporção Sexo x País",
       x = "País",
       y = "Proporção",
       fill = "sex")
  

# Correlação

# correlação de Pearson (distribuição normal - estatística paramétrica)
# Correlação de Spearman (distribuição não-normal - estatística não-paramétrica)
# Correlação de Kendall (distribuição não-normal - estatística não-paramétrica)

## Primeiro passo - Verificação de normalidade (Shapiro-Wilk test)

names(Dataset)

options(scipen = 999)
options(scipen = 0)

shapiro.test(Dataset$bdi_sum)
hist(Dataset$bdi_sum)

shapiro.test(Dataset$bai_sum)
hist(Dataset$bai_sum)

cor.test(Dataset$bdi_sum,Dataset$bai_sum, method = "pearson") %>% pander()
cor.test(Dataset$bdi_sum,Dataset$bai_sum, method = "spearman") %>% pander
cor.test(Dataset$bdi_sum,Dataset$bai_sum, method = "kendall") %>% pander()

names(big_five)
round(cor(big_five[ ,59:63], method = "pearson"),digits = 2) %>% pander()

nrow(big_five)

matrix <- corr.test(big_five[1:30,59:63], method = "pearson")

matrix$r
matrix$p
matrix$n

matrix$stars %>% knitr::kable()

corr.test(big_five$extr,big_five$amab, method = "pearson")

corr.test(big_five[,59:63], big_five$idade, method = "pearson") %>% pander()

corr.test(Dataset$bdi_sum,Dataset$bai_sum, method = "pearson")

ggplot(dataset, aes(x=bdi_sum,y=bai_sum)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  labs(title = " Depressão x Ansiedade",
       x = "Depressão",
       y = "Ansiedade")

pairs.panels(big_five[ ,59:63], method = "pearson")

corrplot::corrplot(matrix$r,
          method = "shade",
          type = "lower",
          order = "hclust",
          adCoef.col = TRUE)

### ============================================================================
# gerar dois vetores N(0,1), independentes
z1 <- rnorm(n=100, mean = -1.2, sd = 0.54)
z2 <- rnorm(n=100, mean = -0.7, sd = 0.52)
z3 <- rnorm(n=100, mean = -0.1, sd = 0.57)
z4 <- rnorm(n=100, mean =  0.4, sd = 0.55)
z5 <- rnorm(n=100, mean =  0.5, sd = 0.54)

rho = -0.8  # o coeficiente de correlacao

e1 = z1
e2 = rho*z1+sqrt(1-rho^2)*z2

cor(e1,e2)  # aproximadamente -0.8
### ============================================================================

# Diferenças entre grupos

# T-test: para uma amostra, amostras independentes, amostras pareadas

# Análises pressupostos

## Análise de normalidade (Shapiro-Wilk)

glimpse(big_five_BR)

shapiro.test(big_five_BR$neur)
hist(big_five_BR$neur)

# assimetria e curtose entre -0,5 e 0,5
skew(big_five_BR$neur, na.rm = TRUE)
kurtosi(big_five_BR$neur, na.rm = TRUE)

count(big_five,país)

média_neur <- big_five %>% 
  filter(!país == "BR") %>% 
  select("neur")

mean(média_neur$neur, na.rm = TRUE)

# t-test para uma amostra

t.test(big_five_BR$neur,mu = 3.094893)

mean(big_five_BR$neur, na.rm = TRUE)
sd(big_five_BR$neur, na.rm = TRUE)

big_five %>% mutate(Brasil = ifelse(país == "BR","BR","OU")) %>% 
  ggplot(aes(x = Brasil, y = neur)) +
  geom_boxplot(color = "black", fill = "#74c69d") +
  labs(title = "Média de Neuroticismo: Brasil x Mundo",
       x = "País",
       y = "Neuroticismo")

# t-teste para amostras independentes
# primeiro passo: trasnformar a categoria "outros" em NA
big_five_BR$gênero[big_five_BR$gênero == "Outro"] <- NA

count(big_five_BR,gênero)

# variâncias podem ser assumidas como iguais
car::leveneTest(neur ~ gênero, big_five_BR, center=mean)
# conclusão: as variâncias de homens e mulheres em neur podem ser assumidas como iguais

t.test(neur ~ gênero, big_five_BR, var.equal = TRUE)

t_extr <- t.test(extr ~ gênero, big_five_BR, var.equal = TRUE)
t_neur <- t.test(neur ~ gênero, big_five_BR, var.equal = TRUE)
t_amab <- t.test(amab ~ gênero, big_five_BR, var.equal = TRUE)
t_cons <- t.test(cons ~ gênero, big_five_BR, var.equal = TRUE)
t_aber <- t.test(aber ~ gênero, big_five_BR, var.equal = TRUE)

data.frame(Variável = c("Extroversão","Neuroticismo","Amabilidade",
                        "Conscienciosidade","Abertura"),
           t = c(round(t_extr$statistic,digits = 3),
                 round(t_neur$statistic,digits = 3),
                 round(t_amab$statistic,digits = 3),
                 round(t_cons$statistic,digits = 3),
                 round(t_aber$statistic,digits = 3)),
           gl = c(t_extr$parameter,
                  t_neur$parameter,
                  t_amab$parameter,
                  t_cons$parameter,
                  t_aber$parameter),
           p = c(round(t_extr$p.value,digits = 3),
                 round(t_neur$p.value,digits = 3),
                 round(t_amab$p.value,digits = 3),
                 round(t_cons$p.value,digits = 3),
                 round(t_aber$p.value,digits = 3))) %>% pander::pander()

arsenal::tableby(gênero ~ extr + neur + amab + cons + aber,
                data = big_five_BR,
                test = FALSE) %>% 
  summary(text = TRUE)

# t-test para amostras pareadas

big_five_BR %>% select(gênero,neur)

t.test(df$pos_teste,df$pre_teste, paired = TRUE)

# ANOVA de uma via
# Verficiar o efeito da escolaridade (education) no nível de autoritarismo (auth).

glimpse(rwas)
count(rwas,education)

# Construção do modelo
anova_auth <- aov(auth ~ education,rwas)
summary(anova_auth)

one_way_anova_tab <- arsenal::tableby(education ~ auth, test=FALSE,rwas)
summary(one_way_anova_tab, text=TRUE)

# ANOVA por medidas repetidas

glimpse(df_aov_mr)
view(df_aov_mr)

# transformar o banco de dados do formato amplo (wide) para o formato longo (long)
# usar a função melt() do pacote reshape

df_aov_mr1 <- reshape::melt(df_aov_mr,
              id = "id",
              measured = c("v1","v2","v3","v4","v5"))

# reordenar as colunas pelas variáveis
df_aov_mr1 <- reshape::sort_df(df_aov_mr1,vars = "id")

glimpse(df_aov_mr1)

df_aov_mr1$id <- factor(df_aov_mr1$id)


# ANOVA por medidas repetidas

## pacote ez, função ezANOVA

mod_df_aov_mr1 <- ez::ezANOVA(data = df_aov_mr1, # banco de dados no formato longo
                              dv = value,        # variável dependente
                              wid = id,          # variável que identifica os sujeitos
                              within = variable, # variável intra-sujeitos
                              detailed = TRUE,   # saída mais detalhada
                              type = 3)          # método empregado para a comparação das variâncias
                                                 # tipo 3 é por soma dos quadrados das diferenças 

mod_df_aov_mr1

# visualização dos dados em uma tabela

table_AOV_medrep <- 
  data.frame(variáveis = c('v1','v2','v3','v4','v5'),
             médias = c(round(mean(df_aov_mr$v1), digits = 2),
                        round(mean(df_aov_mr$v2), digits = 2),
                        round(mean(df_aov_mr$v3), digits = 2),
                        round(mean(df_aov_mr$v4), digits = 2),
                        round(mean(df_aov_mr$v5), digits = 2)),
             devpad = c(round(sd(df_aov_mr$v1), digits = 2),
                        round(sd(df_aov_mr$v2), digits = 2),
                        round(sd(df_aov_mr$v3), digits = 2),
                        round(sd(df_aov_mr$v4), digits = 2),
                        round(sd(df_aov_mr$v5), digits = 2)))

table_AOV_medrep %>% pander::pander()  

# Representação gráfica

ggplot(table_AOV_medrep, aes(x = variáveis, y = médias)) +
  geom_point(color = "#9e2a2b",size = 4) +
  geom_line(group = 1, linetype = "dashed") + 
  labs(title = "ANOVA POR MEDIDAS REPETIDAS",
       x = "Medidas repetidas",
       y = "Médias")
