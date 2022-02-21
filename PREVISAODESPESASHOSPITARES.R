# MACHINE LEARNING - APRENDIZAGEM SUPERVISIONADA COM INPUTS E OUTPUTS 

# ESCOLHA DIRETÓRIO DE TRABALHO
setwd(choose.dir())
getwd()

# DEFININDO PROBLEMA DE NEGÓCIO.
# PROBLEMA: PREVISÃO DE DESPESAS HOSPITALARES.
# STAKEHOLDERS: HOSPITAIS, EMPRESAS DE PLANO DE SAÚDE,ETC.

# CONJUNTO DE DADOS HIPOTÉTICOS.
# 1338 OBSERVAÇÕES E 7 VARIÁVEIS.

# ETAPA 1 - CARREGANDO OS DADOS
despesas <- read.csv('despesas.csv')
View(despesas)

# ETAPA 2 - ANÁLISE EXPLORATÓRIA E PREPARAÇÃO DOS DADOS.

# VISUALIZAÇÃO DOS DADOS.
str(despesas)

# MEDIDAS DE TENDÊNCIA CENTRAL DA VAR. GASTOS.
summary(despesas$gastos)

# CONSTRUÇÃO DE HISTOGRAMA PARA FACILITAR ENTENDIMENTO UNIVARIADO.
hist(despesas$gastos, main = 'HISTOGRAMA', xlab = 'Gastos')

# TABELA DE CONTINGÊNCIA PARA REGIÕES
table(despesas$regiao)

# COEFICIENTE DE CORRELAÇÃO ENTRE AS VARIÁVEIS NUMÉRICAS - MATRIZ DE CORRELAÇÃO
# SENDO QUE:
# 1 = ALTA CORRELAÇÃO ENTRE AS VARIÁVEIS
# 0 = SEM CORRELAÇÃO
# -1 = CORRELAÇÃO NEGATIVA 

# VERIFICADO QUE COM HÁ CORRELAÇÃO BAIXA ENTRE AS VARIAVEIS BMI(IMC) COM A IDADE
# ISTO SIGNIFICA QUE COM O AUMENTO DA IDADE HÁ AUMENTO DE IMC.
# CORRELAÇÃO MEDIANA ENTRE AS VARIÁVEIS FILHOS/GASTOS E IDADE/GASTOS SENDO QUE
# COM O AUMENTO DA PRIMEIRA VÁRIÁVEL HÁ AUMENTO DOS GASTOS.
cor(despesas[c('idade','bmi','filhos','gastos')])


# VISUALIZANDO AS CORRELAÇÕES ATRAVÉS DE GRÁFICOS SCATTERPLOT (DISPERSÃO)
pairs(despesas[c('idade','bmi','filhos','gastos')])

# PARA MAIS INFORMAÇÕES 

install.packages('psych')
library(psych)

pairs.panels(despesas[c('idade','bmi','filhos','gastos')])

# ETAPA 3: TREINANDO O MODELO

modelo <- lm(gastos ~ idade + filhos + bmi + sexo + fumante + regiao, data = despesas)

# OUTRA FORMA MAIS COMPACTA

modelo <- lm(gastos ~ .,data = despesas)

# VISUALIZANDO OS COEFICIENTES

modelo

# PREVENDO DESPESAS MÉDICAS DOS DADOS DE TREINO

previsao1 <- predict(modelo)
View(previsao1)

# PREVENDO COM OS DADOS DE TESTE
# DADOS DA PLANILHA TESTE NÃO CONTÈM INFORMAÇÕES SOBRE OS GASTOS, PORÉM APENAS 
# REGISTROS SOBRE IDADE, BMI, FUMANTE, FILHOS

despesasteste <- read.csv('despesas-teste.csv')

# ETAPA 4 AVALIANDO A PERFORMANCE DO MODELO 
summary(modelo)

previsao2 <- predict(modelo, despesasteste)
View(previsao2)

# ETAPA 5 - OTIMIZAÇÃO DA PERFOMANCE DO MODELO

# ADICIONANDO O DOBRO DA VARIAVEL IDADE
despesas$idade2 <- despesas$idade ^ 2

# ADICIONANDO INDICADOR BMI >= 30
despesas$bmi30 <- ifelse(despesas$bmi >= 30, 1,0)
View(despesas)

# SEGUNDA VERSAO DO MODELO

modelo_v2 <- lm(gastos ~ idade + idade2 + filhos + bmi + sexo +
                  bmi30 * fumante + regiao, data = despesas)

summary(modelo_v2)