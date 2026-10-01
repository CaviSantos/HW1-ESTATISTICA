library(readr)
library(gt)
#lendo o arquivo csv

m <- 565324
r <- 1 + (m %% 100)

#lendo até o arquivo r, e armazenando em data_group
data_group <- read_csv("HW1_bike_sharing.csv", skip = r, n_max = 10, col_names = names(read_csv("HW1_bike_sharing.csv", n_max = 0)))
#transforma tudo em uma tabela minimalista
gt(data_group)

#criando uma tabela com o data set
tabela <- gt(data_group)
cat(as.character(as_latex(tabela)))

