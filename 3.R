library(dplyr)
library(readr)
T8.4 <- read_table('D:\\Documents\\Statistics\\Multi\\GP\\T8-4.DAT',
                    F)
S <- cov(T8.4)
e <- eigen(S)
e.values <- e$values
e.vectors <- e$vectors
CV <- diag(e.values)%*%t(e.vectors)
e.values %>%
  diag() %>%
  sqrt() %>%
  solve() %*%
  CV -> A
diag(S) %>%
  diag() %>%
  sqrt() %>%
  solve() -> B
corr <- A%*%B
corr <- round(corr,4)
corr <- as_tibble(corr)
write_csv(corr,
          'D:\\Documents\\Statistics\\Multi\\GP\\corr.csv',
          col_names = F)
