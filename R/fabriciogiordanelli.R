descritiva <- function(dados) {
  for (i in 1:ncol(dados)) {
    if (is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE)  {
      teste <- dados %>%
        dplyr::group_by(dados[,i]) %>%
        dplyr::summarise(qtd = n()) %>%
        dplyr::mutate(perc = 100*round(qtd/sum(qtd),4)) %>%
        dplyr::arrange(-perc) %>%
        dplyr::mutate(n_Count = row_number()) %>%
        filter(n_Count <= 10) %>%
        dplyr::rename(col1 = 1)

      print(
        ggplot(teste, aes(x = col1, y = qtd, fill = col1)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = paste0("(",qtd,", ",perc,"%",")")), vjust = -1, size = 2.5) +
          scale_fill_brewer(palette="Paired") +
          labs(x = NULL,
               y = "count & perc",
               fill = colnames(dados[i]))
      )

    }
  }



  # dados2 <- dados %>%
  #   keep(is.numeric)
  #
  # res.pca <- prcomp(dados2, scale = TRUE)
  #
  # fviz_pca_var(res.pca,
  #              col.var = "contrib", # Color by contributions to the PC
  #              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  #              repel = TRUE     # Avoid text overlapping
  #              )




  for (i in 1:ncol(dados)-1) {
    for (j in (1+i):ncol(dados)) {
      if (is.numeric(dados[,i]) == TRUE & is.numeric(dados[,j]) == TRUE )  {

        nome1 <- colnames(dados[i])
        nome2 <- colnames(dados[j])

        #print(
        # ggplot(dados, aes_string(x = nome1,y = nome2)) +
        #  geom_point()
        #)
        print(chart.Correlation(dados[,c(i,j)], histogram=TRUE, pch=19)
        )
      }
    }
  }



  for (i in 1:ncol(dados)-1) {
    for (j in (1+i):ncol(dados)) {
      if (((is.factor(dados[,i]) == TRUE &
            is.numeric(dados[,j]) == TRUE) |
           (is.character(dados[,i]) == TRUE &
            is.numeric(dados[,j]) == TRUE ))|
          ((is.numeric(dados[,i]) == TRUE &
            is.factor(dados[,j]) == TRUE) |
           (is.numeric(dados[,i]) == TRUE &
            is.character(dados[,j]) == TRUE )))  {

        nome1 <- colnames(dados[i])
        nome2 <- colnames(dados[j])

        print(
          ggplot(dados, aes_string(x = nome1,y = nome2)) +
            geom_boxplot() +
            coord_flip()
        )
      }
      if ((is.factor(dados[,i]) == TRUE &
           is.numeric(dados[,j]) == TRUE) |
          (is.character(dados[,i]) == TRUE &
           is.numeric(dados[,j]) == TRUE )) {


        print(
          kable(dados %>%
                  group_by_at(colnames(dados[i])) %>%
                  summarize_at(.vars = colnames(dados[j]), funs(min, max, mean, median, sd, q1 = quantile(.,probs = c(0.25)), q3 = quantile(., probs = 0.75)),na.rm = TRUE)) %>%
            kable_styling()
        )
      }

      if ((is.numeric(dados[,i]) == TRUE &
           is.factor(dados[,j]) == TRUE) |
          (is.numeric(dados[,i]) == TRUE &
           is.character(dados[,j]) == TRUE)) {


        print(
          kable(dados %>%
                  group_by_at(colnames(dados[j])) %>%
                  summarize_at(.vars = colnames(dados[i]), funs(min, max, mean, median, sd, q1 = quantile(.,probs = c(0.25)), q3 = quantile(., probs = 0.75)))) %>%
            kable_styling()
        )
      }

    }
  }



  for (i in 1:ncol(dados)-1) {
    for (j in (1+i):ncol(dados)) {
      if (((is.factor(dados[,i]) == TRUE &
            is.factor(dados[,j]) == TRUE) |
           (is.factor(dados[,i]) == TRUE &
            is.character(dados[,j]) == TRUE)) |
          ((is.character(dados[,i]) == TRUE &
            is.factor(dados[,j]) == TRUE) |
           (is.character(dados[,i]) == TRUE &
            is.character(dados[,j]) == TRUE))) {

        teste <- dados %>%
          dplyr::group_by(dados[,i],dados[,j]) %>%
          dplyr::summarise(qtd = n()) %>%
          dplyr::mutate(perc = 100*round(qtd/sum(qtd),4)) %>%
          dplyr::mutate(n_Count = row_number()) %>%
          filter(n_Count <= 10) %>%
          dplyr::rename(col1 = 1, col2 = 2)

        print(
          ggplot(teste, aes(x = col1,y = perc, fill = col2)) +
            geom_bar(stat = "identity", position = position_dodge(width = 1)) +
            geom_text(aes(label = paste0("(",qtd,", ",perc,"%",")")),position = position_dodge(width = 1), vjust = -1, size = 2) +
            scale_fill_brewer(palette="Paired") +
            labs(x = NULL,
                 y = "count & perc",
                 fill = colnames(dados[j]))


        )
      }
    }
  }

}





