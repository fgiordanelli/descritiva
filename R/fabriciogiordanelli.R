descritiva <- function(dados) {
  for (i in 1:ncol(dados)) {
    if (is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE)  {
      teste <- dados %>%
        dplyr::group_by(dados[,i]) %>%
        dplyr::summarise(qtd = n()) %>%
        dplyr::mutate(perc = 100*round(qtd/sum(qtd),4)) %>%
        dplyr::arrange(-perc) %>%
        dplyr::mutate(n_Count = row_number()) %>%
        dplyr::filter(n_Count <= 10) %>%
        dplyr::rename(col1 = 1)

      print(
        ggplot2::ggplot(teste, aes(x = col1, y = qtd, fill = col1)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::geom_text(aes(label = paste0("(",qtd,", ",perc,"%",")")), vjust = -1, size = 2.5) +
          ggplot2::scale_fill_brewer(palette="Paired") +
          ggplot2::labs(x = NULL,
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

        print(
          PerformanceAnalytics::chart.Correlation(dados[,c(i,j)], histogram=TRUE, pch=19
              )

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
          ggplot2::ggplot(dados, aes_string(x = nome1,y = nome2)) +
            ggplot2::geom_boxplot() +
            ggplot2::coord_flip() +
            ggplot2::labs(
              title = paste(colnames(dados[i]),colnames(dados[j]),sep = " x ")
            )
        )
      }
      if ((is.factor(dados[,i]) == TRUE &
           is.numeric(dados[,j]) == TRUE) |
          (is.character(dados[,i]) == TRUE &
           is.numeric(dados[,j]) == TRUE )) {


        print(
          knitr::kable(dados %>%
                         dplyr::group_by_at(colnames(dados[i])) %>%
                         dplyr::summarize_at(.vars = colnames(dados[j]),
                                      list(~ min(.,na.rm = TRUE),
                                           q1 = ~ quantile(.,
                                                           probs = c(0.25),
                                                           na.rm = TRUE),
                                           q3 = ~ quantile(.,
                                                           probs = c(0.75),
                                                           na.rm = TRUE),
                                           ~ max(.,na.rm = TRUE),
                                           ~ mean(., na.rm = TRUE),
                                           ~ sd(., na.rm = TRUE))),
                       format = "html",
                       digits = 2,
                       caption = paste(colnames(dados[i]),colnames(dados[j]),sep = " x ")) %>%
            kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
        )



        dados2= dados %>%
          dplyr::select(colnames(dados[j]),colnames(dados[i])) %>%
          dplyr::group_by_at(colnames(dados[i]))

        dados3= dados2 %>%
          dplyr::group_split() %>%
          setNames(unlist(group_keys(dados2)))


        dados4 <- do.call("cbind", dados3)

        dados5 <- dplyr::select_if(dados4, is.numeric)

        print(
          PerformanceAnalytics::chart.Correlation(dados5)
        )


      }

      if ((is.numeric(dados[,i]) == TRUE &
           is.factor(dados[,j]) == TRUE) |
          (is.numeric(dados[,i]) == TRUE &
           is.character(dados[,j]) == TRUE)) {


        print(
          knitr::kable(dados %>%
                         dplyr::group_by_at(colnames(dados[j])) %>%
                         dplyr::summarize_at(.vars = colnames(dados[i]),
                                      list(~ min(.,na.rm = TRUE),
                                           q1 = ~ quantile(.
                                                           ,probs = c(0.25),
                                                           na.rm = TRUE),
                                           q3 = ~ quantile(.,
                                                           probs = c(0.75),
                                                           na.rm = TRUE),
                                           ~ max(.,na.rm = TRUE),
                                           ~ mean(., na.rm = TRUE),
                                           ~ sd(., na.rm = TRUE))),
                       format = "html",digits = 2) %>%
            kableExtra::kable_styling()
        )


        dados2= dados %>%
          dplyr::select(colnames(dados[i]),colnames(dados[j])) %>%
          dplyr::group_by_at(colnames(dados[j]))

        dados3= dados2 %>%
          dplyr::group_split() %>%
          setNames(unlist(group_keys(dados2)))


        dados4 <- do.call("cbind", dados3)

        dados5 <- dplyr::select_if(dados4, is.numeric)

        print(
          chart.Correlation(dados5)
        )


      }

    }
  }

#  (min, max, mean, median, sd, q1 = quantile(.,probs = c(0.25)), q3 = quantile(., probs = 0.75)

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
          dplyr::filter(n_Count <= 10) %>%
          dplyr::rename(col1 = 1, col2 = 2)

        print(
          ggplot2::ggplot(teste, aes(x = col1,y = perc, fill = col2)) +
            ggplot2::geom_bar(stat = "identity", position = position_dodge(width = 1)) +
            ggplot2::geom_text(aes(label = paste0("(",qtd,", ",perc,"%",")")),position = position_dodge(width = 1), vjust = -1, size = 2) +
            ggplot2::scale_fill_brewer(palette="Paired") +
            ggplot2::labs(x = NULL,
                 y = "count & perc",
                 fill = colnames(dados[j]))


        )
      }
    }
  }

}
