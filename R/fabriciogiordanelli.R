descritiva <- function(dados,soma) {
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
          ggplot2::geom_text(aes(label = paste0("(",qtd,", ",perc,"%",")")), size = 2.5, vjust = -1) +
          scale_fill_brewer(palette="Paired") +
          ggplot2::labs(x = NULL,
                        y = "count & perc",
                        fill = colnames(dados[i]),
                        title = paste("Quantidade e Percentual de", colnames(dados[i])))
      )

    }
  }



  for (i in 1:ncol(dados)-1) {
    for (j in (1+i):ncol(dados)) {
      if ((is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE) &
          (is.factor(dados[,j]) == TRUE | is.character(dados[,j]) == TRUE)) {

        teste <- dados %>%
          dplyr::group_by(dados[,i],dados[,j]) %>%
          dplyr::summarise(qtd = n()) %>%
          dplyr::mutate(perc = 100*round(qtd/sum(qtd),4)) %>%
          dplyr::mutate(n_Count = row_number()) %>%
          dplyr::filter(n_Count <= 10) %>%
          dplyr::rename(col1 = 1, col2 = 2)

        print(
          ggplot2::ggplot(teste, aes(x = col1,y = qtd, fill = col2)) +
            ggplot2::geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
            ggplot2::geom_text(aes(label = paste0("(",qtd,", ",perc,"%",")")),position = position_dodge(width = 0.9), vjust = -1, size = 2) +
            scale_fill_brewer(palette="Paired") +
            ggplot2::labs(x = NULL,
                          y = "count & perc",
                          fill = colnames(dados[j]),
                          title = paste("Quantidade e Percentual de", colnames(dados[i])," por ",colnames(dados[j])))


        )
      }
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



  for (i in 1:ncol(dados)) {
    if (is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE) {
      for (j in 1:ncol(dados)) {
        if (is.numeric(dados[,j]) == TRUE) {

          nome1 <- colnames(dados[i])
          nome2 <- colnames(dados[j])

          if(!missing(soma)) {
            teste <- dados %>%
              dplyr::group_by_at(nome1) %>%
              dplyr::summarize_at(.vars = nome2,
                                  list(soma = ~sum(.,na.rm = TRUE))) %>%
              dplyr::mutate(perc = 100*round(soma/sum(soma),4)) %>%
              dplyr::arrange(-perc) %>%
              dplyr::mutate(n_Count = row_number()) %>%
              dplyr::filter(n_Count <= 10) %>%
              dplyr::rename(col1 = 1)

            print(
              ggplot2::ggplot(teste, aes(x = col1, y = soma, fill = col1)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::geom_text(aes(label = paste0("(",round(soma,0),", ",perc,"%",")")), size = 2.5, vjust = -1) +
                scale_fill_brewer(palette="Paired") +
                ggplot2::labs(x = NULL,
                              y = "count & perc",
                              fill = colnames(dados[i]),
                              title = paste("Soma total de ", colnames(dados[j])))
            )
          }
          else {

            teste <- dados %>%
              dplyr::group_by_at(nome1) %>%
              dplyr::summarize_at(.vars = nome2,
                                  list(media = ~mean(.,na.rm = TRUE))) %>%
              dplyr::mutate(perc = 100*round(media/sum(media),4)) %>%
              dplyr::arrange(-perc) %>%
              dplyr::mutate(n_Count = row_number()) %>%
              dplyr::filter(n_Count <= 10) %>%
              dplyr::rename(col1 = 1)

            print(
              ggplot2::ggplot(teste, aes(x = col1, y = media, fill = col1)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::geom_text(aes(label = paste0("(",round(media,2),", ",perc,"%",")")), size = 2.5, vjust = -1) +
                scale_fill_brewer(palette="Paired") +
                ggplot2::labs(x = NULL,
                              y = "count & perc",
                              fill = colnames(dados[i]),
                              title = paste("Média total de ", colnames(dados[j])))
            )


          }






          print(
            ggplot2::ggplot(dados, aes_string(x = nome1,y = nome2)) +
              ggplot2::geom_boxplot() +
              ggplot2::labs(
                title = paste(colnames(dados[i]),colnames(dados[j]),sep = " x ")
              )
          )


          print(
            knitr::kable(dados %>%
                           dplyr::group_by_at(colnames(dados[i])) %>%
                           dplyr::summarize_at(.vars = colnames(dados[j]),
                                               list(n = ~ n(),
                                                    ~ min(.,na.rm = TRUE),
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

        }
      }
    }
  }


  for (i in 1:ncol(dados)) {
    if (is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE) {
      for (j in 1:ncol(dados)) {
        if (is.numeric(dados[,j]) == TRUE) {
          tryCatch({
            dados2 <- dados %>%
              dplyr::select(colnames(dados[i]),colnames(dados[j])) %>%
              dplyr::group_by_at(colnames(dados[i]))

            dados3 <- dados2 %>%
              dplyr::group_split() %>%
              setNames(unlist(group_keys(dados2)))



            dados4 <- do.call("cbind", dados3)

            dados5 <- dplyr::select_if(dados4, is.numeric)

            print(
              PerformanceAnalytics::chart.Correlation(dados5)
            )
            stop("teste")} ,error = function(e){})

        }
      }
    }
  }



  for (i in 1:ncol(dados)-1) {
    for (j in (1+i):ncol(dados)) {
      for (k in 1:ncol(dados)) {
        if ((is.factor(dados[,i]) == TRUE | is.character(dados[,i]) == TRUE) &
            (is.factor(dados[,j]) == TRUE | is.character(dados[,j]) == TRUE) &
            is.numeric(dados[,k]) == TRUE ){
          {


            tryCatch({

              nome1 <- colnames(dados[i])
              nome2 <- colnames(dados[j])
              nome3 <- colnames(dados[k])

              if (!missing(soma)){

                teste <- oi %>%
                  dplyr::group_by_at(vars(all_of(nome1),all_of(nome2))) %>%
                  dplyr::summarize_at(.vars = nome3,
                                      list(soma = ~sum(.,na.rm = TRUE))) %>%
                  dplyr::mutate(perc = 100*round(soma/sum(soma),4)) %>%
                  dplyr::rename(col1 = 1, col2 = 2)

                print(
                  ggplot2::ggplot(teste, aes(x = col1, y = soma, fill = col2)) +
                    ggplot2::geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
                    ggplot2::geom_text(aes(label = paste0("(",round(soma,0),", ",perc,"%",")")),position = position_dodge(width = 0.9), vjust = -1, size = 2) +
                    scale_fill_brewer(palette="Paired") +
                    ggplot2::labs(x = NULL,
                                  y = "count & perc",
                                  fill = colnames(dados[i]),
                                  title = paste("Soma total de tarifa"))
                )

              }
              else {

                teste <- oi %>%
                  dplyr::group_by_at(vars(all_of(nome1),all_of(nome2))) %>%
                  dplyr::summarize_at(.vars = nome3,
                                      list(media = ~mean(.,na.rm = TRUE))) %>%
                  dplyr::mutate(perc = 100*round(media/sum(media),4)) %>%
                  dplyr::rename(col1 = 1, col2 = 2)

                print(
                  ggplot2::ggplot(teste, aes(x = col1, y = media, fill = col2)) +
                    ggplot2::geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
                    ggplot2::geom_text(aes(label = paste0("(",round(media,2),", ",perc,"%",")")),position = position_dodge(width = 0.9), vjust = -1, size = 2) +
                    scale_fill_brewer(palette="Paired") +
                    ggplot2::labs(x = NULL,
                                  y = "count & perc",
                                  fill = colnames(dados[i]),
                                  title = paste("Média total de tarifa"))
                )



              }

              print(
                ggplot2::ggplot(dados, aes_string(x = nome3,y = nome1)) +
                  ggplot2::geom_boxplot() +
                  ggplot2::coord_flip() +
                  ggplot2::facet_wrap(as.formula(paste("~", nome2))) +
                  ggplot2::labs(
                    title = paste(nome1,nome2,nome3,sep = " x "))
              )
              stop("teste")} ,error = function(e){})

            tryCatch({
              print(
                knitr::kable(dados %>%
                               dplyr::group_by_at(vars(all_of(nome1),all_of(nome2))) %>%
                               dplyr::summarize_at(.vars = nome3,
                                                   list(n = ~ n(),
                                                        ~ min(.,na.rm = TRUE),
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
                             caption = paste(nome1,nome2,nome3,sep = " x ")) %>%
                  kableExtra::kable_styling(bootstrap_options = c("striped", "hover")))
              stop("teste")} ,error = function(e){})


          }
        }
      }
    }
  }






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

      for (k in 1:ncol(dados)){
        if (is.factor(dados[,k]) == TRUE | is.character(dados[,k]) == TRUE) {

          tryCatch({
            a <- dados %>%
              dplyr::group_by(dados[,k]) %>%
              dplyr::summarize(corr = cor(!!sym(colnames(dados[i])),!!sym(colnames(dados[j]))))  %>%
              rename(col1 = 1)


            print(
              ggplot2::ggplot(a, aes(x = col1, y = corr,fill = col1)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::geom_text(aes(label = round(corr,2))) +
                scale_fill_brewer(palette="Paired") +
                ggplot2::labs(x = NULL,
                              y = "Correlação",
                              fill = colnames(dados[i]),
                              title = paste("Correlação entre ",colnames(dados[i])," e ",colnames(dados[j]))) +
                ggplot2::lims(y = c(-1,1)) +
                ggplot2::coord_flip()
            )

            stop("teste")} ,error = function(e){})
        }


      }


    }
  }







}
