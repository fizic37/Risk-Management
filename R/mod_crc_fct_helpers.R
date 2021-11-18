# Trebuie sa tin cont de faptul ca un beneficiar apare pe mai multe randuri intr-un raport

sumar_baza_date_crc_func <-
  function(df) {
    df %>% dplyr::group_by(`Cod debitor`, `Data situatie risc global`) %>%
      dplyr::summarise(
        Nr_beneficiari = dplyr::n_distinct(`Cod debitor`),
        Total_rate_datorate = sum(Rate_datorate),
        Rate_medii_datorate = sum(Rate_datorate),
        are_restante_peste_30_zile = mean(are_restante_peste_30_zile),
        scor_mediu_serv_datorie = mean(scor_serv_datorie),
        sume_totale_utilizate = sum(`Suma datorata utilizata`)
      ) %>%
      dplyr::group_by(`Data situatie risc global`) %>% dplyr::summarise(
        `Nr beneficiari` = sum(Nr_beneficiari),
        `*Total rate datorate` = sum(Total_rate_datorate),
        `*Rate datorate medii per beneficiar` =      mean(Rate_medii_datorate),
        `Ponderea nr de benef cu restante peste 30 de zile` = mean(are_restante_peste_30_zile),
        `Scor mediu serviciul datoriei` = mean(scor_mediu_serv_datorie),
        `Scor mediu serviciul datoriei ponderat cu sumele utilizate` = weighted.mean(scor_mediu_serv_datorie,
                                                                                     w = sume_totale_utilizate)
      ) %>% dplyr::arrange(desc(`Data situatie risc global`))
  }