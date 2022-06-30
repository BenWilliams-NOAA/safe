#' create the main safe table
#'
#' @param data a specific dataframe holding results
#' @param year what is the current year
#' @param tier what tier
#' @param c1 estimated catch current year
#' @param c2 proj catch year + 1
#' @param c3 proj catch year + 2
#'
#' @export main_table
#'
main_table <- function(data, year, tier, c1, c2, c3){

  flextable::flextable(data) %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = 11, part = "all") %>%
    flextable::add_header_row(value = c( "", "a", "b"), colwidths = c(1,2,2)) %>%
    flextable::compose(i=2, j=1,
            value = flextable::as_paragraph( "Quantity/Status"),
            part = "header") %>%
    flextable::compose(i = 1, j = 2:3,
            value = flextable::as_paragraph( "As estimated or " , flextable::as_i("specified last"), " year for:"),
            part = "header") %>%
    flextable::compose(i = 1, j = 4:5,
            value = flextable::as_paragraph( "As estimated or " , flextable::as_i("recommended this"), " year for:"),
            part = "header") %>%
    flextable::compose(i = 2, j = 2,
                       value = flextable::as_paragraph(as.character(year)), part = "header") %>%
    flextable::compose(i = 2, j = 3:4,
                       value = flextable::as_paragraph(flextable::as_chunk(as.character(year + 1))),
            part = "header") %>%
    flextable::compose(i = 2, j = 5,
                       value = flextable::as_paragraph(flextable::as_chunk(as.character(year + 2))),
            part = "header") %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(j=2:5, align = "center") %>%
    flextable::align(j = 1, part = "header", align="left") %>%
    flextable::bold(i = 2, j = 1, part = "header") %>%
    flextable::width(j = 1, width = 2.5) %>%
    flextable::width(j = 2:5, width = 0.65) %>%
    flextable::bg(j = 2, bg = "#f7f7f7", part = "all") %>%
    flextable::bg(j = 3, bg = "#f7f7f7", part = "all") %>%
    flextable::border_remove() %>%
    flextable::hline_top(part = "header") %>%
    flextable::hline_top() %>%
    flextable::vline_right(part = "body") %>%
    flextable::vline_right(part = "header") %>%
    flextable::vline_left(part = "body") %>%
    flextable::vline_left(part = "header") -> tbl

  if(tier == 3){
    tier = "3a"
      flextable::font(
        flextable::footnote(tbl, i=2, j=4:5, part = "header", ref_symbols = "*",
                            value = flextable::as_paragraph(
                              flextable::as_chunk(paste0("Projections are based on an estimated catch of ",
                                                         prettyNum(c1, big.mark = ","), " t for ", year,
                                                         " and estimates of ",
                                                         prettyNum(c2, big.mark = ","),
                                                         " t and ",
                                                         prettyNum(c3, big.mark = ","),
                                                         " t used in place of maximum permissible ABC for ",
                                                         year+1, " and ", year+2, "."))),
                            sep = "."),
        fontname = "Times New Roman",
        part = "all") %>%
      flextable::compose(i = 2, j = 2:5,
                         value = flextable::as_paragraph(tier)) %>%
      flextable::compose(i = 5, j = 1,
                         value = flextable::as_paragraph("B", flextable::as_sub("100%"))) %>%
      flextable::compose(i = 6, j = 1,
                         value = flextable::as_paragraph("B", flextable::as_sub("40%"))) %>%
      flextable::compose(i = 7, j = 1,
                         value = flextable::as_paragraph("B", flextable::as_sub("35%"))) %>%
      flextable::compose(i = 8, j = 1,
                         value = flextable::as_paragraph("F", flextable::as_sub("OFL"))) %>%
      flextable::compose(i = 9, j = 1,
                         value = flextable::as_paragraph(flextable::as_i("max"),"F", flextable::as_sub("ABC"))) %>%
      flextable::compose(i = 10, j = 1,
                         value = flextable::as_paragraph("F", flextable::as_sub("ABC"))) %>%
      flextable::compose(i = 12, j = 1,
                         value = flextable::as_paragraph(flextable::as_i("max"),"ABC (t)")) %>%
      flextable::compose(i = 14, j = 1,
                         value = flextable::as_paragraph("")) %>%
      flextable::compose(i = 14, j = 2:3,
                         value = flextable::as_paragraph( "As determined " , flextable::as_i("last"), " year for:")) %>%
      flextable::compose(i = 14, j = 4:5,
                         value = flextable::as_paragraph( "As determined " , flextable::as_i("this"), " year for:")) %>%
      flextable::compose(i = 15, j = 2,
                         value = flextable::as_paragraph(flextable::as_chunk(as.character(year - 1)))) %>%
      flextable::compose(i = 15, j = 3:4,
                         value = flextable::as_paragraph(flextable::as_chunk(as.character(year)))) %>%
      flextable::compose(i = 15, j = 5,
                         value = flextable::as_paragraph(flextable::as_chunk(as.character(year + 1)))) %>%
      flextable::compose(i=16, j=c(2,4),
                         value = flextable::as_paragraph("No")) %>%
      flextable::compose(i=16, j=c(3,5),
                         value = flextable::as_paragraph("n/a")) %>%
      flextable::compose(i=17:18, j=c(3,5),
                         value = flextable::as_paragraph("No")) %>%
      flextable::compose(i=17:18, j=c(2,4),
                         value = flextable::as_paragraph("n/a")) %>%
      flextable::colformat_double(i = c(3:7, 11:13), j = 2:5,
                                  big.mark=",", digits = 0, na_str = "N/A") %>%
      flextable::merge_h(i=14, part = "body") %>%
      flextable::bold(i = 15, j = 1) %>%
      flextable::hline(i=13) %>%
      flextable::hline(i=15) %>%
      flextable::hline(i=18) %>%
      flextable::fix_border_issues()

  } else if(tier==1){

    if(nrow(tbl$body$data)>17){
      stop("this is not a tier 1 input, \nmaybe you have a tier 3 stock...")
    }

    tier = "1a"

    tbl %>%
      footnote(i=2, j=4:5, part = "header", ref_symbols = "*",
               value = as_paragraph(as_chunk(paste0("Projections are based on an estimated catch of ",
                                                    prettyNum(c1, big.mark = ","), " t for ", year,
                                                    " and estimates of ",
                                                    prettyNum(c2, big.mark = ","),
                                                    " t and ",
                                                    prettyNum(c3, big.mark = ","),
                                                    " t used in place of maximum permissible ABC for ",
                                                    year+1, " and ", year+2, "."))),
               sep = ".") %>%
      compose(i = 2, j = 2:5, value = as_paragraph(tier), part = "body") %>%
      compose(i = 4, j = 1, value = as_paragraph("F", as_sub("OFL")), part = "body") %>%
      compose(i = 5, j = 1, value = as_paragraph(as_i("max"),"F", as_sub("ABC")), part = "body") %>%
      compose(i = 6, j = 1, value = as_paragraph("F", as_sub("ABC")), part = "body") %>%
      compose(i = 7, j = 1, value = as_paragraph(as_i("max"),"ABC (t)"), part = "body") %>%
      compose(i = 13, j = 1, value = as_paragraph("")) %>%
      compose(i = 13, j = 2:3,
              value = as_paragraph( "As determined " , as_i("last"), " year for:")) %>%
      compose(i = 13, j = 4:5,
              value = as_paragraph( "As determined " , as_i("this"), " year for:")) %>%
      compose(i = 14, j = 2,
              value = as_paragraph(as_chunk(as.character(year - 1)))) %>%
      compose(i = 14, j = 3:4,
              value = as_paragraph(as_chunk(as.character(year)))) %>%
      compose(i = 14, j = 5,
              value = as_paragraph(as_chunk(as.character(year + 1)))) %>%
      compose(i=15, j=c(2,4), value = as_paragraph("No")) %>%
      compose(i=15, j=c(3,5), value = as_paragraph("n/a")) %>%
      compose(i=16:17, j=c(3,5), value = as_paragraph("No")) %>%
      compose(i=16:17, j=c(2,4), value = as_paragraph("n/a")) %>%
      colformat_double(i = c(3:7, 10:12), j = 2:5, big.mark=",", digits = 0, na_str = "N/A") %>%
      merge_h(i=13, part = "body") %>%
      align(align = "center", part = "header") %>%
      align(j=2:5, align = "center", part = "body") %>%
      align(j = 1, part = "header", align="left") %>%
      bold(i = 2, j = 1, part = "header") %>%
      bold(i = 15, j = 1) %>%
      hline(i=12) %>%
      hline(i=14) %>%
      hline(i=17) %>%
      fix_border_issues()
  } else if(tier == 5){

    tier = "5"

    tbl %>%
      compose(i = 2, j = 2:5, value = as_paragraph(tier), part = "body") %>%
      compose(i = 4, j = 1, value = as_paragraph("F", as_sub("OFL")), part = "body") %>%
      compose(i = 5, j = 1, value = as_paragraph(as_i("max"),"F", as_sub("ABC")), part = "body") %>%
      compose(i = 6, j = 1, value = as_paragraph("F", as_sub("ABC")), part = "body") %>%
      compose(i = 8, j = 1, value = as_paragraph(as_i("max"),"ABC (t)"), part = "body") %>%
      compose(i = 10, j = 1, value = as_paragraph("")) %>%
      compose(i = 10, j = 2:3,
              value = as_paragraph( "As determined " , as_i("last"), " year for:")) %>%
      compose(i = 10, j = 4:5,
              value = as_paragraph( "As determined " , as_i("this"), " year for:")) %>%
      compose(i = 11, j = 2,
              value = as_paragraph(as_chunk(as.character(year - 1)))) %>%
      compose(i = 11, j = 3:4,
              value = as_paragraph(as_chunk(as.character(year)))) %>%
      compose(i = 11, j = 5,
              value = as_paragraph(as_chunk(as.character(year + 1)))) %>%
      compose(i=12, j=c(2,4), value = as_paragraph("No")) %>%
      compose(i=12, j=c(3,5), value = as_paragraph("n/a")) %>%
      colformat_double(i = c(3,7:9), j = 2:5, big.mark=",", digits = 0, na_str = "N/A") %>%
      merge_h(i=10, part = "body") %>%
      align(align = "center", part = "header") %>%
      align(j=2:5, align = "center", part = "body") %>%
      align(j = 1, part = "header", align="left") %>%
      hline(i=9) %>%
      hline(i=11) %>%
      hline(i=12) %>%
      fix_border_issues()
  } else {
    tier = "6"

    tbl %>%
      compose(i = 1, j = 2:5, value = as_paragraph(tier), part = "body") %>%
      compose(i = 3, j = 1, value = as_paragraph(as_i("max"),"ABC (t)"), part = "body") %>%
      compose(i = 5, j = 1, value = as_paragraph("")) %>%
      compose(i = 5, j = 2:3,
              value = as_paragraph( "As determined " , as_i("last"), " year for:")) %>%
      compose(i = 5, j = 4:5,
              value = as_paragraph( "As determined " , as_i("this"), " year for:")) %>%
      compose(i = 6, j = 2,
              value = as_paragraph(as_chunk(as.character(year - 1)))) %>%
      compose(i = 6, j = 3:4,
              value = as_paragraph(as_chunk(as.character(year)))) %>%
      compose(i = 6, j = 5,
              value = as_paragraph(as_chunk(as.character(year + 1)))) %>%
      compose(i=7, j=c(2,4), value = as_paragraph("No")) %>%
      compose(i=7, j=c(3,5), value = as_paragraph("n/a")) %>%
      colformat_double(i = c(2:4), j = 2:5, big.mark=",", digits = 0, na_str = "N/A") %>%
      merge_h(i=5, part = "body") %>%
      align(align = "center", part = "header") %>%
      align(j=2:5, align = "center", part = "body") %>%
      align(j = 1, part = "header", align="left") %>%
      hline(i=4) %>%
      hline(i=6) %>%
      hline(i=7) %>%
      fix_border_issues()
  }
}
