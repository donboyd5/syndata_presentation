
var <- "taxbc"

taxchange %>%
  filter(name==var) %>%
  select(ftype, agirange, name, value=pchange) %>%
  pivot_wider(names_from=ftype, values_from=value) %>%
  mutate(diff=syn - puf) %>%
  kable(digits=1, format="rst", format.args=list(big.mark=","))


compchange_flex <- function(var, tabdata_all, htext){
  tax_change <- tabdata_all %>%
    filter(name==var) %>%
    select(-taxplan) %>%
    pivot_wider(names_from=plantype, values_from=sum) %>%
    mutate(change=reform - baseline,
           pct_change=change / baseline * 100)
  
  change_comp <- tax_change %>%
    select(ftype, agirange, name, change) %>%
    mutate(change=change / 1e9) %>%
    pivot_wider(names_from=ftype, values_from=change) %>%
    mutate(diff=syn - puf,
           pdiff=diff / puf * 100)
  
  pch_comp <- tax_change %>%
    select(ftype, agirange, name, pct_change) %>%
    pivot_wider(names_from = ftype, values_from=pct_change, names_prefix = "pch_")
  
  ftdata <- change_comp %>%
    left_join(pch_comp, by=c("agirange", "name"))
  
  # header and row heights
  hh <- .8
  rh <- hh / 2 
  datacols <- c(1, 3:6, 8:9)
  
  ft <- ftdata %>%
    flextable(col_keys = c("agirange", 
                           "separator1",
                           "puf", "syn", "diff",
                           "pdiff",
                           "separator2",
                           "pch_puf", "pch_syn")) %>%
    set_header_labels(agirange="AGI range",
                      puf = "Enhanced\nPUF",
                      syn = "Synthetic\nfile",
                      diff="Difference in estimated impacts",
                      pdiff="Difference as % of PUF estimate",
                      pch_puf="Enhanced\nPUF",
                      pch_syn="Synthetic\nfile") %>%
    add_header_row(values=c("", "Estimated impacts", "", "% change from baseline"), colwidths=c(2, 4, 1, 2)) %>%
    add_header_row(values=c(htext), colwidths=c(9)) %>%
    align(i = 2, j = NULL, align = "center", part = "header") %>%
    colformat_num(j = c("puf", "syn", "diff"),
                  big.mark=",", digits = 1, na_str = "N/A") %>%
    colformat_num(j = ~ pdiff + pch_puf + pch_syn,
                  big.mark=",", digits = 1, na_str = "N/A") %>%
    width(j= ~ agirange, width=2) %>%
    width(j = ~ puf + syn + diff + pdiff, width=1.5) %>%
    width(j= ~ separator1 + separator2, width=0.3) %>%
    height_all(part = "header", height = hh) %>%
    height_all(part = "body", height = rh) %>%
    # now control the themes # theme_booktabs() %>%
    border_remove() %>%
    hline(i=2, j=~puf, border = fp_border(width = 1), part = "header") %>%
    hline(i=2, j=~pch_puf, border = fp_border(width = 1), part = "header") %>%
    hline_bottom(j=datacols, border = fp_border(width = 1), part = "header") %>%
    hline(i=7, j=datacols, border = fp_border(width = 1), part = "body") %>%
    border(j = ~ separator1 + separator2, border = fp_border(width=0), part = "all") %>%
    fontsize(size = 16, part = "all") %>%
    fontsize(i=1, size = 24, part = "header") %>%
    bold(i=1, part = "header") %>%
    color(i = 1, part = "header", color="#2554E7")
  return(ft)  
}

htext <- "Trump 2017 proposal compared with 2017 law as baseline\nRegular tax before credits, $ billions"
ft <- compchange_flex("taxbc", tabdata_all, htext)
ft
save_as_image(ft, path = here::here("results", "ft.png"), zoom=7)

ft %>% empty_blanks()


flextable_dim(ft)
sum(dim(ft)$widths); sum(dim(ft)$heights)
dim(ft)
dim_pretty(ft)

ft %>% theme_alafoli()
ft %>% theme_booktabs()
ft %>% theme_alafoli()
ft %>% theme_alafoli()

# theme_alafoli()
# theme_booktabs()
# theme_box()
# theme_tron()
# theme_tron_legacy()
# theme_vader()
# theme_vanilla()
# theme_zebra()


ft3 <- ft2 %>% 
  width(j= ~ agirange, width=2) %>%
  width(j = ~ puf + syn + diff + pdiff, width=1.5) %>%
  width(j= ~ separator1 + separator2, width=0.1) %>%
  height_all(part = "header", height = hh) %>%
  height_all(part = "body", height = rh) %>%
  fontsize(size = 16, part = "all")
dim(ft3)
sum(dim(ft3)$widths); sum(dim(ft3)$heights)
ft3
save_as_image(ft3, path = here::here("results", "ft.png"), zoom=7)

ft <- height_all( ft, height = .4 )
ft <- height( ft, i = 3, height = 1 )

ft2 <- ft
