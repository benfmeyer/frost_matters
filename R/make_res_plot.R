#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dittmar
#' @param meyer
#' @param bdn
#' @param principe
#' @param fs_trw
#' @return
#' @author ben
#' @export
make_res_plot <- function(dittmar, meyer, bdn, principe, fs_trw, custom_theme) {

  .tri.to.squ<-function(x)
  {
    rn<-row.names(x)
    cn<-colnames(x)
    an<-unique(c(cn,rn))
    myval<-x[!is.na(x)]
    mymat<-matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
    for(ext in 1:length(cn))
    {
      for(int in 1:length(rn))
      {
        if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
        mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
        mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
      }
      
    }
    return(mymat)
  }
  
  
  dt <- dittmar$site_id |> 
    unique()
  pr <- principe$site_id |>
    unique()
  bd <- bdn$site_id |> 
    unique()
  my <- meyer$site_id |> 
    unique()
  
  soi <- fs_trw |> 
    mutate(ds = case_when(
      site_id %in% dt ~ "dittmar",
      site_id %in% pr ~ "principe",
      site_id %in% bd ~ "bdn",
      site_id %in% my ~ "meyer"
    ))
  

  rt <- function(x, n) {
    pre_dr <- rollmean(lag(x), k = n, align = "right", fill = NA)
    idx <- x / pre_dr
  }
  
  res <- soi |> group_by(site_id, idx, type) |> 
    mutate(res = rt(val, 2)) |> 
    filter(case_when(
      ds %in% c("meyer", "principe") ~ year  == 2011,
      ds == "dittmar" ~ year  ==  1953,
      ds == "bdn" ~ year == 2011
    ), idx != "rwl") 
    
  meds <- res |> 
    group_by(year, type) |> 
    summarize(rmed = median(res, na.rm = TRUE))
  
  sig <- res |>
    group_by(year) |> 
    group_split() |> 
    map(~pairwise.wilcox.test(.x$res, .x$type, paired = TRUE)) |> 
    map(~.tri.to.squ(.x$p.value)) |> 
    map(multcompLetters) |>
    map(pluck, "Letters") |> 
    map(as.data.frame) |> 
    map(rownames_to_column, "type") |> 
    map2(c(1953, 2011), ~mutate(.x, year = .y)) |> 
    bind_rows() |> 
    set_names(c("type", "letter", "year"))
    
  pal <- nationalparkcolors::park_palette("Saguaro", 3)
  
  rp <- res |> 
    ggplot(aes(factor(type), res, fill = type))+
    geom_boxplot(outlier.shape = NA, alpha = 0.4)+
    geom_point(aes(color = type), position = position_jitter(seed = 1, width = .2))+
    geom_text(data = sig, aes(factor(type), y = 0.2, color = type, label = letter), size = 6)+
    geom_hline(yintercept = 1, linetype = 2, color = "grey")+
    theme_clean()+
    facet_grid(~year)+
    scale_fill_manual(values = pal, labels = c("LPJ-GUESS", "LPJ-GUESS-FROST", "RWI"))+
    scale_x_discrete(labels = c("LPJ-GUESS", "LPJ-GUESS-FROST", "RWI"))+
    scale_y_continuous(breaks = seq(0.4, 1.6, 0.2))+
    scale_color_manual(values = pal, guide = "none")+
    labs(x = "LSF Year", y = "Resistance", fill = "Dataset")+
    custom_theme
    

  return(list(rp, meds))
}
