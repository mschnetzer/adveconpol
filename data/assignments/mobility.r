library(tidyverse)
library(waffle)
library(msthemes)
library(wesanderson)

faclevel <- c("Pflichtschule","Lehre/BMS","Höhere Schule (AHS/BHS)","Hochschule/Akademie")

tibble(labels=factor(faclevel,levels=rev(faclevel)),
       'Eltern mit Lehrabschluss'=c(7.5,59.3,17.3,15.9),
       'Eltern mit Universitätsabschluss'=c(3.6,15.6,23.4,57.3)) -> df
plotdf <- df %>% gather(cat,value,2:3) %>% mutate(value = round(value,0))

ggplot(plotdf, aes(fill=labels, values=value)) +
  geom_waffle(color = "white", size=1.125, n_rows = 5) +
  facet_wrap(~cat, ncol=1) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_manual(values=met.brewer("Lakota"), "Bildung der Kinder (25-44 Jahre)") +
  theme_ms(grid=F,alttf=T) +
  theme(strip.text.x=element_text(size=12,margin=margin(b=5, t=5)),
        plot.caption = element_text(margin=margin(t=4)),
        plot.title = element_text(margin=margin(b=6)),
        legend.title=element_text(size=9)) +
  coord_equal() +
  labs(
    title = "Bildungsmobilität in Österreich",caption="Daten: Bildung in Zahlen 2019/20, Statistik Austria. Grafik: @matschnetzer"
  ) +
  theme_enhance_waffle()

ggsave("edumob1.png", width=8, height=4, dpi=300)
