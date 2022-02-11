library(tidyverse)
library(themes360info)
library(ggtext)
library(here)

# chart 1:
# https://aptika.kominfo.go.id/wp-content/uploads/2020/11/Survei-Literasi-Digital-Indonesia-2020.pdf
# p. 43
stopping_hoaxes <-
  tribble(
    ~ actor,                                    ~ prop,
    "Ministry of\nCommunication & Information",  0.548,
    "Citizens",                                 0.45,
    "National army\nand national police",        0.447,
    "News organisations",                       0.227,
    "Digital platforms",                        0.201,
    "President",                                0.155,
    "Journalists",                              0.146,
    "Local opinion leaders",                    0.141,
    "National intelligence agency",             0.116,
    "Religious leaders",                        0.093,
    "I don't know",                             0.017) %>%
  mutate(actor = fct_reorder(actor, prop, .desc = TRUE))

stopping_hoaxes %>%
  {
    ggplot(.) +
      aes(x = actor, y = prop) +
      geom_col(fill = "black") +
      # labels inside bigger bars
      geom_text(
        aes(label = scales::percent(prop, accuracy = 0.1)),
        data = filter(stopping_hoaxes, prop > 0.05),
        hjust = "right", nudge_y = -0.01,
        colour = "white", size = 4.5, family = "Body 360info", fontface = "bold") +
      # labels outside smaller bars
      geom_text(
        aes(label = scales::percent(prop, accuracy = 0.1)),
        data = filter(stopping_hoaxes, prop < 0.05),
        hjust = "left", nudge_y = 0.01,
        colour = "black", size = 4.5, family = "Body 360info", fontface = "bold") +
      # question annotation
      annotate("richtext",
        x = 11, y = Inf,
        hjust = "inward", vjust = "inward",
        label = paste(
          "\"Among these institutions<br>or actors, ",
          "who do you think<br>should act to stop the<br>distribution of hoaxes?\""),
        label.colour = NA, fill = NA,
        family = "Body 360info", fontface = "bold", size = 7) +
      scale_y_continuous(position = "right", labels = scales::label_percent()) +
      coord_flip() +
      theme_360() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
      labs(
        x = NULL, y = "Proportion of respondents",
        # title = toupper("Blah"),
        # subtitle = toupper("Bigger question here"),
        caption = "**SOURCE:** xxxxxxxxxx")
  } %>%
  save_360plot(here("out", "chart1.png"), shape = "square")

# chart 2:

personal_responsibility <-
  tribble(
    ~ action,                                         ~ `2020`,  ~ `2021`,
    "To look for the truth\nof information I receive", 0.841,        0.838,
    "If it's proven as hoaxes,\nI will reprimand him", 0.269,        0.179,
    "Ignore/delete",                                  0.074,        0.085,
    "Report people who\nspread hoaxes",                0.059,        0.06,
    "I don't know",                                   0.014,        0.01) %>%
  mutate(action = fct_reorder(action, `2020`, .desc = TRUE)) %>%
  pivot_longer(-action, names_to = "year", values_to = "prop") %>%
  mutate(year = factor(year, levels = c("2020", "2021")))

personal_responsibility %>%
  {
    ggplot(.) +
      aes(x = action, y = prop) +
      geom_col(aes(fill = year),
        position = position_dodge2(reverse = TRUE)) +
      # question annotation
      annotate("richtext",
        x = 4, y = max(.$prop, na.rm = TRUE),
        hjust = "inward", vjust = "inward",
        label = paste(
          "\"What would you do",
          "to prevent the distribution",
          "of hoaxes/fake news?\"", sep = "<br>"),
        label.colour = NA, fill = NA,
        family = "Body 360info", fontface = "bold", size = 7) +
      scale_y_continuous(position = "right", labels = scales::label_percent()) +
      scale_fill_manual(name = NULL, values = c(
        "2020" = pal_360[["blue"]],
        "2021" = pal_360[["darkblue"]])) +
      coord_flip() +
      theme_360() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top", legend.direction = "horizontal") +
      labs(
        x = NULL, y = "Proportion of respondents",
        # title = toupper("Blah"),
        # subtitle = toupper("Bigger question here"),
        caption = "**SOURCE:** xxxxxxxxxx")
  } %>%
  save_360plot(here("out", "chart2.png"), shape = "sdtv-landscape")
