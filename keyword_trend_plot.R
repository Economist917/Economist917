library(ggrepel)

label_data <- filtered_keywords %>%
  filter(PY == max(PY))  # only last year for label

ggplot(filtered_keywords, aes(x = PY, y = freq, color = DE)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  geom_text_repel(data = label_data,
                  aes(label = DE),
                  hjust = 0, nudge_x = 0.5,
                  direction = "y",
                  segment.color = "grey50",
                  size = 3,
                  show.legend = FALSE) +
  scale_x_continuous(limits = c(2015, 2025), breaks = 2015:2024) +
  scale_y_continuous(limits = c(0, 750)) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  labs(
    title = "Keyword Trends (2015–2024)",
    x = "Year", y = "Frequency"
  )

top_labels <- label_data %>%
  top_n(7, freq)

geom_text_repel(data = top_labels, aes(label = DE), ...)

library(dplyr)
library(stringr)

DF_keywords <- DF %>%
  select(PY, DE) %>%
  filter(!is.na(DE)) %>%
  mutate(DE = tolower(DE)) %>%
  separate_rows(DE, sep = ";") %>%
  mutate(DE = trimws(DE)) %>%
  mutate(DE = case_when(
    DE %in% c("sdgs", 
              "sustainable development goals", 
              "sustainable development goals (sdgs)") ~ "sustainable development goals",
    TRUE ~ DE
  ))
# Count keyword frequency per year
keyword_trends <- DF_keywords %>%
  group_by(PY, DE) %>%
  summarise(freq = n(), .groups = "drop")

# Top 10 keywords overall
top_keywords <- keyword_trends %>%
  group_by(DE) %>%
  summarise(total_freq = sum(freq)) %>%
  arrange(desc(total_freq)) %>%
  slice_head(n = 10)

# Filter just those top keywords
top_keyword_trends <- keyword_trends %>%
  filter(DE %in% top_keywords$DE)
library(ggplot2)
library(ggrepel)
library(forcats)


# Label positions for final year
label_data <- top_keyword_trends %>%
  filter(PY == max(PY))

# Plot
ggplot(top_keyword_trends, aes(x = PY, y = freq, color = DE)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  geom_text_repel(data = label_data, aes(label = DE),
                  hjust = 0, nudge_x = 0.5, size = 3,
                  direction = "y", segment.color = "grey60",
                  show.legend = FALSE) +
  scale_x_continuous(breaks = 2015:2024, limits = c(2015, 2025)) +
  scale_y_continuous(limits = c(0, 750)) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  labs(title = "Keyword Trends (2015–2024)", x = "Year", y = "Frequency")
