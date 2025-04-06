# Installing Packages
install.packages(
  c("ggplot2", "tibble", "tidyr", "forcats", "purrr", "prismatic", "corrr", 
    "cowplot", "ggforce", "ggrepel", "ggridges", "ggsci", "ggtext", "ggthemes", 
    "grid", "gridExtra", "patchwork", "rcartocolor", "scico", "showtext", 
    "shiny", "plotly", "highcharter", "echarts4r", "readr","dplyr", "devtools")
)

library("devtools")
devtools::install_github("Mikata-Project/ggthemr")

# Importing the Elections Dataset
elections <- readr::read_csv("elections.csv")

# Filter the first 48 observations
elections_filtered <- elections[1:48, ]
write.csv(elections_filtered, "elections_filtered.csv", row.names = FALSE)


# Overview of Dataset
tibble::glimpse(elections_filtered)

library(ggplot2)
library(scales)
library(patchwork)
library(tidyr)  # for pivot_longer
library(dplyr)  # for %>% pipe operator

# Theme Setting
library("ggthemr")
ggthemr("light")

theme_set(theme_test())


# Registered Voters
# Create a bar chart of votes by candidate
reg_vot <- ggplot(elections_filtered, aes(x = County, y = Reg_Voters)) +
  geom_bar(stat = "identity", fill = "gold4") +
  scale_y_continuous(labels = comma) +
  labs(title = "Registered Voters Per County", 
       x = "County", 
       y = "Voters",
       caption = "Data: IEBC") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8.5),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 1),
        legend.title = element_blank())

reg_vot

# Total Votes Cast
tot_vot <- ggplot(elections_filtered, aes(x = County, y = `Total Votes Cast`)) +
  geom_bar(stat = "identity", fill = "green") +
  scale_y_continuous(labels = comma) +
  labs(title = "Total Votes Per County", 
       x = "County", 
       y = "Votes",
       caption = "Data: IEBC") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8.5),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 1),
        legend.title = element_blank())

tot_vot

# Rejected Votes
# Creating a Bar Chart of Rejected Votes
rej_vot <- ggplot(elections_filtered, aes(x = County, y = Rejected)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  scale_y_continuous(labels = comma) +
  labs(title = "Rejected Votes Per County",
       x = "County",
       y = "Votes",
       caption = "Data: IEBC") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8.5),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 1),
        legend.title = element_blank())

rej_vot

# Voter Turnout Per IEBC
# Ensure percentages are numeric
elections_filtered$Voter_TurnOut <- as.numeric(sub("%", "", elections_filtered$Voter_TurnOut))

turnout <- ggplot(elections_filtered, aes(x = County, y = `Voter_TurnOut`)) +
  geom_bar(stat = "identity", fill = "green2") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  labs(title = "Voter TurnOut Per County",
       x = "County",
       y = "Turn Out",
       caption = "Data: IEBC") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8.5),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 1),
        legend.title = element_blank()) +
  # Add horizontal dotted line at 50%
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", linewidth = 1)

turnout

# Raila Odinga Votes Per IEBC
rao <- ggplot(elections_filtered, aes(x = County, y = `Raila-IEBC`)) +
  geom_bar(stat = "identity", fill = "blue4") +
  scale_y_continuous(labels = comma) +
  labs(title = "Odinga's Votes Per County",
       x = "County",
       y = "Votes",
       caption = "Data: IEBC") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8.5),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 1),
        legend.title = element_blank())

rao

# William Ruto Votes Per IEBC
ruto <- ggplot(elections_filtered, aes(x = County, y = `Ruto-IEBC`)) +
  geom_bar(stat = "identity", fill = "yellow") +
  scale_y_continuous(labels = comma) +
  labs(title = "Ruto's Votes Per County",
       x = "County",
       y = "Votes",
       caption = "Data: IEBC") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8.5),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 1),
        legend.title = element_blank())

ruto

# Wajackoyah Votes Per IEBC
wajackoyah <- ggplot(elections_filtered, aes(x = County, y = `Wajackoyah`)) +
  geom_bar(stat = "identity", fill = "green3") +
  scale_y_continuous(labels = comma) +
  labs(title = "Wajackoyah's Votes Per County",
       x = "County",
       y = "Votes",
       caption = "Data: IEBC") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8.5),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 1),
        legend.title = element_blank())

wajackoyah

# Waihiga Votes Per IEBC
waihiga <- ggplot(elections_filtered, aes(x = County, y = `Waihiga`)) +
  geom_bar(stat = "identity", fill = "purple3") +
  scale_y_continuous(labels = comma) +
  labs(title = "Waihiga's Votes Per County",
       x = "County",
       y = "Votes",
       caption = "Data: IEBC") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8.5),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 1),
        legend.title = element_blank())

waihiga

# Differential of Voting Results between IEBC and Independent Observers
# Reshape data into long format using pivot_longer
data_long <- elections_filtered %>%
  pivot_longer(cols = c(`RAO Diff`, `WSR Diff`), 
               names_to = "Candidate", 
               values_to = "Difference")

comparison = ggplot(data_long, aes(x = County, y = Difference, fill = Candidate)) +
  geom_bar(stat = "identity") +
  labs(title = "Differential of Voting Results between IEBC and Independent Observers",
       x = "County", y = "Difference",
       fill = "Candidate") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8.5),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 1),
        legend.title = element_blank())

comparison

# Summarize the total votes for each candidate/column
totals <- elections_filtered %>%
  select(`Raila-IEBC`, `Ruto-IEBC`, Wajackoyah, Waihiga, Rejected) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Candidate", values_to = "Votes")

# Calculate the percentage of each category
totals <- totals %>%
  mutate(Percentage = Votes / sum(Votes) * 100)

# Modify Candidate names for legend
totals$Candidate <- recode(totals$Candidate,
                           `Raila-IEBC` = "Raila",
                           `Ruto-IEBC` = "Ruto")

# Create the pie chart with percentages
final = ggplot(totals, aes(x = "", y = Votes, fill = Candidate)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +  
  # Add percentages as labels
  labs(title = "Presidential Election Results",
       x = NULL, y = NULL, caption = "Data:IEBC") +
  theme_void() +  # Clean up axes and background
  theme(legend.title = element_blank(), 
        legend.position = "right",  # Position legend to the right
        plot.title = element_text(hjust = 0.5, size = 12))  # Center title

final

# Combined Voter Count per Candidate per County
vcpcpc = (rao + ruto) /
  (wajackoyah + waihiga)

# Combined Overall Election Data - 1
(reg_vot + tot_vot ) /
  (rej_vot + turnout)

# Combined Overall Election Data - 2
(comparison + final)

#Saving Plots as Images
ggsave("Final Presidential Results.png", plot = final, width = 8, height = 6, dpi = 300)

ggsave("Registered Voters per County.png", plot = reg_vot, width = 8, height = 6, dpi = 300)

ggsave("Total Votes Cast per County.png", plot = tot_vot, width = 8, height = 6, dpi = 300)

ggsave("Rejected Votes Cast per County.png", plot = rej_vot, width = 8, height = 6, dpi = 300)

ggsave("Voter Turnout per County.png", plot = turnout, width = 8, height = 6, dpi = 300)

ggsave("Vote Differentials per County.png", plot = comparison, width = 8, height = 6, dpi = 300)

ggsave("Candidate Votes per County.png", plot = vcpcpc, width = 12, height = 6, dpi = 300)
