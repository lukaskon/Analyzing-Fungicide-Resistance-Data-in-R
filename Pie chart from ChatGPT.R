# Load required libraries
library(ggplot2)
library(readxl)
library(dplyr)

# Example data (replace with your actual dataset)
#might need to read it in manually-make sure CCR is a character, not number
COMPLETE_FSAR_R <- read_excel("COMPLETE_FSAR_R.xlsx", 
                                +     col_types = c("text", "text", "text", 
                                                    +         "text", "numeric", "numeric", "numeric", 
                                                    +         "numeric", "numeric", "numeric", 
                                                    +         "numeric", "numeric", "numeric", 
                                                    +         "numeric", "numeric", "numeric", 
                                                    +         "numeric", "text", "numeric", "numeric", 
                                                    +         "numeric", "numeric", "numeric", 
                                                    +         "numeric", "numeric", "numeric", 
                                                    +         "numeric"))

View(COMPLETE_FSAR_R)

# Count the number of isolates for each CCR value
ccr_counts <- table(COMPLETE_FSAR_R$CCR)


# Calculate the percentage of isolates for each CCR value
ccr_percentages <- ccr_counts / sum(ccr_counts) * 100


# Combine values in columns 4-7
combined_count <- sum(ccr_counts[5:8])
combined_percentage <- sum(ccr_percentages[5:8])

# Create a new table with the combined value
ccr_counts_modified <- c(ccr_counts[1:4], `4+` = combined_count)
ccr_counts_modified

ccr_counts_modified <- c("CCR" = ccr_counts[1], "CCR" = ccr_counts[2],"CCR" = ccr_counts[3],
                         "CCR" = ccr_counts[4], "CCR4+" = combined_count)
ccr_counts_modified
names(ccr_counts_modified) <- gsub("\\.", "", as.character(names(ccr_counts_modified)))
ccr_counts_modified

ccr_percentages_m <- ccr_counts_modified / sum(ccr_counts_modified) * 100


# Create a data frame with CCR values and their corresponding percentages
ccr_data <- data.frame(CCR = names(ccr_percentages_m), Percentage = ccr_percentages_m)

# Plot the pie chart
ggplot(ccr_data, aes(x = "", y = Percentage, fill = CCR)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Percentage of Isolates with Each CCR Value",
       fill = "CCR") +
  theme_minimal() +
  theme(legend.position = "right")


#colors in RGB format for publication
color_palette <- c("#5E84BA", "#74B08B", "#8C969D", "#4E9694", "#8276A6")
color_palette2 <- c("#5E84BA", "#74B08B", "#57A7A4", "#62B7B1", "#9E9854")


pie_chart <- pie(ccr_percentages_m, col=color_palette,
                 labels = paste(names(ccr_percentages_m), "(", round(ccr_percentages_m, 1), "%)"),
                 border="black", cex=1)
pie_chart

tiff("Piechart_6x6_600res.tiff", units="in", width=6, height=6, res=600)
pie(ccr_percentages_m, col=color_palette,
    labels = paste(names(ccr_percentages_m), "(", round(ccr_percentages_m, 1), "%)"),
    border="black", cex=1)
dev.off()





