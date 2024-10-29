library(argparse)
library(readr)
library(dplyr)

# Define argument parser
parser <- ArgumentParser(description="Perform ward clustering to a distance matrix.")

# Add arguments
parser$add_argument("--output_dir", "-o", dest="output_dir", type="character", help="output directory where files will be saved", default=getwd())
parser$add_argument("--name", "-n", dest="name", type="character", help="name of the dataset", default="ward")
parser$add_argument("--distances", dest="distances", type="character", help="distance matrix on which to perform ward clustering", default=NULL)
parser$add_argument('--clusters', dest="clusters", type="integer", help='number of clusters for ward', default=3)
parser$add_argument('--seed', dest="seed", type="integer", help='seed', default=42)

# Parse command-line arguments
opt <- parser$parse_args()

# Check if mandatory argument are provided
if (is.null(opt$distances)) {
  stop("Error: Mandatory argument --distances is required.")
}

# Read the distance matrix
distances_df <- read_csv(opt$distances)

# Extract the distance matrix, excluding the 'id' column
distance_matrix <- distances_df %>%
  select(-id) %>%
  as.matrix()

# Set the row names to the 'id' column
rownames(distance_matrix) <- distances_df$id

# Perform Ward clustering
set.seed(opt$seed)
hc <- hclust(as.dist(distance_matrix), method = "ward.D2")

# Cut the tree into the desired number of clusters
clusters <- cutree(hc, k = opt$clusters)

# Create a data frame for the clusters
clusters_df <- data.frame(id = distances_df$id, label = clusters)

# Write clusters to disk
write_csv(clusters_df, file.path(opt$output_dir, paste0(opt$name, ".clusters.csv")), na = "")

# Print success message
cat("Clustering complete. Results saved to:", file.path(opt$output_dir, paste0(opt$name, ".clusters.csv")), "\n")