# ******************************************************************************
# Run study-pilot pipeline — DM + AE + PR + MH + CM + TV + TI + IE + SV + XS + DS + SC + BE + CE + CO + EC + EX + VS + QS + FA + RELREC
# ******************************************************************************

library(sdtmbuilder)
# During development, use devtools::load_all("..") to pick up source changes
devtools::load_all("..")

# Set working directory to study-pilot (adjust path if needed)
# setwd("study-pilot")

out <- run_study(
  config_path       = "metadata/config.yaml",
  domains           = c("DM", "AE", "PR", "MH", "CM", "TV", "TI", "IE", "SV", "XS", "DS", "SC", "BE", "CE", "CO", "EC", "EX", "VS", "QS", "FA", "ML", "APSC"),
  create_supp       = FALSE,
  generate_programs = TRUE,
  verbose           = TRUE
)

# Inspect DM
cat("\n--- DM ---\n")
str(out$results$DM$data, max.level = 1)

# Inspect AE
cat("\n--- AE ---\n")
str(out$results$AE$data, max.level = 1)

# Inspect PR
cat("\n--- PR ---\n")
str(out$results$PR$data, max.level = 1)

# Inspect MH
cat("\n--- MH ---\n")
str(out$results$MH$data, max.level = 1)

# Inspect CM
cat("\n--- CM ---\n")
str(out$results$CM$data, max.level = 1)

# Inspect TV
cat("\n--- TV ---\n")
str(out$results$TV$data, max.level = 1)

# Inspect TI
cat("\n--- TI ---\n")
str(out$results$TI$data, max.level = 1)

# Inspect IE
cat("\n--- IE ---\n")
str(out$results$IE$data, max.level = 1)

# Inspect XS
cat("\n--- XS ---\n")
str(out$results$XS$data, max.level = 1)

# Inspect DS
cat("\n--- DS ---\n")
str(out$results$DS$data, max.level = 1)

# Inspect SC
cat("\n--- SC ---\n")
str(out$results$SC$data, max.level = 1)

# Inspect BE
cat("\n--- BE ---\n")
str(out$results$BE$data, max.level = 1)

# Inspect CE
cat("\n--- CE ---\n")
str(out$results$CE$data, max.level = 1)

# Inspect VS
cat("\n--- VS ---\n")
str(out$results$VS$data, max.level = 1)

# Inspect QS
cat("\n--- QS ---\n")
str(out$results$QS$data, max.level = 1)

# Inspect FA
cat("\n--- FA ---\n")
str(out$results$FA$data, max.level = 1)

# Inspect RELREC
cat("\n--- RELREC ---\n")
if (!is.null(out$relrec) && nrow(out$relrec) > 0L) {
  cat("  Rows: ", nrow(out$relrec), "\n")
  print(out$relrec)
} else {
  cat("  No RELREC data generated.\n")
}
