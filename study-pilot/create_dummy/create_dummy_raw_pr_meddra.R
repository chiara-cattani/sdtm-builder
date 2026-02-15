# =============================================================================
# Create Dummy Raw Data: pr_meddra.csv (MedDRA Coded Procedures)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/pr_meddra.csv
# Depends: raw/pr.csv (reads PR terms for MedDRA coding)
# =============================================================================

library(tibble)

pr <- read.csv("raw/pr.csv", stringsAsFactors = FALSE)

# MedDRA lookup for procedures
meddra_pr_lookup <- tibble(
  term        = c("Blood transfusion", "Appendectomy", "Tonsillectomy",
                  "Vaccination", "Dental cleaning", "Physical examination",
                  "Circumcision", "Caesarean section", "Colonoscopy",
                  "MRI scan", "Ultrasound", "X-ray"),
  llt_name    = c("Blood transfusion", "Appendicectomy", "Tonsillectomy",
                  "Vaccination", "Teeth cleaning", "Physical examination",
                  "Circumcision", "Caesarean section", "Colonoscopy",
                  "Magnetic resonance imaging", "Ultrasound scan", "X-ray"),
  llt_code    = c("10005841", "10003074", "10044104",
                  "10046851", "10043262", "10034898",
                  "10009182", "10007560", "10010029",
                  "10025444", "10045265", "10048380"),
  pt_name     = c("Blood transfusion", "Appendicectomy", "Tonsillectomy",
                  "Vaccination", "Dental prophylaxis", "Physical examination",
                  "Circumcision", "Caesarean section", "Colonoscopy",
                  "Magnetic resonance imaging", "Ultrasonography", "Radiography"),
  pt_code     = c("10005841", "10003074", "10044104",
                  "10046851", "10012832", "10034898",
                  "10009182", "10007560", "10010029",
                  "10025444", "10045265", "10037782"),
  hlt_name    = c("Transfusion procedures", "Appendix therapeutic procedures",
                  "Tonsillar therapeutic procedures",
                  "Prophylactic procedures NEC",
                  "Dental therapeutic procedures NEC",
                  "Physical examination procedures",
                  "Male reproductive tract surgical procedures",
                  "Obstetric procedures",
                  "Lower GI tract endoscopic procedures",
                  "Magnetic resonance imaging",
                  "Ultrasonic and echographic investigations",
                  "Skeletal and cardiac muscle analyses"),
  hlt_code    = c("10044660", "10003076", "10044105",
                  "10037041", "10012197", "10034899",
                  "10026936", "10029906", "10010030",
                  "10025445", "10045266", "10039623"),
  hlgt_name   = c("Transfusions and related procedures",
                  "Gastrointestinal therapeutic procedures",
                  "Tonsillar therapeutic procedures NEC",
                  "Prophylaxis", "Dental therapeutic procedures",
                  "Physical examination topics",
                  "Male reproductive tract procedures",
                  "Obstetric procedures",
                  "Gastrointestinal investigations",
                  "Imaging procedures NEC",
                  "Diagnostic procedures NEC",
                  "Skeletal and cardiac muscle analyses"),
  hlgt_code   = c("10044661", "10017965", "10044106",
                  "10037042", "10012198", "10034900",
                  "10026937", "10029907", "10017965",
                  "10025446", "10045267", "10039624"),
  soc_name    = rep("Surgical and medical procedures", 12),
  soc_code    = rep("10042613", 12),
  pt_soc_name = rep("Surgical and medical procedures", 12),
  pt_soc_code = rep("10042613", 12),
  soc_list    = rep("PRIMARY", 12)
)

pr_meddra_rows <- list()
for (r in seq_len(nrow(pr))) {
  term <- pr$PRTrt[r]
  m <- meddra_pr_lookup[meddra_pr_lookup$term == term, ]
  if (nrow(m) == 0L) m <- meddra_pr_lookup[1, ]

  pr_meddra_rows[[r]] <- tibble(
    SubjectId      = pr$SubjectId[r],
    PRSPID         = pr$PRSPID[r],
    LLT_Name       = m$llt_name[1],
    LLT_Code       = m$llt_code[1],
    PT_Name        = m$pt_name[1],
    PT_Code        = m$pt_code[1],
    HLT_Name       = m$hlt_name[1],
    HLT_Code       = m$hlt_code[1],
    HLGT_Name      = m$hlgt_name[1],
    HLGT_Code      = m$hlgt_code[1],
    SOC_Name       = m$soc_name[1],
    SOC_Code       = m$soc_code[1],
    PT_SOC_Name    = m$pt_soc_name[1],
    PT_SOC_Code    = m$pt_soc_code[1],
    SOC_List       = m$soc_list[1],
    DictInstance   = "MedDRA 27.0",
    Version        = "27.0",
    CodedOnDate    = format(as.Date(pr$PRStDat[r]) + sample(1:5, 1), "%Y-%m-%d"),
    InitiatedDate  = format(as.Date(pr$PRStDat[r]), "%Y-%m-%d"),
    LastEditedDate = format(as.Date(pr$PRStDat[r]) + sample(1:8, 1), "%Y-%m-%d")
  )
}
pr_meddra <- dplyr::bind_rows(pr_meddra_rows)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(pr_meddra, "raw/pr_meddra.csv", row.names = FALSE, na = "")
cat("Created raw/pr_meddra.csv:", nrow(pr_meddra), "rows x", ncol(pr_meddra), "cols\n")
