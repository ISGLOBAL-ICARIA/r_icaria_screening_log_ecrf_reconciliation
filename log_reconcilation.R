date <- "20220221"

crf <- read.csv(paste0("participants_", date, ".csv"))
log <- read.csv(paste0("screening_log_", date, ".csv"))

crf$screening_date <- substr(crf$screening_date, 0, 10)
consent.by.hf.date <- table(crf$hf, crf$screening_date)
consent.by.hf.date <- as.data.frame(consent.by.hf.date)
colnames(consent.by.hf.date) <- c("hf", "screening_date", "n_consent_crf")

log <- log[, c("hf", "screening_date", "n_consent")]
colnames(log) <- c("hf", "screening_date", "n_consent_log")

reconciliation <- merge(
  x   = log, 
  y   = consent.by.hf.date, 
  by  = c("hf", "screening_date"),
  all = T
)


reconciliation$match <-
  (is.na(reconciliation$n_consent_log) & reconciliation$n_consent_crf == 0) |
  (reconciliation$n_consent_log == reconciliation$n_consent_crf) | 
  (reconciliation$n_consent_log == 0 & is.na(reconciliation$n_consent_crf))

reconciliation$match[which(is.na(reconciliation$match))] <- F

reconciliation <- reconciliation[which(!reconciliation$match), ]

reconciliation$diff <- reconciliation$n_consent_log - 
  reconciliation$n_consent_crf
reconciliation$diff[which(is.na(reconciliation$n_consent_log))] <- 
  0 - reconciliation$n_consent_crf[which(is.na(reconciliation$n_consent_log))]
reconciliation$diff[which(is.na(reconciliation$n_consent_crf))] <- 
  reconciliation$n_consent_log[which(is.na(reconciliation$n_consent_crf))]

write.csv(reconciliation, file = paste0("screening_log_crf_reconciliation_", date, ".csv"))