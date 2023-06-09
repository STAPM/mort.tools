
#' Oesphageal SCC AC splits
#'
#' We split oesophageal cancer into its two main histological types: Squamous Cell Carcinoma (SCC) and Adenocarcinoma (AC).
#' We apportion overall oesophageal cancer prevalence between AC and SCC using data on percentage prevalence by age and sex.
#' CRUK requested data direct from the UK cancer registries on the number of oesophageal SCCs.
#' That data isn’t routinely available because it’s morphology (cell type) data, whereas what's published as standard is topography (body part).
#' Alternatively we could distinguish SCC using the body sites 'upper third of oesophagus' and 'middle third of oesophagus' (ICD-10 codes C15.3 and C15.4)
#' as a fairly good proxy for SCC (Coupland 2012). These percentages are for 2014 for England.
#' We therefore need to be aware that in modelling periods that are substantially earlier or later, the percentages might not hold true.
#' Specifically, AC incidence in males has risen over time while AC incidence in females, and SCC incidence in both sexes, has remained fairly stable –
#' so the relative proportions of AC and SCC in males have varied over time. We should also consider whether
#' it's sensible to assume the distribution of AC and SCC in oesophageal cancer will be the same for deaths as it is for cases.
#' Coupland et al show that 1yr and 5yr overall survival is significantly lower in SCC (1yr 30.3\%, 5yr 8.3\%) than in AC (5yr 36.4%, 5yr 9.4%) –
#' though the absolute differences are quite small. So arguably there would be a slightly higher ratio of SCC:AC in deaths than in cases.
#' Arnold et al also provide splits of Oesophageal cancer by subtype for Northern and Western Europe (this might be the approach used in the GBD).
#'
#' @docType data
#'
#' @format A data table
#'
#' @source Percentages supplied by CRUK
#'
"oesoph_splits"
