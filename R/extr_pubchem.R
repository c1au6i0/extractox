#' Generate a dataframe with Specified Columns and NAs
#'
#' Used internally to handle no results queries of extr_casrn_from_cid
#'
#' @param missing_chem Vector of missing chemical names.
#'
#' @return A dataframe with the specified columns and all NAs.
#' @keywords internal
#' @noRd
create_na_df <- function(missing_chem) {
  column_names <- c("cid", "iupac_name", "cas_rn", "cid_all", "cas_rn_all",
               "molecular_formula", "molecular_weight", "canonical_smiles",
               "isomeric_smiles", "in_ch_i", "in_ch_i_key", "iupac_name_y",
               "x_log_p", "exact_mass", "monoisotopic_mass", "tpsa",
               "complexity", "charge", "h_bond_donor_count",
               "h_bond_acceptor_count", "rotatable_bond_count",
               "heavy_atom_count", "isotope_atom_count", "atom_stereo_count",
               "defined_atom_stereo_count", "undefined_atom_stereo_count",
               "bond_stereo_count", "defined_bond_stereo_count",
               "undefined_bond_stereo_count", "covalent_unit_count",
               "volume3d", "x_steric_quadrupole3d", "y_steric_quadrupole3d",
               "z_steric_quadrupole3d", "feature_count3d",
               "feature_acceptor_count3d", "feature_donor_count3d",
               "feature_anion_count3d", "feature_cation_count3d",
               "feature_ring_count3d", "feature_hydrophobe_count3d",
               "conformer_model_rmsd3d", "effective_rotor_count3d",
               "conformer_count3d", "fingerprint2d", "query")

  # Create the dataframe with all NAs
  out <- data.frame(matrix(NA, nrow = length(missing_chem), ncol = length(column_names)))
  names(out) <- column_names

  out$query <- missing_chem

  out
}

#' Retrieve CASRN for PubChem CIDs
#'
#' This function retrieves the CASRN for a given set of PubChem Compound Identifiers (CID).
#' It queries PubChem through the `webchem` package and extracts the CASRN from the depositor-supplied synonyms.
#'
#' @param pubchem_id A numeric vector of PubChem CIDs. These are unique identifiers
#' for chemical compounds in the PubChem database.
#' @param verbose A logical value indicating whether to print detailed messages. Default is TRUE.
#' @return A data frame containing the CID, CASRN, and IUPAC name of the compound.
#' The returned data frame includes three columns:
#' \describe{
#'   \item{CID}{The PubChem Compound Identifier.}
#'   \item{cas_rn}{The corresponding CASRN of the compound.}
#'   \item{IUPACName}{The IUPAC name of the compound.}
#' }
#' @seealso \href{https://pubchem.ncbi.nlm.nih.gov/}{PubChem}
#' @export
#' @examples
#' \donttest{
#' # Example with formaldehyde and aflatoxin
#' cids <- c(712, 14434) # CID for formaldehyde and aflatoxin B1
#' extr_casrn_from_cid(cids)
#' }
extr_casrn_from_cid <- function(pubchem_id, verbose = TRUE) {

  if (missing(pubchem_id)) {
    cli::cli_abort("The argument {.field {pubchem_id}} is required.")
  }
  check_internet(verbose = verbose)

  if (isTRUE(verbose)) {
    cli::cli_alert_info("Querying {pubchem_id}.")
  }

  pubchem_data <- webchem::pc_sect(pubchem_id, "Depositor-Supplied Synonyms")

  cas_rn_data <- pubchem_data[grep("^\\d{2,7}-\\d+-\\d$", pubchem_data$Result), ]
  colnames(cas_rn_data)[colnames(cas_rn_data) == "Result"] <- "cas_rn"
  colnames(cas_rn_data)[colnames(cas_rn_data) == "Name"] <- "IUPACName"
  colnames(cas_rn_data)[colnames(cas_rn_data) == "CID"] <- "cid"

  cas_rn_data <- cas_rn_data[c("cid", "cas_rn", "IUPACName")]
  cas_rn_data
}


#' Query Chemical Information from IUPAC Names
#'
#' This function takes a vector of IUPAC names and queries the PubChem database
#' (using the `webchem` package) to obtain the corresponding CASRN and CID for
#' each compound. It reshapes the resulting data, ensuring that each compound has
#' a unique row with the CID, CASRN, and additional chemical properties.
#'
#' @param IUPAC_names A character vector of IUPAC names. These are standardized names
#' of chemical compounds that will be used to search in the PubChem database.
#' @param verbose A logical value indicating whether to print detailed messages. Default is TRUE.
#' @return A data frame with information on the queried compounds, including:
#' \describe{
#'   \item{iupac_name}{The IUPAC name of the compound.}
#'   \item{cid}{The PubChem Compound Identifier (CID).}
#'   \item{isomeric_smiles}{The SMILES string (Simplified Molecular Input Line Entry System).}
#' }
#' @export
#' @examples
#' \donttest{
#' # Example with formaldehyde and aflatoxin
#' extr_chem_info(IUPAC_names = c("Formaldehyde", "Aflatoxin B1"))
#' }
extr_chem_info <- function(IUPAC_names, verbose = TRUE) {

  if (missing(IUPAC_names)) {
    cli::cli_abort("The argument {.field {IUPAC_names}} is required.")
  }

  check_internet(verbose = verbose)

  iupac_cid <- webchem::get_cid(IUPAC_names, domain = "compound", verbose = verbose)


  # Rename and mutate using base R
  iupac_cid$constituent <- iupac_cid$query
  iupac_cid$cid <- as.numeric(iupac_cid$cid)

  # Handle missing CIDs
  if (any(is.na(iupac_cid$cid))) {
    missing_c <- iupac_cid$constituent[is.na(iupac_cid$cid)]

  if (isTRUE(verbose)){
        cli::cli_warn(paste0("CID not retrieved for", paste(missing_c, collapse = ", "), "!"))
  }

  missing_df <- create_na_df(missing_c)
  }


  iupac_cid_clean <- iupac_cid[!is.na(iupac_cid$cid), ]

  # Get CAS from PubChem
  cid_cas <- extr_casrn_from_cid(iupac_cid_clean$cid, verbose = verbose)

  # Ensure unique rows and summarize using base R
  iupac_cid_cas_unique <- stats::aggregate(cbind(cid, cas_rn) ~ IUPACName, data = cid_cas, function(x) list(unique(x)))
  iupac_cid_cas_unique$cid <- sapply(iupac_cid_cas_unique$cid, function(x) x[!is.na(x)][1])
  iupac_cid_cas_unique$cas_rn <- sapply(iupac_cid_cas_unique$cas_rn, function(x) x[!is.na(x)][1])
  iupac_cid_cas_unique$cid_all <- sapply(iupac_cid_cas_unique$cid, paste, collapse = ", ")
  iupac_cid_cas_unique$cas_rn_all <- sapply(iupac_cid_cas_unique$cas_rn, paste, collapse = ", ")

  all_prop <- webchem::pc_prop(iupac_cid_cas_unique$cid)
  all_prop$CID <- as.numeric(all_prop$CID)

  # Clean and join data
  out <- merge(iupac_cid_cas_unique, all_prop, by.x = "cid", by.y = "CID", all.x = TRUE)
  out <- merge(out, iupac_cid, by.x = "cid", by.y = "cid", all.x = TRUE) |>
    janitor::clean_names()

  colnames(out)[colnames(out) == "iupac_name_x"] <- "iupac_name"
  colnames(out)[colnames(out) == "query_x"] <- "query"
  out_clean <- out[, !colnames(out) %in% c("constituent_x", "query_y", "constituent_y", "constituent")]

  if (any(is.na(iupac_cid$cid))) {
    out_clean <- rbind(missing_df, out_clean)
  }

  out_clean[match(IUPAC_names, out_clean$query),  ]
}


#' Extract FEMA from PubChem
#'
#' This function retrieves FEMA (Flavor and Extract Manufacturers Association)
#'   flavor profile information for a list of CAS Registry Numbers (CASRN) from
#'   the PubChem database using the `webchem` package.
#' @param casrn A vector of CAS Registry Numbers (CASRN) as atomic vectors.
#' @param verbose A logical value indicating whether to print detailed messages.
#'    Default is TRUE.
#' @return A data frame containing the FEMA flavor profile information for each
#'   CASRN. If no information is found for a particular CASRN, the output will include a row indicating this.
#' @seealso \href{https://pubchem.ncbi.nlm.nih.gov/}{PubChem}
#' @export
#' @examples
#' \donttest{
#' extr_pubchem_fema(c("83-67-0", "1490-04-6"))
#' }
extr_pubchem_fema <- function(casrn, verbose = TRUE) {

  extr_pubchem_section(casrn, section = "FEMA Flavor Profile", verbose = verbose)
}


#' Extract GHS Codes from PubChem
#'
#' This function extracts GHS (Globally Harmonized System) codes from PubChem. It relies on the `webchem` package to interact with PubChem.
#'
#' @param casrn Character vector of CAS Registry Numbers (CASRN).
#' @param verbose A logical value indicating whether to print detailed messages. Default is TRUE.
#' @return A dataframe containing GHS information.
#' @seealso \href{https://pubchem.ncbi.nlm.nih.gov/}{PubChem}
#' @export
#' @examples
#' \donttest{
#' extr_pubchem_ghs(casrn = c("50-00-0", "64-17-5"))
#' }
extr_pubchem_ghs <- function(casrn, verbose = TRUE) {
  extr_pubchem_section(casrn, section = "GHS Classification", verbose = verbose)
}


#' Extract PubChem Section Data
#'
#' A generalized function to extract specific section data (e.g., FEMA or GHS) from PubChem for a given CASRN.
#'
#' @param casrn A character vector of CAS Registry Numbers (CASRN).
#' @param section A character string specifying the PubChem section to query (e.g., "FEMA Flavor Profile" or "GHS Classification").
#' @param verbose A logical value indicating whether to print detailed messages. Default is TRUE.
#' @return A dataframe containing the queried section information for each CASRN.
#' @keywords internal
#' @noRd
extr_pubchem_section <- function(casrn, section, verbose = TRUE) {
  if (missing(casrn)) {
    cli::cli_abort("The argument {.field {casrn}} is required.")
  }

  check_internet(verbose = verbose)

  dat <- lapply(casrn, function(cas) {
    extr_pubchem_section_(cas, section, verbose)
  })

  do.call(rbind, dat)
}

#' Internal Helper Function for `extr_pubchem_section`
#'
#' @param casrn A single CASRN.
#' @param section A character string specifying the PubChem section to query.
#' @param verbose A logical value indicating whether to print detailed messages. Default is TRUE.
#' @noRd
#' @keywords internal
extr_pubchem_section_ <- function(casrn, section, verbose = TRUE) {

  dat_cid <- webchem::get_cid(casrn, match = "first", verbose = verbose)

  col_out <- c(
    "cid",
    "casrn",
    "IUPAC_name",
    "result",
    "source_name",
    "source_id",
    "other",
    "query"
  )

  # Handle no CID retrieved
  if (is.na(dat_cid$cid)) {

    names_casrn <- webchem::pc_sect(dat_cid$cid, "Depositor-Supplied Synonyms")
    na_matrix <- matrix(NA, nrow = 1, ncol = length(col_out))
    out_df <- as.data.frame(na_matrix)
    colnames(out_df) <- col_out
    out_df$casrn <- NA
    out_df$query <- casrn
    out_df$other <- "CASRN not found"

    if (isTRUE(verbose)) {
      cli::cli_warn("CASRN {.field {casrn}} not found!")
    }
    return(out_df)
  }

  dat_section <- webchem::pc_sect(dat_cid$cid, section = section, verbose = verbose) |>
    janitor::clean_names()


  # Handle empty results for section
  if (ncol(dat_section) == 0) {

    name_casrn <- webchem::pc_sect(dat_cid$cid, "Depositor-Supplied Synonyms")
    na_matrix <- matrix(NA, nrow = 1, ncol = length(col_out))
    out_df <- as.data.frame(na_matrix)
    colnames(out_df) <- col_out
    out_df$IUPAC_name <- name_casrn$Name[[1]]
    out_df$cid <- dat_cid$cid
    out_df$casrn <- casrn
    out_df$query <- casrn
    out_df$other <- paste(section, "info not found")

    if (isTRUE(verbose)) {
      cli::cli_warn("{section} for {.field {casrn}} not found!")
    }
    return(out_df)
  }

  # Clean and format the results
  out_df <- merge(dat_cid, dat_section, by = "cid")
  out_df$result <- gsub(paste0("^", section, ".*"), "", out_df$result)
  out_df[, "other"] <- NA

  names(out_df)[names(out_df) %in% c("query", "name")] <- c("casrn", "IUPAC_name")
  out_df[, "query"] <- casrn
  out_df
}
