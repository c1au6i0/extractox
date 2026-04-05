# Download and Extract Data from CompTox Chemistry Dashboard

This function interacts with the CompTox Chemistry Dashboard to download
and extract a wide range of chemical data based on user-defined search
criteria. It allows for flexible input types and supports downloading
various chemical properties, identifiers, and predictive data. It was
inspired by the `ECOTOXr::websearch_comptox` function.

## Usage

``` r
extr_comptox_(
  ids,
  download_items = NULL,
  mass_error = 0,
  verify_ssl = FALSE,
  xlsx_file,
  base_url,
  ...
)
```

## Arguments

- ids:

  A character vector containing the items to be searched within the
  CompTox Chemistry Dashboard. These can be chemical names, CAS Registry
  Numbers (CASRN), InChIKeys, or DSSTox substance identifiers (DTXSID).

- download_items:

  A character vector of items to be downloaded. This includes a
  comprehensive set of chemical properties, identifiers, predictive
  data, and other relevant information. By Default, it download all the
  info

  DTXCID

  :   The unique identifier for a chemical in the EPA's CompTox
      Chemicals Dashboard.

  CASRN

  :   The Chemical Abstracts Service Registry Number, a unique numerical
      identifier for chemical substances.

  INCHIKEY

  :   The hashed version of the full International Chemical Identifier
      (InChI) string.

  IUPAC_NAME

  :   The International Union of Pure and Applied Chemistry (IUPAC) name
      of the chemical.

  SMILES

  :   The Simplified Molecular Input Line Entry System (SMILES)
      representation of the chemical structure.

  INCHI_STRING

  :   The full International Chemical Identifier (InChI) string.

  MS_READY_SMILES

  :   The SMILES representation of the chemical structure, prepared for
      mass spectrometry analysis.

  QSAR_READY_SMILES

  :   The SMILES representation of the chemical structure, prepared for
      quantitative structure-activity relationship (QSAR) modeling.

  MOLECULAR_FORMULA

  :   The chemical formula representing the number and type of atoms in
      a molecule.

  AVERAGE_MASS

  :   The average mass of the molecule, calculated based on the isotopic
      distribution of the elements.

  MONOISOTOPIC_MASS

  :   The mass of the molecule calculated using the most abundant
      isotope of each element.

  QC_LEVEL

  :   The quality control level of the data.

  SAFETY_DATA

  :   Safety information related to the chemical.

  EXPOCAST

  :   Exposure predictions from the EPA's ExpoCast program.

  DATA_SOURCES

  :   Sources of the data provided.

  TOXVAL_DATA

  :   Toxicological values related to the chemical.

  NUMBER_OF_PUBMED_ARTICLES

  :   The number of articles related to the chemical in PubMed.

  PUBCHEM_DATA_SOURCES

  :   Sources of data from PubChem.

  CPDAT_COUNT

  :   The number of entries in the Chemical and Product Categories
      Database (CPDat).

  IRIS_LINK

  :   Link to the EPA's Integrated Risk Information System (IRIS) entry
      for the chemical.

  PPRTV_LINK

  :   Link to the EPA's Provisional Peer-Reviewed Toxicity Values
      (PPRTV) entry for the chemical.

  WIKIPEDIA_ARTICLE

  :   Link to the Wikipedia article for the chemical.

  QC_NOTES

  :   Notes related to the quality control of the data.

  ABSTRACT_SHIFTER

  :   Information related to the abstract shifter.

  TOXPRINT_FINGERPRINT

  :   The ToxPrint chemoinformatics fingerprint of the chemical.

  ACTOR_REPORT

  :   The Aggregated Computational Toxicology Resource (ACTOR) report
      for the chemical.

  SYNONYM_IDENTIFIER

  :   Identifiers for synonyms of the chemical.

  RELATED_RELATIONSHIP

  :   Information on related chemicals.

  ASSOCIATED_TOXCAST_ASSAYS

  :   Assays associated with the chemical in the ToxCast database.

  TOXVAL_DETAILS

  :   Details of toxicological values.

  CHEMICAL_PROPERTIES_DETAILS

  :   Details of the chemical properties.

  BIOCONCENTRATION_FACTOR_TEST_PRED

  :   Predicted bioconcentration factor from tests.

  BOILING_POINT_DEGC_TEST_PRED

  :   Predicted boiling point in degrees Celsius from tests.

  48HR_DAPHNIA_LC50_MOL/L_TEST_PRED

  :   Predicted 48-hour LC50 for Daphnia in mol/L from tests.

  DENSITY_G/CM^3_TEST_PRED

  :   Predicted density in g/cm³ from tests.

  DEVTOX_TEST_PRED

  :   Predicted developmental toxicity from tests.

  96HR_FATHEAD_MINNOW_MOL/L_TEST_PRED

  :   Predicted 96-hour LC50 for fathead minnow in mol/L from tests.

  FLASH_POINT_DEGC_TEST_PRED

  :   Predicted flash point in degrees Celsius from tests.

  MELTING_POINT_DEGC_TEST_PRED

  :   Predicted melting point in degrees Celsius from tests.

  AMES_MUTAGENICITY_TEST_PRED

  :   Predicted Ames mutagenicity from tests.

  ORAL_RAT_LD50_MOL/KG_TEST_PRED

  :   Predicted oral LD50 for rats in mol/kg from tests.

  SURFACE_TENSION_DYN/CM_TEST_PRED

  :   Predicted surface tension in dyn/cm from tests.

  THERMAL_CONDUCTIVITY_MW_M×K_TEST_PRED

  :   Predicted thermal conductivity in mW/m×K from tests.

  TETRAHYMENA_PYRIFORMIS_IGC50_MOL/L_TEST_PRED

  :   Predicted IGC50 for Tetrahymena pyriformis in mol/L from tests.

  VISCOSITY_CP_CP_TEST_PRED

  :   Predicted viscosity in cP from tests.

  VAPOR_PRESSURE_MMHG_TEST_PRED

  :   Predicted vapor pressure in mmHg from tests.

  WATER_SOLUBILITY_MOL/L_TEST_PRED

  :   Predicted water solubility in mol/L from tests.

  ATMOSPHERIC_HYDROXYLATION_RATE\_\\AOH\\\_CM3/MOLECULE\\SEC_OPERA_PRED

  :   Predicted atmospheric hydroxylation rate in cm³/molecule\\sec from
      OPERA.

  BIOCONCENTRATION_FACTOR_OPERA_PRED

  :   Predicted bioconcentration factor from OPERA.

  BIODEGRADATION_HALF_LIFE_DAYS_DAYS_OPERA_PRED

  :   Predicted biodegradation half-life in days from OPERA.

  BOILING_POINT_DEGC_OPERA_PRED

  :   Predicted boiling point in degrees Celsius from OPERA.

  HENRYS_LAW_ATM-M3/MOLE_OPERA_PRED

  :   Predicted Henry's law constant in atm-m³/mole from OPERA.

  OPERA_KM_DAYS_OPERA_PRED

  :   Predicted Km in days from OPERA.

  OCTANOL_AIR_PARTITION_COEFF_LOGKOA_OPERA_PRED

  :   Predicted octanol-air partition coefficient (log Koa) from OPERA.

  SOIL_ADSORPTION_COEFFICIENT_KOC_L/KG_OPERA_PRED

  :   Predicted soil adsorption coefficient (Koc) in L/kg from OPERA.

  OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED

  :   Predicted octanol-water partition coefficient (log P) from OPERA.

  MELTING_POINT_DEGC_OPERA_PRED

  :   Predicted melting point in degrees Celsius from OPERA.

  OPERA_PKAA_OPERA_PRED

  :   Predicted pKa (acidic) from OPERA.

  OPERA_PKAB_OPERA_PRED

  :   Predicted pKa (basic) from OPERA.

  VAPOR_PRESSURE_MMHG_OPERA_PRED

  :   Predicted vapor pressure in mmHg from OPERA.

  WATER_SOLUBILITY_MOL/L_OPERA_PRED

  :   Predicted water solubility in mol/L from OPERA.

  EXPOCAST_MEDIAN_EXPOSURE_PREDICTION_MG/KG-BW/DAY

  :   Predicted median exposure from ExpoCast in mg/kg-bw/day.

  NHANES

  :   National Health and Nutrition Examination Survey data.

  TOXCAST_NUMBER_OF_ASSAYS/TOTAL

  :   Number of assays in ToxCast.

  TOXCAST_PERCENT_ACTIVE

  :   Percentage of active assays in ToxCast.

- mass_error:

  Numeric value indicating the mass error tolerance for searches
  involving mass data. Default is `0`.

- verify_ssl:

  Logical value indicating whether SSL certificates should be verified.
  Default is `FALSE`. Note that this argument is not used on linux OS.

- xlsx_file:

  Path to file to write with results.

- base_url:

  Comptox url.

- ...:

  Additional arguments passed to
  [`httr2::req_options()`](https://httr2.r-lib.org/reference/req_options.html).
  Note that this argument is not used on linux OS.
