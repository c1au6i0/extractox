# Download and Extract Data from CompTox Chemistry Dashboard

This function interacts with the CompTox Chemistry Dashboard to download
and extract a wide range of chemical data based on user-defined search
criteria. It allows for flexible input types and supports downloading
various chemical properties, identifiers, and predictive data. It was
inspired by the `ECOTOXr::websearch_comptox` function.

## Usage

``` r
extr_comptox(
  ids,
  download_items = c("CASRN", "INCHIKEY", "IUPAC_NAME", "SMILES", "INCHI_STRING",
    "MS_READY_SMILES", "QSAR_READY_SMILES", "MOLECULAR_FORMULA", "AVERAGE_MASS",
    "MONOISOTOPIC_MASS", "QC_LEVEL", "SAFETY_DATA", "EXPOCAST", "DATA_SOURCES",
    "TOXVAL_DATA", "NUMBER_OF_PUBMED_ARTICLES", "PUBCHEM_DATA_SOURCES", "CPDAT_COUNT",
    "IRIS_LINK", "PPRTV_LINK", "WIKIPEDIA_ARTICLE", "QC_NOTES", "ABSTRACT_SHIFTER",
    "TOXPRINT_FINGERPRINT", "ACTOR_REPORT", "SYNONYM_IDENTIFIER", "RELATED_RELATIONSHIP",
    "ASSOCIATED_TOXCAST_ASSAYS", "TOXVAL_DETAILS", 
     "CHEMICAL_PROPERTIES_DETAILS",
    "BIOCONCENTRATION_FACTOR_TEST_PRED", "BOILING_POINT_DEGC_TEST_PRED",
    "48HR_DAPHNIA_LC50_MOL/L_TEST_PRED", "DENSITY_G/CM^3_TEST_PRED", "DEVTOX_TEST_PRED",
    "96HR_FATHEAD_MINNOW_MOL/L_TEST_PRED", "FLASH_POINT_DEGC_TEST_PRED",
    "MELTING_POINT_DEGC_TEST_PRED", "AMES_MUTAGENICITY_TEST_PRED",
    "ORAL_RAT_LD50_MOL/KG_TEST_PRED", "SURFACE_TENSION_DYN/CM_TEST_PRED",
    "THERMAL_CONDUCTIVITY_MW/(M*K)_TEST_PRED",
    "TETRAHYMENA_PYRIFORMIS_IGC50_MOL/L_TEST_PRED", "VISCOSITY_CP_CP_TEST_PRED", 
    
    "VAPOR_PRESSURE_MMHG_TEST_PRED", "WATER_SOLUBILITY_MOL/L_TEST_PRED",
    "ATMOSPHERIC_HYDROXYLATION_RATE_(AOH)_CM3/MOLECULE*SEC_OPERA_PRED",
    "BIOCONCENTRATION_FACTOR_OPERA_PRED",
    "BIODEGRADATION_HALF_LIFE_DAYS_DAYS_OPERA_PRED", "BOILING_POINT_DEGC_OPERA_PRED",
    "HENRYS_LAW_ATM-M3/MOLE_OPERA_PRED", "OPERA_KM_DAYS_OPERA_PRED",
    "OCTANOL_AIR_PARTITION_COEFF_LOGKOA_OPERA_PRED",
    "SOIL_ADSORPTION_COEFFICIENT_KOC_L/KG_OPERA_PRED",
    "OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED", "MELTING_POINT_DEGC_OPERA_PRED", 
    
    "OPERA_PKAA_OPERA_PRED", "OPERA_PKAB_OPERA_PRED", "VAPOR_PRESSURE_MMHG_OPERA_PRED",
    "WATER_SOLUBILITY_MOL/L_OPERA_PRED",
    "EXPOCAST_MEDIAN_EXPOSURE_PREDICTION_MG/KG-BW/DAY", "NHANES",
    "TOXCAST_NUMBER_OF_ASSAYS/TOTAL", "TOXCAST_PERCENT_ACTIVE"),
  mass_error = 0,
  verify_ssl = FALSE,
  verbose = TRUE,
  delay = 7,
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
  data, and other relevant information. By Default, it downloads all the
  info.

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

  :   Predicted \# nolint atmospheric hydroxylation rate in
      cm³/molecule\\sec from OPERA.

  BIOCONCENTRATION_FACTOR_OPERA_PRED

  :   Predicted bioconcentration factor from OPERA.

  BIODEGRADATION_HALF_LIFE_DAYS_DAYS_OPERA_PRED

  :   Predicted biodegradation \# nolint half-life in days from OPERA.

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

  :   Predicted water solubility in mol/L \# nolint from OPERA.

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
  involving mass data. Default is `0`. Not used if libcurl depends on
  OpenSSL.

- verify_ssl:

  Logical value indicating whether SSL certificates should be verified.
  Default is `FALSE`. Not used if libcurl depends on OpenSSL.

- verbose:

  A logical value indicating whether to print detailed messages. Default
  is TRUE.

- delay:

  Number of seconds to delay between the initial request and the
  subsequent request to download the Excel file.

- ...:

  Additional arguments passed to
  [`httr2::req_options()`](https://httr2.r-lib.org/reference/req_options.html).
  Not used if libcurl depends on OpenSSL.

## Value

A cleaned data frame containing the requested data from CompTox.

## Details

This function is designed to handle potential connection issues with EPA
servers on Linux systems. These servers may not support modern security
protocols (unsafe legacy renegotiation), causing errors with newer
versions of `libcurl` when linked with `OpenSSL`. To ensure reliability,
the function automatically detects if your system's `libcurl` is likely
to be affected. If so, it uses the `{condathis}` package to download and
run the request with a known-compatible version of `curl` (`7.78.0`).

## See also

[CompTox \# nolint Chemicals Dashboard Resource
Hub](https://www.epa.gov/comptox-tools/comptox-chemicals-dashboard-resource-hub)

## Examples

``` r
# \donttest{
# Example usage of the function:
extr_comptox(ids = c("Aspirin", "50-00-0"))
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to CompTox...
#> ℹ Getting info from CompTox...
#> $comptox_cover_sheet
#> # A tibble: 4 × 2
#>   search_datestamp  x2026_04_05_17_10_27
#>   <chr>                            <dbl>
#> 1 Search term count                    2
#> 2 Found count                          2
#> 3 Not found count                      0
#> 4 Duplicate count                      0
#> 
#> $comptox_main_data
#> # A tibble: 2 × 63
#>   input   found_by dtxsid preferred_name dtxcid casrn inchikey iupac_name smiles
#>   <chr>   <chr>    <chr>  <chr>          <chr>  <chr> <chr>    <chr>      <chr> 
#> 1 Aspirin Approve… DTXSI… Aspirin        DTXCI… 50-7… BSYNRYM… 2-(Acetyl… CC(=O…
#> 2 50-00-0 CASRN    DTXSI… Formaldehyde   DTXCI… 50-0… WSFSSNU… Formaldeh… C=O   
#> # ℹ 54 more variables: inchi_string <chr>, ms_ready_smiles <chr>,
#> #   qsar_ready_smiles <chr>, molecular_formula <chr>, average_mass <dbl>,
#> #   monoisotopic_mass <dbl>, qc_level <dbl>, safety_data <lgl>, expocast <chr>,
#> #   data_sources <dbl>, toxval_data <chr>, number_of_pubmed_articles <chr>,
#> #   pubchem_data_sources <chr>, cpdat_count <chr>, iris_link <chr>,
#> #   pprtv_link <lgl>, wikipedia_article <chr>, qc_notes <chr>,
#> #   toxprint_fingerprint <chr>, actor_report <chr>, …
#> 
#> $comptox_abstract_sifter
#> # A tibble: 2 × 3
#>   dsstox_link_to_dashboard preferred_name chemical_entity_query  
#>   <chr>                    <chr>          <chr>                  
#> 1 DTXSID5020108            Aspirin        50-78-2 OR Aspirin     
#> 2 DTXSID7020637            Formaldehyde   50-00-0 OR Formaldehyde
#> 
#> $comptox_synonym_identifier
#> # A tibble: 2 × 3
#>   searched_chemical identifier                                          pc_codes
#>   <chr>             <chr>                                               <chr>   
#> 1 Aspirin           50-78-2|Aspirin|2-(ACETYLOXYBENZOIC) ACID|2-(Acety… PC-1290…
#> 2 Formaldehyde      50-00-0|Formaldehyde|Fannoform|Floguard 1015|Forma… PC-0430…
#> 
#> $comptox_related_relationships
#> # A tibble: 277 × 7
#>    input   dtxsid        preferred_name has_relationship_with  related_dtxsid 
#>    <chr>   <chr>         <chr>          <chr>                  <chr>          
#>  1 Aspirin DTXSID5020108 Aspirin        Searched Chemical      DTXSID5020108  
#>  2 Aspirin DTXSID5020108 Aspirin        Transformation Product DTXSID5021708  
#>  3 Aspirin DTXSID5020108 Aspirin        Predecessor: Component DTXSID0020109  
#>  4 Aspirin DTXSID5020108 Aspirin        Predecessor: Component DTXSID701336718
#>  5 Aspirin DTXSID5020108 Aspirin        Predecessor: Component DTXSID401392325
#>  6 Aspirin DTXSID5020108 Aspirin        Predecessor: Component DTXSID101392326
#>  7 Aspirin DTXSID5020108 Aspirin        Predecessor: Component DTXSID701413792
#>  8 50-00-0 DTXSID7020637 Formaldehyde   Searched Chemical      DTXSID7020637  
#>  9 50-00-0 DTXSID7020637 Formaldehyde   Transformation Product DTXSID6029757  
#> 10 50-00-0 DTXSID7020637 Formaldehyde   Transformation Parent  DTXSID401386683
#> # ℹ 267 more rows
#> # ℹ 2 more variables: related_preferred_name <chr>, related_casrn <chr>
#> 
#> $comptox_toxcast_assays_ac50
#> # A tibble: 1,485 × 3
#>    input                            x50_00_0_dtxsid7020637 aspirin_dtxsid5020108
#>    <chr>                            <chr>                  <chr>                
#>  1 ACEA_AR_agonist_80hr             -                      1000000.0            
#>  2 ACEA_AR_agonist_AUC_viability    -                      1000000.0            
#>  3 ACEA_AR_antagonist_80hr          -                      1000000.0            
#>  4 ACEA_AR_antagonist_AUC_viability -                      1000000.0            
#>  5 ACEA_ER_80hr                     -                      1000000.0            
#>  6 ACEA_ER_AUC_viability            -                      1000000.0            
#>  7 APR_HepG2_CellCycleArrest_1hr    -                      -                    
#>  8 APR_HepG2_CellCycleArrest_24hr   -                      1000000.0            
#>  9 APR_HepG2_CellCycleArrest_72hr   -                      1000000.0            
#> 10 APR_HepG2_CellLoss_1hr           -                      -                    
#> # ℹ 1,475 more rows
#> 
#> $comptox_toxval_details
#> # A tibble: 264 × 63
#>    searched_chemical dtxsid        casrn   name    source sub_source toxval_type
#>    <chr>             <chr>         <chr>   <chr>   <chr>  <chr>      <chr>      
#>  1 Aspirin           DTXSID5020108 50-78-2 Aspirin NLM C… -          LD50       
#>  2 Aspirin           DTXSID5020108 50-78-2 Aspirin DOD M… TLVadj     MEG        
#>  3 Aspirin           DTXSID5020108 50-78-2 Aspirin ECHA … Toxicity … LEL        
#>  4 Aspirin           DTXSID5020108 50-78-2 Aspirin EPA E… EPA ORD    LOEL       
#>  5 Aspirin           DTXSID5020108 50-78-2 Aspirin ECHA … Developme… NOAEL      
#>  6 Aspirin           DTXSID5020108 50-78-2 Aspirin NLM C… -          LD50       
#>  7 Aspirin           DTXSID5020108 50-78-2 Aspirin EPA E… EPA ORD    NOEL       
#>  8 Aspirin           DTXSID5020108 50-78-2 Aspirin EPA E… EPA ORD    NOEL       
#>  9 Aspirin           DTXSID5020108 50-78-2 Aspirin ECHA … Developme… NOAEL      
#> 10 Aspirin           DTXSID5020108 50-78-2 Aspirin ECHA … Developme… NOAEL      
#> # ℹ 254 more rows
#> # ℹ 56 more variables: toxval_subtype <chr>, toxval_type_supercategory <chr>,
#> #   qualifier <chr>, toxval_numeric <dbl>, toxval_units <chr>,
#> #   risk_assessment_class <chr>, study_type <chr>, study_duration_class <chr>,
#> #   study_duration_value <dbl>, study_duration_units <chr>,
#> #   species_common <chr>, strain <chr>, latin_name <chr>,
#> #   species_supercategory <chr>, sex <chr>, generation <chr>, …
#> 
#> $comptox_chemical_properties
#> # A tibble: 83 × 8
#>    dtxsid        dtxcid      type      name       value units source description
#>    <chr>         <chr>       <chr>     <chr>      <chr> <chr> <chr>  <chr>      
#>  1 DTXSID5020108 DTXCID50108 predicted pKa Acidi… 4.2   Log1… OPERA… "<a href=\…
#>  2 DTXSID5020108 DTXCID50108 predicted Boiling P… 283.0 °C    OPERA… "<a href=\…
#>  3 DTXSID5020108 DTXCID50108 predicted Boiling P… 281.… °C    TEST5… "<a href=\…
#>  4 DTXSID5020108 DTXCID50108 predicted Density    1.33… g/cm… TEST5… "<a href=\…
#>  5 DTXSID5020108 DTXCID50108 predicted Flash Poi… 152.… °C    TEST5… "<a href=\…
#>  6 DTXSID5020108 DTXCID50108 predicted Henry's L… 8.31… atm-… OPERA… "<a href=\…
#>  7 DTXSID5020108 DTXCID50108 predicted LogD5.5    -0.13 Log1… OPERA… "<a href=\…
#>  8 DTXSID5020108 DTXCID50108 predicted LogD7.4    -2.01 Log1… OPERA… "<a href=\…
#>  9 DTXSID5020108 DTXCID50108 predicted LogKoa: O… 7.75  Log1… OPERA… "<a href=\…
#> 10 DTXSID5020108 DTXCID50108 predicted LogKow: O… 1.19  Log1… OPERA… "<a href=\…
#> # ℹ 73 more rows
#> 
# }
```
