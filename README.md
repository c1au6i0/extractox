
<!-- README.md is generated from README.Rmd. Please edit that file -->

# extractox <img src='man/figures/extractox.png' align='right' height="139px" alt='logo'  style="float:right; height:139px;">

<!-- badges: start -->

[![R-CMD-check](https://github.com/c1au6i0/extractox/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/c1au6i0/extractox/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/extractox)](https://CRAN.R-project.org/package=extractox)
[![DEV
version](https://img.shields.io/badge/devel%20version-0.1.9003-blue.svg)](https://github.com/c1au6i0/extractox)
<!-- badges: end -->

`extractox` is a comprehensive R package designed to simplify querying
various chemical, toxicological, and biological databases such as the
Comparative Toxicogenomics Database (CTP) of the MDI Biological
Laboratory and NC State University, the Integrated Chemical Environment
(ICE) of the National Toxicology Program (NTP), the Integrated Risk
Information System (IRIS) of the Environmental Protection Agency (EPA),
the CompTox Chemicals Dashboard Resource Hub (CompTox) of EPA, and
PubChem of the National Center for Biotechnology Information/National
Institures Of Health (NCBI, NIH). The package facilitates interaction
with APIs, providing curated and user-friendly outputs.

To communicate with Pubchem, `extractox` relies on the package`webchem`.

## Installation

Install the package `extractox` from CRAN.

``` r
# From CRAN
install.packages("extractox")
```

### Development Version

To get a bug fix or to use a feature from the development version, you
can install the development version of `extractox` from GitHub.

``` r
# Install pak if not already installed
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

# Install the package from GitHub
pak::pkg_install("c1au6i0/extractox")
```

## Features

### ICE Database

The ICE database, managed by the EPA, provides access to a variety of
data related to chemical toxicity, exposure, and risk assessment. It
includes data from high-throughput screening assays, in vivo studies,
and computational models.

`extr_ice` provides access to NTP’s ICE database for toxicological and
exposure-related data.

``` r
library(extractox)
ice_data <- extr_ice(casrn = c("50-00-0"), assays = NULL) # assays is null so all assays are retrieved
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to ICE database...
#> ℹ Request succeeded with status code: 200
names(ice_data)
#>  [1] "assay"                 "endpoint"              "substance_type"       
#>  [4] "casrn"                 "qsar_ready_id"         "value"                
#>  [7] "unit"                  "species"               "receptor_species"     
#> [10] "route"                 "sex"                   "strain"               
#> [13] "life_stage"            "tissue"                "lesion"               
#> [16] "location"              "assay_source"          "in_vitro_assay_format"
#> [19] "reference"             "reference_url"         "dtxsid"               
#> [22] "substance_name"        "pubmed_id"             "query"
```

There are more than **2000 possible assays** in ICE. The
`extr_ice_assay_names()` function allows to search for assay names that
match a pattern you’re interested in. Please note that searches are case
sensitive and accept regexp.

``` r
extr_ice_assay_names("Rat Acute") # keep empty to retrieve all
#> [1] "Rat Acute Oral Toxicity"         "Rat Acute Inhalation Toxicity"  
#> [3] "Rat Acute Dermal Toxicity"       "CATMoS, Rat Acute Oral Toxicity"
```

### EPA IRIS

The IRIS database is managed by the EPA and contains information on the
health effects of exposure to various substances found in the
environment. It provides qualitative and quantitative health risk
information.

`extr_iris` provides access to EPA’s IRIS database and accepts queries
CASRN or IUPAC names of chemicals.

``` r
iris_info <- extr_iris(c("glyphosate", "50-00-0"))
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Quering glyphosate to EPA IRIS database...
#> ℹ Request succeeded with status code: 200
#> ℹ Quering 50-00-0 to EPA IRIS database...
#> ℹ Request succeeded with status code: 200
#> Warning: Chemical glyphosate not found!
names(iris_info)
#> [1] "chemical_name"                 "casrn"                        
#> [3] "exposure_route"                "assessment_type"              
#> [5] "critical_effect_or_tumor_type" "woe_characterization"         
#> [7] "toxicity_value_type"           "toxicity_value"               
#> [9] "query"
```

### EPA PPRTVs

The `extr_pprtv` function allows you to extract data for specified
identifiers (CASRN or chemical names) from the EPA’s Provisional
Peer-Reviewed Toxicity Values (PPRTVs) database. This function retrieves
and processes data, with options to use cached files or force a fresh
download if necessary.

``` r
# Example usage to extract data for a specific CASRN
extr_pprtv(ids = "107-02-8", search_type = "casrn", verbose = TRUE)
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Downloading data from <https://cfpub.epa.gov/ncea/pprtv/atoz.cfm>.
#> ℹ Overwriting cache.
#> ℹ Extracting EPA PPRTVs.
#>   pprtv_substance_id chemical    casrn last_revision
#> 1               1555 Acrolein 107-02-8          2002
#>                                          pprtv_assessment
#> 1 https://cfpub.epa.gov/ncea/pprtv/documents/Acrolein.pdf
#>                                                                 iris_link
#> 1 https://cfpub.epa.gov/ncea/iris2/chemicalLanding.cfm?substance_nmbr=364
#>   rf_c_value rf_d_value           woe     date_downloaded    query
#> 1   See IRIS   See IRIS Not available 2025-01-01 16:11:39 107-02-8

# Example usage to extract data for a chemical name
extr_pprtv(ids = "Acrolein", search_type = "name", verbose = TRUE, force = FALSE)
#> ✔ Successfully load. 'epa_pprtvs.rds' from cache.
#> ℹ Cache date 2025-01-01 16:11:39.
#> ℹ Set `force = TRUE` to force download from EPA.
#> ℹ Extracting EPA PPRTVs.
#>   pprtv_substance_id chemical    casrn last_revision
#> 1               1555 Acrolein 107-02-8          2002
#>                                          pprtv_assessment
#> 1 https://cfpub.epa.gov/ncea/pprtv/documents/Acrolein.pdf
#>                                                                 iris_link
#> 1 https://cfpub.epa.gov/ncea/iris2/chemicalLanding.cfm?substance_nmbr=364
#>   rf_c_value rf_d_value           woe     date_downloaded    query
#> 1   See IRIS   See IRIS Not available 2025-01-01 16:11:39 Acrolein
```

### CompTox

The CompTox Chemistry Dashboard, managed by the EPA, provides access to
data on chemical structures, properties, and associated bioactivity
data. It integrates data from various sources to support chemical safety
assessments.

`extr_comptox` extracts data from the CompTox Chemistry Dashboard using
both CASRN and IUPAC names of chemicals and returns a list of
**dataframes**.

``` r
info_comptox <- extr_comptox(ids = c("Aspirin", "50-00-0"))
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to CompTox...
#> ℹ Request succeeded with status code: 202
#> ℹ Getting info from CompTox...
#> ℹ Request succeeded with status code: 200
names(info_comptox)
#> [1] "comptox_cover_sheet"           "comptox_main_data"            
#> [3] "comptox_abstract_sifter"       "comptox_synonym_identifier"   
#> [5] "comptox_related_relationships" "comptox_toxcast_assays_ac50"  
#> [7] "comptox_toxval_details"        "comptox_chemical_properties"
```

### WHO IARC

The IARC Monographs database is managed by the World Health Organization
(WHO) International Agency for Research on Cancer (IARC) and contains
evaluations of the carcinogenic risks of various substances to humans.
It provides detailed information about Monographs, including publication
volumes and years, evaluation years, and additional relevant details.

The functione `extr_monograph` provides access to the WHO IARC
Monographs database and accepts queries using CASRN or the names of
chemicals.

``` r
dat <- extr_monograph(search_type = "casrn", ids = c("105-74-8", "120-58-1"))
#> ℹ Extracting WHO IARC monographs...
#> Last updated: 2024-11-29 5:08pm (CET)
str(dat)
#> 'data.frame':    2 obs. of  8 variables:
#>  $ casrn                  : chr  "105-74-8" "120-58-1"
#>  $ agent                  : chr  "Lauroyl peroxide" "Isosafrole"
#>  $ group                  : chr  "3" "3"
#>  $ volume                 : chr  "36, Sup 7, 71" "10, Sup 7"
#>  $ volume_publication_year: chr  "1999" "1987"
#>  $ evaluation_year        : int  1998 1987
#>  $ additional_information : chr  "" ""
#>  $ query                  : chr  "105-74-8" "120-58-1"

# Example usage for name search
dat2 <- extr_monograph(search_type = "name", ids = c("Aloe", "Schistosoma", "Styrene"))
#> ℹ Extracting WHO IARC monographs...
#> Last updated: 2024-11-29 5:08pm (CET)
```

### PubChem Database

PubChem is an open chemistry database at the NIH. It provides
information on chemical structures, identifiers, chemical and physical
properties, biological activities, safety and toxicity information,
patents, literature citations, and more.

A series of functions that rely on the `webchem` package are used to
extract chemical information, Globally Harmonized System (`GHS`)
classification data, or flavor classification from PubChem.

The function `extr_chem_info` retrieves chemical information of
IUPAC-named chemicals. A warning is displayed if the chemical is not
found.

``` r
chem_info <- extr_chem_info(iupac_names = c("Formaldehyde", "Aflatoxin B1"))
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> Querying Formaldehyde. OK (HTTP 200).
#> Querying Aflatoxin B1. OK (HTTP 200).
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Querying pubchem_ids.
names(chem_info)
#>  [1] "cid"                         "iupac_name"                 
#>  [3] "casrn"                       "cid_all"                    
#>  [5] "casrn_all"                   "molecular_formula"          
#>  [7] "molecular_weight"            "canonical_smiles"           
#>  [9] "isomeric_smiles"             "inchi"                      
#> [11] "inchi_key"                   "iupac_name"                 
#> [13] "x_log_p"                     "exact_mass"                 
#> [15] "monoisotopic_mass"           "tpsa"                       
#> [17] "complexity"                  "charge"                     
#> [19] "h_bond_donor_count"          "h_bond_acceptor_count"      
#> [21] "rotatable_bond_count"        "heavy_atom_count"           
#> [23] "isotope_atom_count"          "atom_stereo_count"          
#> [25] "defined_atom_stereo_count"   "undefined_atom_stereo_count"
#> [27] "bond_stereo_count"           "defined_bond_stereo_count"  
#> [29] "undefined_bond_stereo_count" "covalent_unit_count"        
#> [31] "volume3d"                    "x_steric_quadrupole3d"      
#> [33] "y_steric_quadrupole3d"       "z_steric_quadrupole3d"      
#> [35] "feature_count3d"             "feature_acceptor_count3d"   
#> [37] "feature_donor_count3d"       "feature_anion_count3d"      
#> [39] "feature_cation_count3d"      "feature_ring_count3d"       
#> [41] "feature_hydrophobe_count3d"  "conformer_model_rmsd3d"     
#> [43] "effective_rotor_count3d"     "conformer_count3d"          
#> [45] "fingerprint2d"               "query"
```

Two functions are used to extract specific sections of PubChem chemical
information using CASRN:

- `extr_pubchem_ghs` extracts Globally Harmonized System (GHS) codes.
- `extr_pubchem_fema` extracts flavor profile data.

``` r
ghs_info <- extr_pubchem_ghs(casrn = c("50-00-0", "64-17-5"))
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> Querying 50-00-0. OK (HTTP 200).
#> Querying 712. OK (HTTP 200).
#> Querying 64-17-5. OK (HTTP 200).
#> Querying 702. OK (HTTP 200).
fema_info <- extr_pubchem_fema(casrn = c("50-00-0", "123-68-2"))
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> Querying 50-00-0. OK (HTTP 200).
#> Querying 712. Not Found (HTTP 404).
#> Warning: FEMA Flavor Profile for 50-00-0 not found!
#> Querying 123-68-2. OK (HTTP 200).
#> Querying 31266. OK (HTTP 200).
```

### Tox Info

The function `extr_tox` is a wrapper used to call all the
above-mentioned functions and retrieve a list of dataframes.

``` r
info_tox <- extr_tox(casrn = c("Aspirin", "50-00-0"))
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> Querying Aspirin. OK (HTTP 200).
#> Querying 2244. OK (HTTP 200).
#> Querying 50-00-0. OK (HTTP 200).
#> Querying 712. OK (HTTP 200).
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to CompTox...
#> ℹ Request succeeded with status code: 202
#> ℹ Getting info from CompTox...
#> ℹ Request succeeded with status code: 200
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to ICE database...
#> ℹ Request succeeded with status code: 200
#> Warning: Chemical Aspirin not found!
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Quering Aspirin to EPA IRIS database...
#> ℹ Request succeeded with status code: 200
#> ℹ Quering 50-00-0 to EPA IRIS database...
#> ℹ Request succeeded with status code: 200
#> Warning: Chemical Aspirin not found!
#> ℹ Extracting WHO IARC monographs...
#> Last updated: 2024-11-29 5:08pm (CET)
#> Warning: Chemical Aspirin not found!
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Downloading data from <https://cfpub.epa.gov/ncea/pprtv/atoz.cfm>.
#> ℹ Overwriting cache.
#> ℹ Extracting EPA PPRTVs.
#> Warning: Chemicals 50-00-0 and Aspirin not found!
```

### CTD Database

The CTP provides information about the interactions between chemicals,
genes, and diseases. It helps in understanding the effects of
environmental exposures on human health.

A series of functions interact with the CTP database.

`extr_ctd` extracts information related to chemical-gene or pathway
associations.

``` r
input_terms <- c("50-00-0", "64-17-5", "methanal", "ethanol")
ctd_association <- extr_ctd(
  input_terms = input_terms,
  category = "chem",
  report_type = "genes_curated",
  input_term_search_type = "directAssociations",
  action_types = "ANY",
  ontology = c("go_bp", "go_cc")
)
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to CTD database...
#> ℹ Request succeeded with status code: 200


# Get expresssion data
ctd_expression <- extr_ctd(
  input_terms = input_terms,
  report_type = "cgixns",
  category = "chem",
  action_types = "expression"
)
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to CTD database...
#> ℹ Request succeeded with status code: 200

names(ctd_expression)
#>  [1] "chemical_name" "chemical_id"   "casrn"         "gene_symbol"  
#>  [5] "gene_id"       "organism"      "organism_id"   "pubmed_ids"   
#>  [9] "query"         NA              NA
```

Tetramers are computationally generated information units that
interrelate four data types from the CTP: a chemical, gene product,
phenotype, and disease. They help in understanding the complex
relationships between these entities and their combined impact on human
health.

`extr_tetramer` extract info related to tetramers from CTD.

``` r
tetramer_data <- extr_tetramer(
  chem = c("50-00-0", "ethanol"),
  disease = "",
  gene = "",
  go = "",
  input_term_search_type = "directAssociations",
  qt_match_type = "equals"
)
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to CTD database for tetramer data for
#> 50-00-0...
#> ℹ Request succeeded with status code: 200
#> ℹ Sending request to CTD database for tetramer data for
#> ethanol...
#> ℹ Request succeeded with status code: 200

names(tetramer_data)
#> [1] "chemical"     "chemical_id"  "gene"         "gene_id"      "phenotype"   
#> [6] "phenotype_id" "disease"      "disease_id"   "query"
```

## Important Note for Linux Users

Please note that functions that pull data from EPA servers may encounter
issues on some Linux systems. This is because these servers do not
accept secure legacy renegotiation. On Linux system, those functions
depend on `curl` and `OpenSSL`, which have known problems with unsafe
legacy renegotiation in newer versions. One workaround is to downgrade
to `curl v7.78.0` and `OpenSSL v1.1.1`. However, please be aware that
using these older versions might introduce potential security
vulnerabilities. Refer to [this
gist](https://gist.github.com/c1au6i0/5cc2d87966340a31032ffebf1cfb657c)
for instructions on how to downgrade `curl` and `OpenSSL` on Ubuntu.
