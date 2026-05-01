# Query Chemical Information from IUPAC Names

This function takes a vector of IUPAC names and queries the PubChem
database (using the `webchem` package) to obtain the corresponding CASRN
and CID for each compound. It reshapes the resulting data, ensuring that
each compound has a unique row with the CID, CASRN, and additional
chemical properties.

## Usage

``` r
extr_chem_info(iupac_names, verbose = TRUE, domain = "compound", delay = 0)
```

## Arguments

- iupac_names:

  A character vector of IUPAC names. These are standardized names of
  chemical compounds that will be used to search in the PubChem
  database.

- verbose:

  A logical value indicating whether to print detailed messages. Default
  is TRUE.

- domain:

  A character string specifying the PubChem domain to query. One of
  `"compound"` or `substance`. Default is `compound`.

- delay:

  A numeric value indicating the delay (in seconds) between API
  requests. This controls the time between successive PubChem queries.
  Default is 0. See Details for more info.

## Value

A data frame with phisio-chemical information on the queried compounds,
including but not limited to:

- iupac_name:

  The IUPAC name of the compound.

- cid:

  The PubChem Compound Identifier (CID).

- isomeric_smiles:

  The SMILES string (Simplified Molecular Input Line Entry System).

## Details

The function performs two queries to PubChem:

1.  The first query retrieves the PubChem Compound Identifier (CID) for
    each IUPAC name.

2.  The second query retrieves additional information using the obtained
    CIDs. In cases of multiple rapid successive requests, the PubChem
    server may deny access. Introducing a delay between requests (using
    the `delay` parameter) can help prevent this issue.

## Examples

``` r
# \donttest{
# Example with formaldehyde and aflatoxin
extr_chem_info(iupac_names = c("Formaldehyde", "Aflatoxin B1"))
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> Querying Formaldehyde. 
#> OK (HTTP 200).
#> 
#> Querying Aflatoxin B1. 
#> OK (HTTP 200).
#> 
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Querying pubchem_ids.
#>      cid   iupac_name     casrn cid_all casrn_all molecular_formula
#> 1 186907 Aflatoxin B1 1162-65-8  186907 1162-65-8          C17H12O6
#> 2    712 Formaldehyde   50-00-0     712   50-00-0              CH2O
#>   molecular_weight                                                   smiles
#> 1           312.27 COC1=C2C3=C(C(=O)CC3)C(=O)OC2=C4[C@@H]5C=CO[C@@H]5OC4=C1
#> 2           30.026                                                      C=O
#>                              connectivity_smiles
#> 1 COC1=C2C3=C(C(=O)CC3)C(=O)OC2=C4C5C=COC5OC4=C1
#> 2                                            C=O
#>                                                                                                                            inchi
#> 1 InChI=1S/C17H12O6/c1-20-10-6-11-14(8-4-5-21-17(8)22-11)15-13(10)7-2-3-9(18)12(7)16(19)23-15/h4-6,8,17H,2-3H2,1H3/t8-,17+/m0/s1
#> 2                                                                                                        InChI=1S/CH2O/c1-2/h1H2
#>                     inchi_key
#> 1 OQIQSTLJSLGHID-WNWIJWBNSA-N
#> 2 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#>                                                                                                       iupac_name_2
#> 1 (3S,7R)-11-methoxy-6,8,19-trioxapentacyclo[10.7.0.02,9.03,7.013,17]nonadeca-1,4,9,11,13(17)-pentaene-16,18-dione
#> 2                                                                                                     formaldehyde
#>   x_log_p   exact_mass monoisotopic_mass tpsa complexity charge
#> 1     1.6 312.06338810      312.06338810 71.1        649      0
#> 2     1.2 30.010564683      30.010564683 17.1          2      0
#>   h_bond_donor_count h_bond_acceptor_count rotatable_bond_count
#> 1                  0                     6                    1
#> 2                  0                     1                    0
#>   heavy_atom_count isotope_atom_count atom_stereo_count
#> 1               23                  0                 2
#> 2                2                  0                 0
#>   defined_atom_stereo_count undefined_atom_stereo_count bond_stereo_count
#> 1                         2                           0                 0
#> 2                         0                           0                 0
#>   defined_bond_stereo_count undefined_bond_stereo_count covalent_unit_count
#> 1                         0                           0                   1
#> 2                         0                           0                   1
#>   volume3d x_steric_quadrupole3d y_steric_quadrupole3d z_steric_quadrupole3d
#> 1    226.6                  8.20                  3.35                  0.83
#> 2     26.8                  0.93                  0.56                  0.56
#>   feature_count3d feature_acceptor_count3d feature_donor_count3d
#> 1              10                        5                     0
#> 2               1                        1                     0
#>   feature_anion_count3d feature_cation_count3d feature_ring_count3d
#> 1                     0                      0                    5
#> 2                     0                      0                    0
#>   feature_hydrophobe_count3d conformer_model_rmsd3d effective_rotor_count3d
#> 1                          0                    0.6                     1.8
#> 2                          0                    0.4                     0.0
#>   conformer_count3d
#> 1                 1
#> 2                 1
#>                                                                                                                                                  fingerprint2d
#> 1 AAADccB4OAAAAAAAAAAAAAAAAAAAASIEAAAwQAAAAAAQAEiBAAAAGgAAAAAADQSwmAMyDoAABACIAqDSCAACCAAgIAAIiAEGCMgcJzaMMRqiOiCl4BUMqQfI6PyOoAACCAAIAABAAAQQABAAAAAAAAAAAA==
#> 2 AAADcQAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEgAAAAAAAAAAAAIAAAAAAAAIAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==
#>          title patent_count patent_family_count literature_count
#> 1 Aflatoxin B1        12177                7222            20046
#> 2 Formaldehyde       837300              446054           151266
#>                                                                                                                                                                                                                                                                                                                                                                        annotation_types
#> 1                          Biological Test Results: Active|Associated Disorders and Diseases|Biological Test Results|Chemical and Physical Properties|Classification|Drug and Medication Information|Food Additives and Ingredients|Identification|Interactions and Pathways|Literature|Patents|Pharmacology and Biochemistry|Safety and Hazards|Spectral Information|Taxonomy|Toxicity
#> 2 Agrochemical Information|Biological Test Results: Active|Associated Disorders and Diseases|Biological Test Results|Chemical and Physical Properties|Classification|Drug and Medication Information|Food Additives and Ingredients|Identification|Interactions and Pathways|Literature|Patents|Pharmacology and Biochemistry|Safety and Hazards|Spectral Information|Taxonomy|Toxicity
#>   annotation_type_count
#> 1                    16
#> 2                    17
#>                                                                                                                                                  source_categories
#> 1 Chemical Vendors|Curation Efforts|Governmental Organizations|Journal Publishers|Legacy Depositors|NIH Initiatives|Research and Development|Subscription Services
#> 2 Chemical Vendors|Curation Efforts|Governmental Organizations|Journal Publishers|Legacy Depositors|NIH Initiatives|Research and Development|Subscription Services
#>          query
#> 1 Aflatoxin B1
#> 2 Formaldehyde
# }
```
