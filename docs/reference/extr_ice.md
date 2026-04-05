# Extract Data from NTP ICE Database

The `extr_ice` function sends a POST request to the ICE API to search
for information based on specified chemical IDs and assays.

## Usage

``` r
extr_ice(casrn, assays = NULL, verify_ssl = FALSE, verbose = TRUE, ...)
```

## Arguments

- casrn:

  A character vector specifying the CASRNs for the search.

- assays:

  A character vector specifying the assays to include in the search.
  Default is NULL, meaning all assays are included. If you don't know
  the exact assay name, you can use the
  [`extr_ice_assay_names()`](https://c1au6i0.github.io/extractox/reference/extr_ice_assay_names.md)
  function to search for assay names that match a pattern you're
  interested in.

- verify_ssl:

  Boolean to control of SSL should be verified or not.

- verbose:

  A logical value indicating whether to print detailed messages. Default
  is TRUE.

- ...:

  Any other arguments to be supplied to `req_option` and thus to
  `libcurl`.

## Value

A data frame containing the extracted data from the ICE API.

## See also

[`extr_ice_assay_names`](https://c1au6i0.github.io/extractox/reference/extr_ice_assay_names.md),
[NTP ICE database](https://ice.ntp.niehs.nih.gov/)

## Examples

``` r
# \donttest{
extr_ice(casrn = c("50-00-0"))
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> ℹ Sending request to ICE database...
#> ℹ Request succeeded with status code: 200
#>                                                                                                                                        assay
#> 1                                                                                   OPERA, soil adsorption coefficient of organic compounds.
#> 2                                                                                                                OPERA, Henry's Law Constant
#> 3                                   OPERA, The whole body primary biotransformation rate (half-life) constant for organic chemicals in fish.
#> 4                                                                                                 OPERA, Octanol-Water Partition Coefficient
#> 5                                                                                                                OPERA, HPLC retention time.
#> 6                                                                                                       OPERA, Human Plasma Fraction Unbound
#> 7                                                                                                                        CoMPARA, AR Binding
#> 8                                                                                                        OPERA, Fish bioconcentration factor
#> 9   OPERA, OH rate constant for the atmospheric, gas-phase reaction between photochemically produced hydroxyl radicals and organic chemicals
#> 10                                                                                                      OPERA, Caco-2 permeability (logPapp)
#> 11                                                                                                                        CERAPP, ER Agonist
#> 12                                                                                                                       CoMPARA, AR Binding
#> 13                                                         OPERA, biodegradation half-life for compounds containing only carbon and hydrogen
#> 14                                                                                                                    CoMPARA, AR Antagonist
#> 15                                                                                                  OPERA, Human Hepatic Intrinsic Clearance
#> 16                                                                                                                        CERAPP, ER Agonist
#> 17                                                                                                                    CoMPARA, AR Antagonist
#> 18                                                                                                                     CERAPP, ER Antagonist
#> 19                                                                                                                     CERAPP, ER Antagonist
#> 20                                                                                                                       CoMPARA, AR Agonist
#> 21                                                                                                                       CoMPARA, AR Agonist
#> 22                                                                                                               CATMoS, Acute Oral Toxicity
#> 23                                                                                                               CATMoS, Acute Oral Toxicity
#> 24                                                                                                                        CERAPP, ER Binding
#> 25                                                                                                                        CERAPP, ER Binding
#> 26                                                                                                               CATMoS, Acute Oral Toxicity
#> 27                                                                                                               CATMoS, Acute Oral Toxicity
#> 28                                                                                                               CATMoS, Acute Oral Toxicity
#> 29                                                                                                               CATMoS, Acute Oral Toxicity
#> 30                                                                                                               CATMoS, Acute Oral Toxicity
#> 31                                                                                                                      OPERA, Boiling Point
#> 32                                                                                                               CATMoS, Acute Oral Toxicity
#> 33                                                                                                               CATMoS, Acute Oral Toxicity
#> 34                                                                                                                     OPERA, Vapor Pressure
#> 35                                                                                                                   OPERA, Water Solubility
#> 36                                                                                                                      OPERA, Melting Point
#> 37                                                                                                  OPERA, Octanol-Air Partition Coefficient
#> 38                                                                                                                                      DPRA
#> 39                                                                                                                                      DPRA
#> 40                                                                                                                                      DPRA
#> 41                                                                                                                                      DPRA
#> 42                                                                                                                                      DPRA
#> 43                                                                                                                                      DPRA
#> 44                                                                                                                                      LLNA
#> 45                                                                                                            Human Repeat Insult Patch Test
#> 46                                                                                                                                      DPRA
#> 47                                                                                                                                      DPRA
#> 48                                                                                                                                      DPRA
#> 49                                                                                                                                      DPRA
#> 50                                                                                                                                      DPRA
#> 51                                                                                                                                      DPRA
#> 52                                                                                                            Human Repeat Insult Patch Test
#> 53                                                                                                            Human Repeat Insult Patch Test
#> 54                                                                                             OPERA, Octanol-Water Distribution Coefficient
#> 55                                                                                                            Human Repeat Insult Patch Test
#> 56                                                                                                            Human Repeat Insult Patch Test
#> 57                                                                                                            Human Repeat Insult Patch Test
#> 58                                                                                         OPERA, Negative Log of Acid Dissociation Constant
#> 59                                                                                                            Human Repeat Insult Patch Test
#> 60                                                                                         OPERA, Negative Log of Acid Dissociation Constant
#> 61                                                                                                            Human Repeat Insult Patch Test
#> 62                                                                                             OPERA, Octanol-Water Distribution Coefficient
#> 63                                                                                                            Human Repeat Insult Patch Test
#> 64                                                                                         OPERA, Negative Log of Acid Dissociation Constant
#> 65                                                                                                            Human Repeat Insult Patch Test
#> 66                                                                                                                             TER Corrosion
#> 67                                                                                                                             TER Corrosion
#> 68                                                                                                                             TER Corrosion
#> 69                                                                                                                             TER Corrosion
#> 70                                                                                                                             TER Corrosion
#> 71                                                                                                                             TER Corrosion
#> 72                                                                                                               SEEM3, Exposure Predictions
#> 73                                                                                                                                    U-SENS
#> 74                                                                                                                                    U-SENS
#> 75                                                                                                                                   SENS-IS
#> 76                                                                                                                                   SENS-IS
#> 77                                                                                                                                    mMUSST
#> 78                                                                                                                                    LuSens
#> 79                                                                                                                              KeratinoSens
#> 80                                                                                                                   Human Maximization Test
#> 81                                                                                                                   Human Maximization Test
#> 82                                                                                                                     In Vitro Genotoxicity
#> 83                                                                                                                      IRIS Carcinogenicity
#> 84                                                                                                                      IRIS Carcinogenicity
#> 85                                                                                                                     Report on Carcinogens
#> 86                                                                                                                      IARC Carcinogenicity
#> 87                                                                                                               SEEM3, Exposure Predictions
#> 88                                                                                                                                     hCLAT
#> 89                                                                                                                                     hCLAT
#> 90                                                                                                                              KeratinoSens
#> 91                                                                                                                                     hCLAT
#> 92                                                                                                                                     hCLAT
#> 93                                                                                                                                     hCLAT
#> 94                                                                                                                                     hCLAT
#> 95                                                                                                                                     hCLAT
#> 96                                                                                                                                     hCLAT
#> 97                                                                                                                                     hCLAT
#> 98                                                                                                             Rat Acute Inhalation Toxicity
#> 99                                                                                                             Rat Acute Inhalation Toxicity
#> 100                                                                                                            Rat Acute Inhalation Toxicity
#> 101                                                                                                            Rat Acute Inhalation Toxicity
#> 102                                                                                                                  Rat Acute Oral Toxicity
#> 103                                                                                                                  Rat Acute Oral Toxicity
#> 104                                                                                                                  Rat Acute Oral Toxicity
#> 105                                                                                                                  Rat Acute Oral Toxicity
#> 106                                                                                                            Rat Acute Inhalation Toxicity
#> 107                                                                                                            Rat Acute Inhalation Toxicity
#> 108                                                                                                            Rat Acute Inhalation Toxicity
#> 109                                                                                                            Rat Acute Inhalation Toxicity
#> 110                                                                                                                                     DPRA
#> 111                                                                                                                                     DPRA
#> 112                                                                                                                                     DPRA
#> 113                                                                                                                                     DPRA
#> 114                                                                                                                                     DPRA
#> 115                                                                                                                                     DPRA
#> 116                                                                                                                                     DPRA
#> 117                                                                                                                                     DPRA
#> 118                                                                                                                  Rat Acute Oral Toxicity
#> 119                                                                                                            OPERA, Number of oxygen atoms
#> 120                                                                                                                  Rat Acute Oral Toxicity
#> 121                                                                                                              OPERA, HPLC retention time.
#> 122                                                                                                                  Rat Acute Oral Toxicity
#> 123                                                                               OPERA, Number of rotatable bonds, excluding terminal bonds
#> 124                                                                                                                  Rat Acute Oral Toxicity
#> 125                                                                                                          OPERA, Number of nitrogen atoms
#> 126                                                                                                                                     DPRA
#> 127                                                                                                                                     DPRA
#> 128                                                                                                                                     DPRA
#> 129                                                                                                                                     DPRA
#> 130                                                                                                           Human Repeat Insult Patch Test
#> 131                                                                                                           Human Repeat Insult Patch Test
#> 132                                                                                                                                     LLNA
#> 133                                                                                                                                     LLNA
#> 134                                                                                                           Human Repeat Insult Patch Test
#> 135                                                                                                           Human Repeat Insult Patch Test
#> 136                                                                                                           Human Repeat Insult Patch Test
#> 137                                                                                                           Human Repeat Insult Patch Test
#> 138                                                                                            OPERA, Octanol-Water Distribution Coefficient
#> 139                                                                                                                                     LLNA
#> 140                                                                                                                                     LLNA
#> 141                                                                                                                                     LLNA
#> 142                                                                                                                                     LLNA
#> 143                                                                                                                                     LLNA
#> 144                                                                                                                                     LLNA
#> 145                                                                                                                                     LLNA
#> 146                                                                                                                                     LLNA
#> 147                                                                                                           Human Repeat Insult Patch Test
#> 148                                                                                                           Human Repeat Insult Patch Test
#> 149                                                                                                           Human Repeat Insult Patch Test
#> 150                                                                                                           Human Repeat Insult Patch Test
#> 151                                                                                                           Human Repeat Insult Patch Test
#> 152                                                                                                           Human Repeat Insult Patch Test
#> 153                                                                                                           Human Repeat Insult Patch Test
#> 154                                                                                                           Human Repeat Insult Patch Test
#> 155                                                                                                           Human Repeat Insult Patch Test
#> 156                                                                                                           Human Repeat Insult Patch Test
#> 157                                                                                                           Human Repeat Insult Patch Test
#> 158                                                                                                           Human Repeat Insult Patch Test
#> 159                                                                                                           Human Repeat Insult Patch Test
#> 160                                                                                                           Human Repeat Insult Patch Test
#> 161                                                                                                           Human Repeat Insult Patch Test
#> 162                                                                                                           Human Repeat Insult Patch Test
#> 163                                                                                                                            TER Corrosion
#> 164                                                                                                                                     DPRA
#> 165                                                                                                                  OPERA, Water Solubility
#> 166                                                                                                                            TER Corrosion
#> 167                                                                                                                                     DPRA
#> 168                                 OPERA, The whole body primary biotransformation rate (half-life) constant for organic chemicals in fish.
#> 169                                                                                                                            TER Corrosion
#> 170                                                                                                                                     DPRA
#> 171                                                                                               OPERA, Octanol-Water Partition Coefficient
#> 172                                                                                                                            TER Corrosion
#> 173                                                                                                                                     DPRA
#> 174                                                                                 OPERA, soil adsorption coefficient of organic compounds.
#> 175                                                                                                                            TER Corrosion
#> 176                                                                                                                                     DPRA
#> 177                                                                                                              OPERA, Henry's Law Constant
#> 178                                                                                                                                     DPRA
#> 179                                                                                                                   OPERA, Number of rings
#> 180                                                                                                                                     DPRA
#> 181                                                    OPERA, Number of hydrogen bond donors (using CDK HBondDonorCountDescriptor algorithm)
#> 182                                                                                                                                     DPRA
#> 183                                                                                            OPERA, Fraction of sp3 carbons to sp2 carbons
#> 184                                                                                                                                     DPRA
#> 185                                                                                                            OPERA, Number of carbon atoms
#> 186                                                                                                                                     DPRA
#> 187                                                                                       OPERA, Number failures of the Lipinski's Rule Of 5
#> 188                                                                                                                                     DPRA
#> 189                                              OPERA, Number of hydrogen bond acceptors (using CDK HBondAcceptorCountDescriptor algorithm)
#> 190                                                                                                                                     DPRA
#> 191                                                                                                                     OPERA, Melting Point
#> 192                                                                                                                                     DPRA
#> 193                                                                                                 OPERA, Octanol-Air Partition Coefficient
#> 194                                                                                                                                     DPRA
#> 195                                                                                                                    OPERA, Vapor Pressure
#> 196                                                                                                                                     DPRA
#> 197                                                                                                                OPERA, Molar refractivity
#> 198                                                                                                                            TER Corrosion
#> 199                                                                                                                                     DPRA
#> 200                                                                                                                         Molecular Weight
#> 201                                                                                                                                     LLNA
#> 202                                                                                                OPERA, Combined dipolarity/polarizability
#> 203                                                                                                                                     LLNA
#> 204                                                                                                              CATMoS, Acute Oral Toxicity
#> 205                                                                                                                                     LLNA
#> 206                                                                                                                     OPERA, Boiling Point
#> 207                                                                                                                                     LLNA
#> 208                                                                                                    OPERA, Topological polar surface area
#> 209                                                                                                                                     LLNA
#> 210                                                                  OPERA, Number of rings containing heteroatoms (N, O, P, S, or halogens)
#> 211                                                                                                                                     LLNA
#> 212                                                                                                          OPERA, Number of aromatic atoms
#> 213                                                                                                                                     LLNA
#> 214                                                                                         OPERA, Number of heavy atoms (i.e. not hydrogen)
#> 215                                                                                                                                     LLNA
#> 216                                                                                                                   OPERA, Number of atoms
#> 217                                                                                                                                     DPRA
#> 218                                                                                            OPERA, Octanol-Water Distribution Coefficient
#> 219                                                                                                                                     DPRA
#> 220                                                                                        OPERA, Negative Log of Acid Dissociation Constant
#> 221                                                                                                                                     DPRA
#> 222                                                                                                      OPERA, Fish bioconcentration factor
#> 223                                                                                                                                     DPRA
#> 224 OPERA, OH rate constant for the atmospheric, gas-phase reaction between photochemically produced hydroxyl radicals and organic chemicals
#> 225                                                                                                                                     DPRA
#> 226                                                                                                     OPERA, Human Plasma Fraction Unbound
#> 227                                                                                                                                     DPRA
#> 228                                                        OPERA, biodegradation half-life for compounds containing only carbon and hydrogen
#> 229                                                                                                                                     DPRA
#> 230                                                                                                     OPERA, Caco-2 permeability (logPapp)
#> 231                                                                                                                                     DPRA
#> 232                                                                                                 OPERA, Human Hepatic Intrinsic Clearance
#> 233                                                                                                                                    hCLAT
#> 234                                                                                                                                    hCLAT
#> 235                                                                                                                                    hCLAT
#> 236                                                                                                                             KeratinoSens
#> 237                                                                                                                             KeratinoSens
#> 238                                                                                                                             KeratinoSens
#> 239                                                                                                                             KeratinoSens
#> 240                                                                                                                                    hCLAT
#> 241                                                                                                                                    hCLAT
#> 242                                                                                                                                    hCLAT
#> 243                                                                                                                                    hCLAT
#> 244                                                                                                                  Human Maximization Test
#> 245                                                                                                                  Human Maximization Test
#> 246                                                                                                                  Human Maximization Test
#> 247                                                                                                           Human Repeat Insult Patch Test
#> 248                                                                                                                  Human Maximization Test
#> 249                                                                                                                  Human Maximization Test
#> 250                                                                                                                  Human Maximization Test
#> 251                                                                                                                  Human Maximization Test
#> 252                                                                                                           Human Repeat Insult Patch Test
#> 253                                                                                                           Human Repeat Insult Patch Test
#> 254                                                                                                           Human Repeat Insult Patch Test
#> 255                                                                                                           Human Repeat Insult Patch Test
#> 256                                                                                                           Human Repeat Insult Patch Test
#> 257                                                                                                           Human Repeat Insult Patch Test
#> 258                                                                                                           Human Repeat Insult Patch Test
#> 259                                                                                                           Human Repeat Insult Patch Test
#> 260                                                                                                                                   LuSens
#> 261                                                                                                                                   LuSens
#> 262                                                                                                                                   mMUSST
#> 263                                                                                                                                  SENS-IS
#> 264                                                                                                                             KeratinoSens
#> 265                                                                                                                             KeratinoSens
#> 266                                                                                                                             KeratinoSens
#> 267                                                                                                                                   LuSens
#> 268                                                                                                              SEEM3, Exposure Predictions
#> 269                                                                                                              SEEM3, Exposure Predictions
#> 270                                                                                                              SEEM3, Exposure Predictions
#> 271                                                                                                                     IRIS Carcinogenicity
#> 272                                                                                                                                  SENS-IS
#> 273                                                                                                                                   U-SENS
#> 274                                                                                                                                   U-SENS
#> 275                                                                                                                                   U-SENS
#>                                                             endpoint
#> 1                                               Applicability_Domain
#> 2                                               Applicability_Domain
#> 3                                               Applicability_Domain
#> 4                                               Applicability_Domain
#> 5                                               Applicability_Domain
#> 6                                               Applicability_Domain
#> 7                                                               Call
#> 8                                               Applicability_Domain
#> 9                                               Applicability_Domain
#> 10                                              Applicability_Domain
#> 11                                                              Call
#> 12                                              Applicability_Domain
#> 13                                              Applicability_Domain
#> 14                                              Applicability_Domain
#> 15                                              Applicability_Domain
#> 16                                              Applicability_Domain
#> 17                                                              Call
#> 18                                                              Call
#> 19                                              Applicability_Domain
#> 20                                                              Call
#> 21                                              Applicability_Domain
#> 22                                                        Very Toxic
#> 23                                              Applicability_Domain
#> 24                                                              Call
#> 25                                              Applicability_Domain
#> 26                                                         Non Toxic
#> 27                                              Applicability_Domain
#> 28                                                EPA Classification
#> 29                                              Applicability_Domain
#> 30                                              Applicability_Domain
#> 31                                              Applicability_Domain
#> 32                                                GHS Classification
#> 33                                              Applicability_Domain
#> 34                                              Applicability_Domain
#> 35                                              Applicability_Domain
#> 36                                              Applicability_Domain
#> 37                                              Applicability_Domain
#> 38                                                              Call
#> 39                                                              Call
#> 40                                                              Call
#> 41                                                              Call
#> 42                                                              Call
#> 43                                                              Call
#> 44                                                              Call
#> 45                                                              Call
#> 46                                                              Call
#> 47                                                              Call
#> 48                                                              Call
#> 49                                                              Call
#> 50                                                              Call
#> 51                                                              Call
#> 52                                                              Call
#> 53                                        Relative reliability score
#> 54                                              Applicability_Domain
#> 55                                                              Call
#> 56                                        Relative reliability score
#> 57                                        Relative reliability score
#> 58                                              Applicability_Domain
#> 59                                                              Call
#> 60                                              Applicability_Domain
#> 61                                                              Call
#> 62                                              Applicability_Domain
#> 63                                        Relative reliability score
#> 64                                              Applicability_Domain
#> 65                                        Relative reliability score
#> 66                                                              Call
#> 67                                                              Call
#> 68                                                              Call
#> 69                                                              Call
#> 70                                                              Call
#> 71                                                              Call
#> 72                                                  Specific Pathway
#> 73                                                              Call
#> 74                                                              Call
#> 75                                                           Potency
#> 76                                                           Potency
#> 77                                                              Call
#> 78                                                              Call
#> 79                                                              Call
#> 80                                        Relative reliability score
#> 81                                                              Call
#> 82                                            Bacterial mutagenicity
#> 83                                                        Tumor type
#> 84                                              WOE characterization
#> 85                                                    Listing status
#> 86                                                        IARC group
#> 87                                                   General Pathway
#> 88                                                              Call
#> 89                                                        CD86, Call
#> 90                                                              Call
#> 91                                                        CD86, Call
#> 92                                                        CD54, Call
#> 93                                                              Call
#> 94                                                        CD54, Call
#> 95                                                              Call
#> 96                                                        CD54, Call
#> 97                                                        CD86, Call
#> 98                                                              LC50
#> 99                                                              LC50
#> 100                                                             LC50
#> 101                                                             LC50
#> 102                                                             LD50
#> 103                                                             LD50
#> 104                                                             LD50
#> 105                                                             LD50
#> 106                                                             LC50
#> 107                                                             LC50
#> 108                                                             LC50
#> 109                                                             LC50
#> 110                                                    Depletion Cys
#> 111                                                    Depletion Lys
#> 112                                              Depletion Lys + Cys
#> 113                                                    Depletion Cys
#> 114                                              Depletion Lys + Cys
#> 115                                                    Depletion Cys
#> 116                                                    Depletion Lys
#> 117                                              Depletion Lys + Cys
#> 118                                                             LD50
#> 119                                                              nbO
#> 120                                                             LD50
#> 121                                                               RT
#> 122                                                             LD50
#> 123                                                          nbRotBd
#> 124                                                             LD50
#> 125                                                              nbN
#> 126                                                    Depletion Lys
#> 127                                              Depletion Lys + Cys
#> 128                                                    Depletion Cys
#> 129                                                    Depletion Lys
#> 130                                  Incidence of positive responses
#> 131                                                    Concentration
#> 132                                                              EC3
#> 133                                                              EC3
#> 134                Concentration, 5% incidence of positive responses
#> 135 Induction dose per skin area, 5% incidence of positive responses
#> 136                                                    Concentration
#> 137                                     Induction dose per skin area
#> 138                                                     LogD, ph 5.5
#> 139                                                              EC3
#> 140                                                              EC3
#> 141                                                              EC3
#> 142                                                              EC3
#> 143                                                              EC3
#> 144                                                              EC3
#> 145                                                              EC3
#> 146                                                              EC3
#> 147              Induction dose per skin area, one positive response
#> 148              Induction dose per skin area, one positive response
#> 149                Concentration, 5% incidence of positive responses
#> 150                Concentration, 5% incidence of positive responses
#> 151                                  Incidence of positive responses
#> 152                                  Incidence of positive responses
#> 153                             Concentration, one positive response
#> 154                             Concentration, one positive response
#> 155                                     Induction dose per skin area
#> 156                                  Incidence of positive responses
#> 157                             Concentration, one positive response
#> 158              Induction dose per skin area, one positive response
#> 159 Induction dose per skin area, 5% incidence of positive responses
#> 160 Induction dose per skin area, 5% incidence of positive responses
#> 161                                                    Concentration
#> 162                                                    Concentration
#> 163                                                              TER
#> 164                                              Depletion Lys + Cys
#> 165                                                               WS
#> 166                                                              TER
#> 167                                                    Depletion Lys
#> 168                                                            LogKM
#> 169                                                              TER
#> 170                                                    Depletion Cys
#> 171                                                             LogP
#> 172                                                              TER
#> 173                                              Depletion Lys + Cys
#> 174                                                           LogKOC
#> 175                                                              TER
#> 176                                                    Depletion Lys
#> 177                                                               HL
#> 178                                                    Depletion Cys
#> 179                                                           nbRing
#> 180                                                    Depletion Cys
#> 181                                                         ndHBdDon
#> 182                                              Depletion Lys + Cys
#> 183                                                   Sp3Sp2HybRatio
#> 184                                                    Depletion Lys
#> 185                                                              nbC
#> 186                                              Depletion Lys + Cys
#> 187                                               nbLipinskiFailures
#> 188                                                    Depletion Cys
#> 189                                                         nbHBdAcc
#> 190                                                    Depletion Lys
#> 191                                                               MP
#> 192                                                    Depletion Cys
#> 193                                                              KOA
#> 194                                                    Depletion Lys
#> 195                                                               VP
#> 196                                              Depletion Lys + Cys
#> 197                                                     MolarRefract
#> 198                                                              TER
#> 199                                                    Depletion Cys
#> 200                                                               MW
#> 201                                                              EC3
#> 202                                                 CombDipolPolariz
#> 203                                                              EC3
#> 204                                                             LD50
#> 205                                                              EC3
#> 206                                                               BP
#> 207                                                              EC3
#> 208                                                   TopoPolSurfAir
#> 209                                                              EC3
#> 210                                                     nbHeteroRing
#> 211                                                              EC3
#> 212                                                       nbAromAtom
#> 213                                                              EC3
#> 214                                                     nbHeavyAtoms
#> 215                                                              EC3
#> 216                                                          nbAtoms
#> 217                                                    Depletion Lys
#> 218                                                     LogD, ph 7.4
#> 219                                              Depletion Lys + Cys
#> 220                                                 pKa, Ionizations
#> 221                                              Depletion Lys + Cys
#> 222                                                           LogBCF
#> 223                                                    Depletion Lys
#> 224                                                           LogAOH
#> 225                                                    Depletion Cys
#> 226                                                               Fu
#> 227                                                    Depletion Lys
#> 228                                                        LogBioDeg
#> 229                                                    Depletion Cys
#> 230                                                         LogCACO2
#> 231                                              Depletion Lys + Cys
#> 232                                                            Clint
#> 233                                                      CD86, EC150
#> 234                                                      CD54, EC200
#> 235                                                             CV75
#> 236                                                             Imax
#> 237                                                            EC1.5
#> 238                                                            EC1.5
#> 239                                                              EC3
#> 240                                                             CV75
#> 241                                                      CD86, EC150
#> 242                                                      CD54, EC200
#> 243                                                             CV75
#> 244                Concentration, 5% incidence of positive responses
#> 245 Induction dose per skin area, 5% incidence of positive responses
#> 246                                                    Concentration
#> 247                             Concentration, one positive response
#> 248                                     Induction dose per skin area
#> 249                                  Incidence of positive responses
#> 250                             Concentration, one positive response
#> 251              Induction dose per skin area, one positive response
#> 252 Induction dose per skin area, 5% incidence of positive responses
#> 253                                                    Concentration
#> 254                                     Induction dose per skin area
#> 255                                     Induction dose per skin area
#> 256                                     Induction dose per skin area
#> 257              Induction dose per skin area, one positive response
#> 258                                  Incidence of positive responses
#> 259                Concentration, 5% incidence of positive responses
#> 260                                                            EC1.5
#> 261                                                             IC50
#> 262                                                      CD86, EC120
#> 263                                                    Concentration
#> 264                                                             IC50
#> 265                                                             IC50
#> 266                                                             Imax
#> 267                                                             Imax
#> 268                                                  50th percentile
#> 269                                                   5th percentile
#> 270                                                  95th percentile
#> 271                                             Inhalation risk unit
#> 272                                                    Concentration
#> 273                                                      CD86, EC150
#> 274                                                      CD86, EC150
#> 275                                                             CV70
#>     substance_type         casrn               qsar_ready_id
#> 1         Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 2         Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 3         Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 4         Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 5         Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 6         Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 7         Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 8         Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 9         Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 10        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 11        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 12        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 13        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 14        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 15        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 16        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 17        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 18        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 19        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 20        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 21        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 22        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 23        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 24        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 25        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 26        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 27        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 28        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 29        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 30        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 31        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 32        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 33        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 34        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 35        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 36        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 37        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 38        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 39        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 40        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 41        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 42        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 43        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 44        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 45        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 46        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 47        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 48        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 49        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 50        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 51        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 52        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 53        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 54        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 55        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 56        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 57        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 58        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 59        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 60        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 61        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 62        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 63        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 64        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 65        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 66         Mixture ICE_660959215                            
#> 67         Mixture ICE_660959215                            
#> 68         Mixture ICE_660959215                            
#> 69         Mixture ICE_660959215                            
#> 70         Mixture ICE_660959215                            
#> 71         Mixture ICE_660959215                            
#> 72        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 73        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 74        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 75        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 76        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 77        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 78        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 79        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 80        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 81        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 82        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 83        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 84        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 85        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 86        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 87        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 88        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 89        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 90        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 91        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 92        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 93        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 94        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 95        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 96        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 97        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 98        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 99        Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 100       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 101       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 102       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 103       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 104       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 105       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 106       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 107       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 108       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 109       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 110       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 111       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 112       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 113       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 114       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 115       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 116       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 117       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 118       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 119       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 120       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 121       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 122       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 123       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 124       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 125       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 126       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 127       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 128       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 129       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 130       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 131       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 132       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 133       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 134       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 135       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 136       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 137       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 138       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 139       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 140       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 141       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 142       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 143       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 144       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 145       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 146       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 147       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 148       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 149       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 150       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 151       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 152       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 153       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 154       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 155       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 156       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 157       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 158       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 159       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 160       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 161       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 162       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 163        Mixture ICE_660959215                            
#> 164       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 165       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 166        Mixture ICE_660959215                            
#> 167       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 168       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 169        Mixture ICE_660959215                            
#> 170       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 171       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 172        Mixture ICE_660959215                            
#> 173       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 174       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 175        Mixture ICE_660959215                            
#> 176       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 177       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 178       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 179       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 180       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 181       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 182       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 183       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 184       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 185       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 186       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 187       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 188       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 189       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 190       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 191       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 192       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 193       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 194       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 195       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 196       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 197       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 198        Mixture ICE_660959215                            
#> 199       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 200       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 201       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 202       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 203       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 204       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 205       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 206       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 207       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 208       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 209       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 210       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 211       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 212       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 213       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 214       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 215       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 216       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 217       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 218       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 219       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 220       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 221       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 222       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 223       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 224       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 225       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 226       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 227       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 228       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 229       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 230       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 231       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 232       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 233       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 234       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 235       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 236       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 237       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 238       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 239       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 240       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 241       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 242       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 243       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 244       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 245       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 246       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 247       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 248       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 249       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 250       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 251       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 252       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 253       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 254       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 255       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 256       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 257       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 258       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 259       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 260       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 261       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 262       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 263       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 264       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 265       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 266       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 267       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 268       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 269       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 270       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 271       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 272       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 273       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 274       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#> 275       Chemical       50-00-0 WSFSSNUMVMOOMR-UHFFFAOYSA-N
#>                                                                                                         value
#> 1                                                                                                           1
#> 2                                                                                                           1
#> 3                                                                                                           0
#> 4                                                                                                           1
#> 5                                                                                                           0
#> 6                                                                                                           0
#> 7                                                                                                           0
#> 8                                                                                                           1
#> 9                                                                                                           1
#> 10                                                                                                          0
#> 11                                                                                                        0.0
#> 12                                                                                                          1
#> 13                                                                                                          0
#> 14                                                                                                          1
#> 15                                                                                                          0
#> 16                                                                                                          1
#> 17                                                                                                        0.0
#> 18                                                                                                        0.0
#> 19                                                                                                          1
#> 20                                                                                                        0.0
#> 21                                                                                                          1
#> 22                                                                                                          0
#> 23                                                                                                          1
#> 24                                                                                                          0
#> 25                                                                                                        1.0
#> 26                                                                                                          0
#> 27                                                                                                          1
#> 28                                                                                                          2
#> 29                                                                                                          1
#> 30                                                                                                          1
#> 31                                                                                                          1
#> 32                                                                                                          3
#> 33                                                                                                          1
#> 34                                                                                                          1
#> 35                                                                                                          1
#> 36                                                                                                          1
#> 37                                                                                                          1
#> 38                                                                                                     Active
#> 39                                                                                                     Active
#> 40                                                                                                     Active
#> 41                                                                                                     Active
#> 42                                                                                                     Active
#> 43                                                                                                     Active
#> 44                                                                                                     Active
#> 45                                                                                                     Active
#> 46                                                                                                     Active
#> 47                                                                                                     Active
#> 48                                                                                                     Active
#> 49                                                                                                     Active
#> 50                                                                                                     Active
#> 51                                                                                                     Active
#> 52                                                                                                   Inactive
#> 53                                                                                                          2
#> 54                                                                                                          0
#> 55                                                                                                     Active
#> 56                                                                                                          3
#> 57                                                                                                          2
#> 58                                                                                                          0
#> 59                                                                                                     Active
#> 60                                                                                                          0
#> 61                                                                                                     Active
#> 62                                                                                                          0
#> 63                                                                                                          3
#> 64                                                                                                          0
#> 65                                                                                                          2
#> 66                                                                                                   Inactive
#> 67                                                                                                   Inactive
#> 68                                                                                                   Inactive
#> 69                                                                                                   Inactive
#> 70                                                                                                   Inactive
#> 71                                                                                                   Inactive
#> 72                                                                                                    Dietary
#> 73                                                                                                     Active
#> 74                                                                                                     Active
#> 75                                                                                                     Strong
#> 76                                                                                                     Strong
#> 77                                                                                                     Active
#> 78                                                                                                     Active
#> 79                                                                                                     Active
#> 80                                                                                                          1
#> 81                                                                                                     Active
#> 82     Weakly Positive; Weakly Positive; Positive; Positive; Positive; Positive; Positive; Positive; Positive
#> 83                                                                                    Squamous cell carcinoma
#> 84  B1 (Probable human carcinogen - based on limited evidence of carcinogenicity in humans) (1986 guidelines)
#> 85                                                                                                      Known
#> 86                                                                                                          1
#> 87                                                                                                 Near-Field
#> 88                                                                                                     Active
#> 89                                                                                                   Inactive
#> 90                                                                                                     Active
#> 91                                                                                                     Active
#> 92                                                                                                     Active
#> 93                                                                                                     Active
#> 94                                                                                                   Inactive
#> 95                                                                                                   Inactive
#> 96                                                                                                     Active
#> 97                                                                                                     Active
#> 98                                                                                                     1.0064
#> 99                                                                                                     0.5866
#> 100                                                                                                  < 0.5682
#> 101                                                                                                     0.203
#> 102                                                                                                   < 800.0
#> 103                                                                                                     800.0
#> 104                                                                                                    2020.0
#> 105                                                                                                  > 7000.0
#> 106                                                                                                     0.578
#> 107                                                                                                    0.3068
#> 108                                                                                                    0.3068
#> 109                                                                                                    1.0002
#> 110                                                                                                      36.7
#> 111                                                                                                       1.3
#> 112                                                                                                     19.55
#> 113                                                                                                      37.3
#> 114                                                                                                      24.0
#> 115                                                                                                      44.2
#> 116                                                                                                       3.6
#> 117                                                                                                      19.0
#> 118                                                                                                   > 100.0
#> 119                                                                                                       1.0
#> 120                                                                                                     100.0
#> 121                                                                                                       0.0
#> 122                                                                                                     500.0
#> 123                                                                                                       0.0
#> 124                                                                                                     550.0
#> 125                                                                                                       0.0
#> 126                                                                                                       1.8
#> 127                                                                                                      25.3
#> 128                                                                                                      48.6
#> 129                                                                                                       2.0
#> 130                                                                                                     7.692
#> 131                                                                                                      1.85
#> 132                                                                                                       0.7
#> 133                                                                                                      0.37
#> 134                                                                                                    0.9768
#> 135                                                                                                     757.0
#> 136                                                                                                      1.11
#> 137                                                                                                    1434.0
#> 138                                                                                                      0.35
#> 139                                                                                                      0.37
#> 140                                                                                                      0.27
#> 141                                                                                                      0.11
#> 142                                                                                                       0.3
#> 143                                                                                                      0.99
#> 144                                                                                                      0.61
#> 145                                                                                                      0.35
#> 146                                                                                                      0.44
#> 147                                                                                                     71.69
#> 148                                                                                                     358.4
#> 149                                                                                                    0.4116
#> 150                                                                                                     2.359
#> 151                                                                                                     4.494
#> 152                                                                                                     7.843
#> 153                                                                                                    0.0925
#> 154                                                                                                    0.4625
#> 155                                                                                                     860.3
#> 156                                                                                                     5.682
#> 157                                                                                                     0.222
#> 158                                                                                                     172.1
#> 159                                                                                                     319.0
#> 160                                                                                                    1828.0
#> 161                                                                                                      0.37
#> 162                                                                                                       3.7
#> 163                                                                                                       6.4
#> 164                                                                                                      35.8
#> 165                                                                                                      1.17
#> 166                                                                                                       6.1
#> 167                                                                                                       4.2
#> 168                                                                                                     -0.65
#> 169                                                                                                       6.4
#> 170                                                                                                      44.7
#> 171                                                                                                      0.35
#> 172                                                                                                      12.3
#> 173                                                                                                     24.45
#> 174                                                                                                      0.44
#> 175                                                                                                       6.1
#> 176                                                                                                      11.2
#> 177                                                                                                     -6.46
#> 178                                                                                                      51.4
#> 179                                                                                                       0.0
#> 180                                                                                                      60.4
#> 181                                                                                                       0.0
#> 182                                                                                                     26.95
#> 183                                                                                                       0.0
#> 184                                                                                                       3.2
#> 185                                                                                                       1.0
#> 186                                                                                                      15.4
#> 187                                                                                                       0.0
#> 188                                                                                                      30.5
#> 189                                                                                                       1.0
#> 190                                                                                                       0.3
#> 191                                                                                                    -105.0
#> 192                                                                                                      40.6
#> 193                                                                                                      1.21
#> 194                                                                                                       3.8
#> 195                                                                                                      2.92
#> 196                                                                                                      19.9
#> 197                                                                                                     6.307
#> 198                                                                                                      12.3
#> 199                                                                                                      36.6
#> 200                                                                                                   30.0106
#> 201                                                                                                       8.0
#> 202                                                                                                     0.562
#> 203                                                                                                       5.6
#> 204                                                                                                     290.0
#> 205                                                                                                       8.2
#> 206                                                                                                     -19.0
#> 207                                                                                                      12.3
#> 208                                                                                                     17.07
#> 209                                                                                                      14.5
#> 210                                                                                                       0.0
#> 211                                                                                                       3.6
#> 212                                                                                                       0.0
#> 213                                                                                                       4.2
#> 214                                                                                                       2.0
#> 215                                                                                                       3.8
#> 216                                                                                                       4.0
#> 217                                                                                                       2.4
#> 218                                                                                                      0.35
#> 219                                                                                                      28.6
#> 220                                                                                                       0.0
#> 221                                                                                                      22.2
#> 222                                                                                                      0.24
#> 223                                                                                                       2.5
#> 224                                                                                                    -11.03
#> 225                                                                                                      54.8
#> 226                                                                                                      0.73
#> 227                                                                                                       2.3
#> 228                                                                                                      0.91
#> 229                                                                                                      49.6
#> 230                                                                                                     -5.07
#> 231                                                                                                     25.95
#> 232                                                                                                      6.35
#> 233                                                                                                       4.3
#> 234                                                                                                       5.2
#> 235                                                                                                       5.8
#> 236                                                                                                       4.1
#> 237                                                                                                     137.0
#> 238                                                                                                     63.21
#> 239                                                                                                      72.2
#> 240                                                                                                      23.3
#> 241                                                                                                       4.3
#> 242                                                                                                      5.14
#> 243                                                                                                       5.8
#> 244                                                                                                    0.1285
#> 245                                                                                                     79.74
#> 246                                                                                                      1.85
#> 247                                                                                                    0.4625
#> 248                                                                                                    1148.0
#> 249                                                                                                      72.0
#> 250                                                                                                    0.1028
#> 251                                                                                                     63.79
#> 252                                                                                                     931.9
#> 253                                                                                                     0.037
#> 254                                                                                                     286.8
#> 255                                                                                                    2868.0
#> 256                                                                                                     28.68
#> 257                                                                                                     358.4
#> 258                                                                                                       0.0
#> 259                                                                                                     1.202
#> 260                                                                                                     184.4
#> 261                                                                                                   > 288.0
#> 262                                                                                                     0.634
#> 263                                                                                                      37.0
#> 264                                                                                                    201.63
#> 265                                                                                                     385.9
#> 266                                                                                                     16.92
#> 267                                                                                                      3.99
#> 268                                                                                                  2.938E-5
#> 269                                                                                                  1.409E-9
#> 270                                                                                                    0.6579
#> 271                                                                                                    1.3E-5
#> 272                                                                                                      37.0
#> 273                                                                                                       1.6
#> 274                                                                                                      5.74
#> 275                                                                                                      5.74
#>                       unit species receptor_species      route sex strain
#> 1                                                                        
#> 2                                                                        
#> 3                                                                        
#> 4                                                                        
#> 5                                                                        
#> 6                                                                        
#> 7                                                                        
#> 8                                                                        
#> 9                                                                        
#> 10                                                                       
#> 11                                                                       
#> 12                                                                       
#> 13                                                                       
#> 14                                                                       
#> 15                                                                       
#> 16                                                                       
#> 17                                                                       
#> 18                                                                       
#> 19                                                                       
#> 20                                                                       
#> 21                                                                       
#> 22                                                                       
#> 23                                                                       
#> 24                                                                       
#> 25                                                                       
#> 26                                                                       
#> 27                                                                       
#> 28                                                                       
#> 29                                                                       
#> 30                                                                       
#> 31                                                                       
#> 32                                                                       
#> 33                                                                       
#> 34                                                                       
#> 35                                                                       
#> 36                                                                       
#> 37                                                                       
#> 38                                                                       
#> 39                                                                       
#> 40                                                                       
#> 41                                                                       
#> 42                                                                       
#> 43                                                                       
#> 44                           Mouse                      Dermal           
#> 45                           Human                      Dermal           
#> 46                                                                       
#> 47                                                                       
#> 48                                                                       
#> 49                                                                       
#> 50                                                                       
#> 51                                                                       
#> 52                           Human                      Dermal           
#> 53                           Human                      Dermal           
#> 54                                                                       
#> 55                           Human                      Dermal           
#> 56                           Human                      Dermal           
#> 57                           Human                      Dermal           
#> 58                                                                       
#> 59                           Human                      Dermal           
#> 60                                                                       
#> 61                           Human                      Dermal           
#> 62                                                                       
#> 63                           Human                      Dermal           
#> 64                                                                       
#> 65                           Human                      Dermal           
#> 66                                                                       
#> 67                                                                       
#> 68                                                                       
#> 69                                                                       
#> 70                                                                       
#> 71                                                                       
#> 72                                                                       
#> 73                                                                       
#> 74                                                                       
#> 75                                                                       
#> 76                                                                       
#> 77                                                                       
#> 78                                                                       
#> 79                                                                       
#> 80                           Human                      Dermal           
#> 81                           Human                      Dermal           
#> 82                                                                       
#> 83                                                  Inhalation           
#> 84                                                  Inhalation           
#> 85                                                                       
#> 86                                                                       
#> 87                                                                       
#> 88                                                                       
#> 89                                                                       
#> 90                                                                       
#> 91                                                                       
#> 92                                                                       
#> 93                                                                       
#> 94                                                                       
#> 95                                                                       
#> 96                                                                       
#> 97                                                                       
#> 98                    mg/L                                               
#> 99                    mg/L                                               
#> 100                   mg/L                                               
#> 101                   mg/L                                               
#> 102                  mg/kg     Rat                                       
#> 103                  mg/kg     Rat                                       
#> 104                  mg/kg     Rat                                       
#> 105                  mg/kg     Rat                                       
#> 106                   mg/L                                               
#> 107                   mg/L                                               
#> 108                   mg/L                                               
#> 109                   mg/L                                               
#> 110                      %                                               
#> 111                      %                                               
#> 112                      %                                               
#> 113                      %                                               
#> 114                      %                                               
#> 115                      %                                               
#> 116                      %                                               
#> 117                      %                                               
#> 118                  mg/kg     Rat                                       
#> 119                  count                                               
#> 120                  mg/kg     Rat                                       
#> 121                Minutes                                               
#> 122                  mg/kg     Rat                                       
#> 123                  count                                               
#> 124                  mg/kg     Rat                                       
#> 125                  count                                               
#> 126                      %                                               
#> 127                      %                                               
#> 128                      %                                               
#> 129                      %                                               
#> 130                      %   Human                      Dermal           
#> 131                      %   Human                      Dermal           
#> 132                      %   Mouse                      Dermal           
#> 133                      %   Mouse                      Dermal           
#> 134                      %   Human                      Dermal           
#> 135                 ug/cm2   Human                      Dermal           
#> 136                      %   Human                      Dermal           
#> 137                 ug/cm2   Human                      Dermal           
#> 138         Log10 unitless                                               
#> 139                      %   Mouse                      Dermal           
#> 140                      %   Mouse                      Dermal           
#> 141                      %   Mouse                      Dermal           
#> 142                      %   Mouse                      Dermal           
#> 143                      %   Mouse                      Dermal           
#> 144                      %   Mouse                      Dermal           
#> 145                      %   Mouse                      Dermal           
#> 146                      %   Mouse                      Dermal           
#> 147                 ug/cm2   Human                      Dermal           
#> 148                 ug/cm2   Human                      Dermal           
#> 149                      %   Human                      Dermal           
#> 150                      %   Human                      Dermal           
#> 151                      %   Human                      Dermal           
#> 152                      %   Human                      Dermal           
#> 153                      %   Human                      Dermal           
#> 154                      %   Human                      Dermal           
#> 155                 ug/cm2   Human                      Dermal           
#> 156                      %   Human                      Dermal           
#> 157                      %   Human                      Dermal           
#> 158                 ug/cm2   Human                      Dermal           
#> 159                 ug/cm2   Human                      Dermal           
#> 160                 ug/cm2   Human                      Dermal           
#> 161                      %   Human                      Dermal           
#> 162                      %   Human                      Dermal           
#> 163               kiloohms                                               
#> 164                      %                                               
#> 165          Log10 moles/L                                               
#> 166               kiloohms                                               
#> 167                      %                                               
#> 168             Log10 days                                               
#> 169               kiloohms                                               
#> 170                      %                                               
#> 171        Log 10 unitless                                               
#> 172               kiloohms                                               
#> 173                      %                                               
#> 174             Log10 L/Kg                                               
#> 175               kiloohms                                               
#> 176                      %                                               
#> 177      Log10 atm-m3/mole                                               
#> 178                      %                                               
#> 179                  count                                               
#> 180                      %                                               
#> 181                  count                                               
#> 182                      %                                               
#> 183      Unitless Fraction                                               
#> 184                      %                                               
#> 185                  count                                               
#> 186                      %                                               
#> 187                  count                                               
#> 188                      %                                               
#> 189                  count                                               
#> 190                      %                                               
#> 191               Degree C                                               
#> 192                      %                                               
#> 193        Log 10 unitless                                               
#> 194                      %                                               
#> 195             Log10 mmHg                                               
#> 196                      %                                               
#> 197                 m3/mol                                               
#> 198               kiloohms                                               
#> 199                      %                                               
#> 200                  g/mol                                               
#> 201                      %   Mouse                      Dermal           
#> 202  dipole moment/ volume                                               
#> 203                      %   Mouse                      Dermal           
#> 204                  mg/kg                                               
#> 205                      %   Mouse                      Dermal           
#> 206               Degree C                                               
#> 207                      %   Mouse                      Dermal           
#> 208                    A^2                                               
#> 209                      %   Mouse                      Dermal           
#> 210                  count                                               
#> 211                      %   Mouse                      Dermal           
#> 212                  count                                               
#> 213                      %   Mouse                      Dermal           
#> 214                  count                                               
#> 215                      %   Mouse                      Dermal           
#> 216                  count                                               
#> 217                      %                                               
#> 218         Log10 unitless                                               
#> 219                      %                                               
#> 220                  count                                               
#> 221                      %                                               
#> 222         Log10 unitless                                               
#> 223                      %                                               
#> 224 Log10 cm3/molecule-sec                                               
#> 225                      %                                               
#> 226      Unitless Fraction                                               
#> 227                      %                                               
#> 228             Log10 days                                               
#> 229                      %                                               
#> 230         log(10-6 cm/s)                                               
#> 231                      %                                               
#> 232      ul/min/10^6 cells                                               
#> 233                  ug/mL                                               
#> 234                  ug/mL                                               
#> 235                  ug/mL                                               
#> 236                  Ratio                                               
#> 237                     uM                                               
#> 238                     uM                                               
#> 239                     uM                                               
#> 240                  ug/mL                                               
#> 241                  ug/mL                                               
#> 242                  ug/mL                                               
#> 243                  ug/mL                                               
#> 244                      %   Human                      Dermal           
#> 245                 ug/cm2   Human                      Dermal           
#> 246                      %   Human                      Dermal           
#> 247                      %   Human                      Dermal           
#> 248                 ug/cm2   Human                      Dermal           
#> 249                      %   Human                      Dermal           
#> 250                      %   Human                      Dermal           
#> 251                 ug/cm2   Human                      Dermal           
#> 252                 ug/cm2   Human                      Dermal           
#> 253                      %   Human                      Dermal           
#> 254                 ug/cm2   Human                      Dermal           
#> 255                 ug/cm2   Human                      Dermal           
#> 256                 ug/cm2   Human                      Dermal           
#> 257                 ug/cm2   Human                      Dermal           
#> 258                      %   Human                      Dermal           
#> 259                      %   Human                      Dermal           
#> 260                     uM                                               
#> 261                     uM                                               
#> 262                  ug/mL                                               
#> 263                      %                                               
#> 264                     uM                                               
#> 265                     uM                                               
#> 266                  Ratio                                               
#> 267                  Ratio                                               
#> 268              mg/kg/day                                               
#> 269              mg/kg/day                                               
#> 270              mg/kg/day                                               
#> 271                  ug/m3                          Inhalation           
#> 272                      %                                               
#> 273                  ug/mL                                               
#> 274                  ug/mL                                               
#> 275                  ug/mL                                               
#>     life_stage tissue lesion location assay_source in_vitro_assay_format
#> 1                                                                       
#> 2                                                                       
#> 3                                                                       
#> 4                                                                       
#> 5                                                                       
#> 6                                                                       
#> 7                                                                       
#> 8                                                                       
#> 9                                                                       
#> 10                                                                      
#> 11                                                                      
#> 12                                                                      
#> 13                                                                      
#> 14                                                                      
#> 15                                                                      
#> 16                                                                      
#> 17                                                                      
#> 18                                                                      
#> 19                                                                      
#> 20                                                                      
#> 21                                                                      
#> 22                                                                      
#> 23                                                                      
#> 24                                                                      
#> 25                                                                      
#> 26                                                                      
#> 27                                                                      
#> 28                                                                      
#> 29                                                                      
#> 30                                                                      
#> 31                                                                      
#> 32                                                                      
#> 33                                                                      
#> 34                                                                      
#> 35                                                                      
#> 36                                                                      
#> 37                                                                      
#> 38                                                                      
#> 39                                                                      
#> 40                                                                      
#> 41                                                                      
#> 42                                                                      
#> 43                                                                      
#> 44                                                                      
#> 45                                                                      
#> 46                                                                      
#> 47                                                                      
#> 48                                                                      
#> 49                                                                      
#> 50                                                                      
#> 51                                                                      
#> 52                                                                      
#> 53                                                                      
#> 54                                                                      
#> 55                                                                      
#> 56                                                                      
#> 57                                                                      
#> 58                                                                      
#> 59                                                                      
#> 60                                                                      
#> 61                                                                      
#> 62                                                                      
#> 63                                                                      
#> 64                                                                      
#> 65                                                                      
#> 66                                                                      
#> 67                                                                      
#> 68                                                                      
#> 69                                                                      
#> 70                                                                      
#> 71                                                                      
#> 72                                                                      
#> 73                                                                      
#> 74                                                                      
#> 75                                                                      
#> 76                                                                      
#> 77                                                                      
#> 78                                                                      
#> 79                                                                      
#> 80                                                                      
#> 81                                                                      
#> 82                                                                      
#> 83                                                                      
#> 84                                                                      
#> 85                                                                      
#> 86                                                                      
#> 87                                                                      
#> 88                                                                      
#> 89                                                                      
#> 90                                                                      
#> 91                                                                      
#> 92                                                                      
#> 93                                                                      
#> 94                                                                      
#> 95                                                                      
#> 96                                                                      
#> 97                                                                      
#> 98                                                                      
#> 99                                                                      
#> 100                                                                     
#> 101                                                                     
#> 102                                                                     
#> 103                                                                     
#> 104                                                                     
#> 105                                                                     
#> 106                                                                     
#> 107                                                                     
#> 108                                                                     
#> 109                                                                     
#> 110                                                                     
#> 111                                                                     
#> 112                                                                     
#> 113                                                                     
#> 114                                                                     
#> 115                                                                     
#> 116                                                                     
#> 117                                                                     
#> 118                                                                     
#> 119                                                                     
#> 120                                                                     
#> 121                                                                     
#> 122                                                                     
#> 123                                                                     
#> 124                                                                     
#> 125                                                                     
#> 126                                                                     
#> 127                                                                     
#> 128                                                                     
#> 129                                                                     
#> 130                                                                     
#> 131                                                                     
#> 132                                                                     
#> 133                                                                     
#> 134                                                                     
#> 135                                                                     
#> 136                                                                     
#> 137                                                                     
#> 138                                                                     
#> 139                                                                     
#> 140                                                                     
#> 141                                                                     
#> 142                                                                     
#> 143                                                                     
#> 144                                                                     
#> 145                                                                     
#> 146                                                                     
#> 147                                                                     
#> 148                                                                     
#> 149                                                                     
#> 150                                                                     
#> 151                                                                     
#> 152                                                                     
#> 153                                                                     
#> 154                                                                     
#> 155                                                                     
#> 156                                                                     
#> 157                                                                     
#> 158                                                                     
#> 159                                                                     
#> 160                                                                     
#> 161                                                                     
#> 162                                                                     
#> 163                                                                     
#> 164                                                                     
#> 165                                                                     
#> 166                                                                     
#> 167                                                                     
#> 168                                                                     
#> 169                                                                     
#> 170                                                                     
#> 171                                                                     
#> 172                                                                     
#> 173                                                                     
#> 174                                                                     
#> 175                                                                     
#> 176                                                                     
#> 177                                                                     
#> 178                                                                     
#> 179                                                                     
#> 180                                                                     
#> 181                                                                     
#> 182                                                                     
#> 183                                                                     
#> 184                                                                     
#> 185                                                                     
#> 186                                                                     
#> 187                                                                     
#> 188                                                                     
#> 189                                                                     
#> 190                                                                     
#> 191                                                                     
#> 192                                                                     
#> 193                                                                     
#> 194                                                                     
#> 195                                                                     
#> 196                                                                     
#> 197                                                                     
#> 198                                                                     
#> 199                                                                     
#> 200                                                                     
#> 201                                                                     
#> 202                                                                     
#> 203                                                                     
#> 204                                                                     
#> 205                                                                     
#> 206                                                                     
#> 207                                                                     
#> 208                                                                     
#> 209                                                                     
#> 210                                                                     
#> 211                                                                     
#> 212                                                                     
#> 213                                                                     
#> 214                                                                     
#> 215                                                                     
#> 216                                                                     
#> 217                                                                     
#> 218                                                                     
#> 219                                                                     
#> 220                                                                     
#> 221                                                                     
#> 222                                                                     
#> 223                                                                     
#> 224                                                                     
#> 225                                                                     
#> 226                                                                     
#> 227                                                                     
#> 228                                                                     
#> 229                                                                     
#> 230                                                                     
#> 231                                                                     
#> 232                                                                     
#> 233                                                                     
#> 234                                                                     
#> 235                                                                     
#> 236                                                                     
#> 237                                                                     
#> 238                                                                     
#> 239                                                                     
#> 240                                                                     
#> 241                                                                     
#> 242                                                                     
#> 243                                                                     
#> 244                                                                     
#> 245                                                                     
#> 246                                                                     
#> 247                                                                     
#> 248                                                                     
#> 249                                                                     
#> 250                                                                     
#> 251                                                                     
#> 252                                                                     
#> 253                                                                     
#> 254                                                                     
#> 255                                                                     
#> 256                                                                     
#> 257                                                                     
#> 258                                                                     
#> 259                                                                     
#> 260                                                                     
#> 261                                                                     
#> 262                                                                     
#> 263                                                                     
#> 264                                                                     
#> 265                                                                     
#> 266                                                                     
#> 267                                                                     
#> 268                                                                     
#> 269                                                                     
#> 270                                                                     
#> 271                                                                     
#> 272                                                                     
#> 273                                                                     
#> 274                                                                     
#> 275                                                                     
#>                                                                                                                                                                                                                                                                                                                                                                             reference
#> 1                                                                                                                                                                                                                                                                                                                                                                                    
#> 2                                                                                                                                                                                                                                                                                                                                                                                    
#> 3                                                                                                                                                                                                                                                                                                                                                                                    
#> 4                                                                                                                                                                                                                                                                                                                                                                                    
#> 5                                                                                                                                                                                                                                                                                                                                                                                    
#> 6                                                                                                                                                                                                                                                                                                                                                                                    
#> 7                                                                                                                                                                                                                                                                                                                                                                                    
#> 8                                                                                                                                                                                                                                                                                                                                                                                    
#> 9                                                                                                                                                                                                                                                                                                                                                                                    
#> 10                                                                                                                                                                                                                                                                                                                                                                                   
#> 11                                                                                                                                                                                                                                                                                                                                                                                   
#> 12                                                                                                                                                                                                                                                                                                                                                                                   
#> 13                                                                                                                                                                                                                                                                                                                                                                                   
#> 14                                                                                                                                                                                                                                                                                                                                                                                   
#> 15                                                                                                                                                                                                                                                                                                                                                                                   
#> 16                                                                                                                                                                                                                                                                                                                                                                                   
#> 17                                                                                                                                                                                                                                                                                                                                                                                   
#> 18                                                                                                                                                                                                                                                                                                                                                                                   
#> 19                                                                                                                                                                                                                                                                                                                                                                                   
#> 20                                                                                                                                                                                                                                                                                                                                                                                   
#> 21                                                                                                                                                                                                                                                                                                                                                                                   
#> 22                                                                                                                                                                                                                                                                                                                                                                                   
#> 23                                                                                                                                                                                                                                                                                                                                                                                   
#> 24                                                                                                                                                                                                                                                                                                                                                                                   
#> 25                                                                                                                                                                                                                                                                                                                                                                                   
#> 26                                                                                                                                                                                                                                                                                                                                                                                   
#> 27                                                                                                                                                                                                                                                                                                                                                                                   
#> 28                                                                                                                                                                                                                                                                                                                                                                                   
#> 29                                                                                                                                                                                                                                                                                                                                                                                   
#> 30                                                                                                                                                                                                                                                                                                                                                                                   
#> 31                                                                                                                                                                                                                                                                                                                                                                                   
#> 32                                                                                                                                                                                                                                                                                                                                                                                   
#> 33                                                                                                                                                                                                                                                                                                                                                                                   
#> 34                                                                                                                                                                                                                                                                                                                                                                                   
#> 35                                                                                                                                                                                                                                                                                                                                                                                   
#> 36                                                                                                                                                                                                                                                                                                                                                                                   
#> 37                                                                                                                                                                                                                                                                                                                                                                                   
#> 38                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 39                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 40                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 41                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 42                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 43                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 44                                                                                                                                                                                                                                                                                                                         Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 45                                                                                                                                                                                                                                                                                                                            Marzulli and Maibach 1973; Not available; Not available
#> 46                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 47                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 48                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 49                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 50                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 51                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 52                                                                                                                                                                                                                                                                                                                            Marzulli and Maibach 1973; Not available; Not available
#> 53                                                                                                                                                                                                                                                                                                                            Marzulli and Maibach 1973; Not available; Not available
#> 54                                                                                                                                                                                                                                                                                                                                                                                   
#> 55                                                                                                                                                                                                                                                                                                                            Marzulli and Maibach 1973; Not available; Not available
#> 56                                                                                                                                                                                                                                                                                                                            Marzulli and Maibach 1973; Not available; Not available
#> 57                                                                                                                                                                                                                                                                                                                            Marzulli and Maibach 1973; Not available; Not available
#> 58                                                                                                                                                                                                                                                                                                                                                                                   
#> 59                                                                                                                                                                                                                                                                                                                            Marzulli and Maibach 1973; Not available; Not available
#> 60                                                                                                                                                                                                                                                                                                                                                                                   
#> 61                                                                                                                                                                                                                                                                                                                            Marzulli and Maibach 1973; Not available; Not available
#> 62                                                                                                                                                                                                                                                                                                                                                                                   
#> 63                                                                                                                                                                                                                                                                                                                            Marzulli and Maibach 1973; Not available; Not available
#> 64                                                                                                                                                                                                                                                                                                                                                                                   
#> 65                                                                                                                                                                                                                                                                                                                            Marzulli and Maibach 1973; Not available; Not available
#> 66                                                                                                                                                                                                                                                                                                                         Botham et al. 1992; 20732113; 10.1016/0887-2333(92)90031-l
#> 67                                                                                                                                                                                                                                                                                                                         Botham et al. 1992; 20732113; 10.1016/0887-2333(92)90031-l
#> 68                                                                                                                                                                                                                                                                                                                         Botham et al. 1992; 20732113; 10.1016/0887-2333(92)90031-l
#> 69                                                                                                                                                                                                                                                                                                                         Botham et al. 1992; 20732113; 10.1016/0887-2333(92)90031-l
#> 70                                                                                                                                                                                                                                                                                                                         Botham et al. 1992; 20732113; 10.1016/0887-2333(92)90031-l
#> 71                                                                                                                                                                                                                                                                                                                         Botham et al. 1992; 20732113; 10.1016/0887-2333(92)90031-l
#> 72                                                                                                                                                                                                                                                                   Data retrieved by NICEATM from the Environmental Protection Agency's Human Exposure github page in November 2022
#> 73                                                                                                                                                                                                                                                                                                                         Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 74                                                                                                                                                                                                                                                                                                                         Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 75                                                                                                                                                                                                                                                                                                                       Hoffman et al. 2018; 29474128; 10.1080/10408444.2018.1429385
#> 76                                                                                                                                                                                                                                                                                                                       Hoffman et al. 2018; 29474128; 10.1080/10408444.2018.1429385
#> 77                                                                                                                                                                                                                                                                                                                         Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 78                                                                                                                                                                                                                                                                                                                         Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 79                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 80  Kligman 1966; 5924294; 10.1038/jid.1966.160|Greif 1967; Not available; Not available|Magnusson and Kligman 1969; 5774356; 10.1038/jid.1969.42|Basketter et al. 1994; 8045461; 10.1016/0278-6915(94)90112-0|Basketter et al. 1999; 10654593; 10.1016/S0278-6915(99)00112-x|Gerberick et al. 2000; 10684384; 10.1053/ajcd.2000.0003|Akkan et al. 2003; Not available; Not available
#> 81  Kligman 1966; 5924294; 10.1038/jid.1966.160|Greif 1967; Not available; Not available|Magnusson and Kligman 1969; 5774356; 10.1038/jid.1969.42|Basketter et al. 1994; 8045461; 10.1016/0278-6915(94)90112-0|Basketter et al. 1999; 10654593; 10.1016/S0278-6915(99)00112-x|Gerberick et al. 2000; 10684384; 10.1053/ajcd.2000.0003|Akkan et al. 2003; Not available; Not available
#> 82                                                                                                                                                                                                                                                                                                                                                                             TR-470
#> 83                                                                                                                                                                                                                                                                                                                                                                                   
#> 84                                                                                                                                                                                                                                                                                                                                                                                   
#> 85                                                                                                                                                                                                                                                                                                                                                                                   
#> 86                                                                                                                                                                                                                                                                                                                                                                                   
#> 87                                                                                                                                                                                                                                                                   Data retrieved by NICEATM from the Environmental Protection Agency's Human Exposure github page in November 2022
#> 88                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 89                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 90                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 91                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 92                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 93                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 94                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 95                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 96                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 97                                                                                                                                                                                                                                                                                                                           Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 98                                                                                                                                                                                                                                                                                                                                                                               AEGL
#> 99                                                                                                                                                                                                                                                                                                                                                                               AEGL
#> 100                                                                                                                                                                                                                                                                                                                                                                              AEGL
#> 101                                                                                                                                                                                                                                                                                                                                                                              AEGL
#> 102                                                                                                                                                                                                                                                                                                                                                                                  
#> 103                                                                                                                                                                                                                                                                                                                                                                                  
#> 104                                                                                                                                                                                                                                                                                                                                                                                  
#> 105                                                                                                                                                                                                                                                                                                                                                                                  
#> 106                                                                                                                                                                                                                                                                                                                                                                              AEGL
#> 107                                                                                                                                                                                                                                                                                                                                                                              AEGL
#> 108                                                                                                                                                                                                                                                                                                                                                                              AEGL
#> 109                                                                                                                                                                                                                                                                                                                                                                              AEGL
#> 110                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 111                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 112                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 113                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 114                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 115                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 116                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 117                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 118                                                                                                                                                                                                                                                                                                                                                                                  
#> 119                                                                                                                                                                                                                                                                                                                                                                                  
#> 120                                                                                                                                                                                                                                                                                                                                                                                  
#> 121                                                                                                                                                                                                                                                                                                                                                                                  
#> 122                                                                                                                                                                                                                                                                                                                                                                                  
#> 123                                                                                                                                                                                                                                                                                                                                                                                  
#> 124                                                                                                                                                                                                                                                                                                                                                                                  
#> 125                                                                                                                                                                                                                                                                                                                                                                                  
#> 126                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 127                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 128                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 129                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 130                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 131                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 132                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 133                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 134                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 135                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 136                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 137                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 138                                                                                                                                                                                                                                                                                                                                                                                  
#> 139                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 140                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 141                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 142                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 143                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 144                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 145                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 146                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 147                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 148                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 149                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 150                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 151                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 152                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 153                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 154                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 155                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 156                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 157                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 158                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 159                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 160                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 161                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 162                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 163                                                                                                                                                                                                                                                                                                                        Botham et al. 1992; 20732113; 10.1016/0887-2333(92)90031-l
#> 164                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 165                                                                                                                                                                                                                                                                                                                                                                                  
#> 166                                                                                                                                                                                                                                                                                                                        Botham et al. 1992; 20732113; 10.1016/0887-2333(92)90031-l
#> 167                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 168                                                                                                                                                                                                                                                                                                                                                                                  
#> 169                                                                                                                                                                                                                                                                                                                        Botham et al. 1992; 20732113; 10.1016/0887-2333(92)90031-l
#> 170                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 171                                                                                                                                                                                                                                                                                                                                                                                  
#> 172                                                                                                                                                                                                                                                                                                                        Botham et al. 1992; 20732113; 10.1016/0887-2333(92)90031-l
#> 173                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 174                                                                                                                                                                                                                                                                                                                                                                                  
#> 175                                                                                                                                                                                                                                                                                                                        Botham et al. 1992; 20732113; 10.1016/0887-2333(92)90031-l
#> 176                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 177                                                                                                                                                                                                                                                                                                                                                                                  
#> 178                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 179                                                                                                                                                                                                                                                                                                                                                                                  
#> 180                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 181                                                                                                                                                                                                                                                                                                                                                                                  
#> 182                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 183                                                                                                                                                                                                                                                                                                                                                                                  
#> 184                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 185                                                                                                                                                                                                                                                                                                                                                                                  
#> 186                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 187                                                                                                                                                                                                                                                                                                                                                                                  
#> 188                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 189                                                                                                                                                                                                                                                                                                                                                                                  
#> 190                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 191                                                                                                                                                                                                                                                                                                                                                                                  
#> 192                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 193                                                                                                                                                                                                                                                                                                                                                                                  
#> 194                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 195                                                                                                                                                                                                                                                                                                                                                                                  
#> 196                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 197                                                                                                                                                                                                                                                                                                                                                                                  
#> 198                                                                                                                                                                                                                                                                                                                        Botham et al. 1992; 20732113; 10.1016/0887-2333(92)90031-l
#> 199                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 200                                                                                                                                                                                                                                                                                                                                                                                  
#> 201                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 202                                                                                                                                                                                                                                                                                                                                                                                  
#> 203                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 204                                                                                                                                                                                                                                                                                                                                                                                  
#> 205                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 206                                                                                                                                                                                                                                                                                                                                                                                  
#> 207                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 208                                                                                                                                                                                                                                                                                                                                                                                  
#> 209                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 210                                                                                                                                                                                                                                                                                                                                                                                  
#> 211                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 212                                                                                                                                                                                                                                                                                                                                                                                  
#> 213                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 214                                                                                                                                                                                                                                                                                                                                                                                  
#> 215                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 216                                                                                                                                                                                                                                                                                                                                                                                  
#> 217                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 218                                                                                                                                                                                                                                                                                                                                                                                  
#> 219                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 220                                                                                                                                                                                                                                                                                                                                                                                  
#> 221                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 222                                                                                                                                                                                                                                                                                                                                                                                  
#> 223                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 224                                                                                                                                                                                                                                                                                                                                                                                  
#> 225                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 226                                                                                                                                                                                                                                                                                                                                                                                  
#> 227                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 228                                                                                                                                                                                                                                                                                                                                                                                  
#> 229                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 230                                                                                                                                                                                                                                                                                                                                                                                  
#> 231                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 232                                                                                                                                                                                                                                                                                                                                                                                  
#> 233                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 234                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 235                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 236                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 237                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 238                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 239                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 240                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 241                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 242                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 243                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 244 Kligman 1966; 5924294; 10.1038/jid.1966.160|Greif 1967; Not available; Not available|Magnusson and Kligman 1969; 5774356; 10.1038/jid.1969.42|Basketter et al. 1994; 8045461; 10.1016/0278-6915(94)90112-0|Basketter et al. 1999; 10654593; 10.1016/S0278-6915(99)00112-x|Gerberick et al. 2000; 10684384; 10.1053/ajcd.2000.0003|Akkan et al. 2003; Not available; Not available
#> 245 Kligman 1966; 5924294; 10.1038/jid.1966.160|Greif 1967; Not available; Not available|Magnusson and Kligman 1969; 5774356; 10.1038/jid.1969.42|Basketter et al. 1994; 8045461; 10.1016/0278-6915(94)90112-0|Basketter et al. 1999; 10654593; 10.1016/S0278-6915(99)00112-x|Gerberick et al. 2000; 10684384; 10.1053/ajcd.2000.0003|Akkan et al. 2003; Not available; Not available
#> 246 Kligman 1966; 5924294; 10.1038/jid.1966.160|Greif 1967; Not available; Not available|Magnusson and Kligman 1969; 5774356; 10.1038/jid.1969.42|Basketter et al. 1994; 8045461; 10.1016/0278-6915(94)90112-0|Basketter et al. 1999; 10654593; 10.1016/S0278-6915(99)00112-x|Gerberick et al. 2000; 10684384; 10.1053/ajcd.2000.0003|Akkan et al. 2003; Not available; Not available
#> 247                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 248 Kligman 1966; 5924294; 10.1038/jid.1966.160|Greif 1967; Not available; Not available|Magnusson and Kligman 1969; 5774356; 10.1038/jid.1969.42|Basketter et al. 1994; 8045461; 10.1016/0278-6915(94)90112-0|Basketter et al. 1999; 10654593; 10.1016/S0278-6915(99)00112-x|Gerberick et al. 2000; 10684384; 10.1053/ajcd.2000.0003|Akkan et al. 2003; Not available; Not available
#> 249 Kligman 1966; 5924294; 10.1038/jid.1966.160|Greif 1967; Not available; Not available|Magnusson and Kligman 1969; 5774356; 10.1038/jid.1969.42|Basketter et al. 1994; 8045461; 10.1016/0278-6915(94)90112-0|Basketter et al. 1999; 10654593; 10.1016/S0278-6915(99)00112-x|Gerberick et al. 2000; 10684384; 10.1053/ajcd.2000.0003|Akkan et al. 2003; Not available; Not available
#> 250 Kligman 1966; 5924294; 10.1038/jid.1966.160|Greif 1967; Not available; Not available|Magnusson and Kligman 1969; 5774356; 10.1038/jid.1969.42|Basketter et al. 1994; 8045461; 10.1016/0278-6915(94)90112-0|Basketter et al. 1999; 10654593; 10.1016/S0278-6915(99)00112-x|Gerberick et al. 2000; 10684384; 10.1053/ajcd.2000.0003|Akkan et al. 2003; Not available; Not available
#> 251 Kligman 1966; 5924294; 10.1038/jid.1966.160|Greif 1967; Not available; Not available|Magnusson and Kligman 1969; 5774356; 10.1038/jid.1969.42|Basketter et al. 1994; 8045461; 10.1016/0278-6915(94)90112-0|Basketter et al. 1999; 10654593; 10.1016/S0278-6915(99)00112-x|Gerberick et al. 2000; 10684384; 10.1053/ajcd.2000.0003|Akkan et al. 2003; Not available; Not available
#> 252                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 253                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 254                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 255                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 256                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 257                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 258                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 259                                                                                                                                                                                                                                                                                                                           Marzulli and Maibach 1973; Not available; Not available
#> 260                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 261                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 262                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 263                                                                                                                                                                                                                                                                                                                      Hoffman et al. 2018; 29474128; 10.1080/10408444.2018.1429385
#> 264                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 265                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 266                                                                                                                                                                                                                                                                                                                          Bauch et al. 2012; 22659254; 10.1016/j.yrtph.2012.05.013
#> 267                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 268                                                                                                                                                                                                                                                                  Data retrieved by NICEATM from the Environmental Protection Agency's Human Exposure github page in November 2022
#> 269                                                                                                                                                                                                                                                                  Data retrieved by NICEATM from the Environmental Protection Agency's Human Exposure github page in November 2022
#> 270                                                                                                                                                                                                                                                                  Data retrieved by NICEATM from the Environmental Protection Agency's Human Exposure github page in November 2022
#> 271                                                                                                                                                                                                                                                                                                                                                                                  
#> 272                                                                                                                                                                                                                                                                                                                      Hoffman et al. 2018; 29474128; 10.1080/10408444.2018.1429385
#> 273                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 274                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#> 275                                                                                                                                                                                                                                                                                                                        Urbisch et al. 2015; 25541156; 10.1016/j.yrtph.2014.12.008
#>     reference_url        dtxsid                      substance_name pubmed_id
#> 1                 DTXSID7020637                        Formaldehyde          
#> 2                 DTXSID7020637                        Formaldehyde          
#> 3                 DTXSID7020637                        Formaldehyde          
#> 4                 DTXSID7020637                        Formaldehyde          
#> 5                 DTXSID7020637                        Formaldehyde          
#> 6                 DTXSID7020637                        Formaldehyde          
#> 7                 DTXSID7020637                        Formaldehyde          
#> 8                 DTXSID7020637                        Formaldehyde          
#> 9                 DTXSID7020637                        Formaldehyde          
#> 10                DTXSID7020637                        Formaldehyde          
#> 11                DTXSID7020637                        Formaldehyde          
#> 12                DTXSID7020637                        Formaldehyde          
#> 13                DTXSID7020637                        Formaldehyde          
#> 14                DTXSID7020637                        Formaldehyde          
#> 15                DTXSID7020637                        Formaldehyde          
#> 16                DTXSID7020637                        Formaldehyde          
#> 17                DTXSID7020637                        Formaldehyde          
#> 18                DTXSID7020637                        Formaldehyde          
#> 19                DTXSID7020637                        Formaldehyde          
#> 20                DTXSID7020637                        Formaldehyde          
#> 21                DTXSID7020637                        Formaldehyde          
#> 22                DTXSID7020637                        Formaldehyde          
#> 23                DTXSID7020637                        Formaldehyde          
#> 24                DTXSID7020637                        Formaldehyde          
#> 25                DTXSID7020637                        Formaldehyde          
#> 26                DTXSID7020637                        Formaldehyde          
#> 27                DTXSID7020637                        Formaldehyde          
#> 28                DTXSID7020637                        Formaldehyde          
#> 29                DTXSID7020637                        Formaldehyde          
#> 30                DTXSID7020637                        Formaldehyde          
#> 31                DTXSID7020637                        Formaldehyde          
#> 32                DTXSID7020637                        Formaldehyde          
#> 33                DTXSID7020637                        Formaldehyde          
#> 34                DTXSID7020637                        Formaldehyde          
#> 35                DTXSID7020637                        Formaldehyde          
#> 36                DTXSID7020637                        Formaldehyde          
#> 37                DTXSID7020637                        Formaldehyde          
#> 38                DTXSID7020637                        Formaldehyde          
#> 39                DTXSID7020637                        Formaldehyde          
#> 40                DTXSID7020637                        Formaldehyde          
#> 41                DTXSID7020637                        Formaldehyde          
#> 42                DTXSID7020637                        Formaldehyde          
#> 43                DTXSID7020637                        Formaldehyde          
#> 44                DTXSID7020637                        Formaldehyde          
#> 45                DTXSID7020637                        Formaldehyde          
#> 46                DTXSID7020637                        Formaldehyde          
#> 47                DTXSID7020637                        Formaldehyde          
#> 48                DTXSID7020637                        Formaldehyde          
#> 49                DTXSID7020637                        Formaldehyde          
#> 50                DTXSID7020637                        Formaldehyde          
#> 51                DTXSID7020637                        Formaldehyde          
#> 52                DTXSID7020637                        Formaldehyde          
#> 53                DTXSID7020637                        Formaldehyde          
#> 54                DTXSID7020637                        Formaldehyde          
#> 55                DTXSID7020637                        Formaldehyde          
#> 56                DTXSID7020637                        Formaldehyde          
#> 57                DTXSID7020637                        Formaldehyde          
#> 58                DTXSID7020637                        Formaldehyde          
#> 59                DTXSID7020637                        Formaldehyde          
#> 60                DTXSID7020637                        Formaldehyde          
#> 61                DTXSID7020637                        Formaldehyde          
#> 62                DTXSID7020637                        Formaldehyde          
#> 63                DTXSID7020637                        Formaldehyde          
#> 64                DTXSID7020637                        Formaldehyde          
#> 65                DTXSID7020637                        Formaldehyde          
#> 66                              dicyanamine/formaldehyde condensate          
#> 67                              dicyanamine/formaldehyde condensate          
#> 68                              dicyanamine/formaldehyde condensate          
#> 69                              dicyanamine/formaldehyde condensate          
#> 70                              dicyanamine/formaldehyde condensate          
#> 71                              dicyanamine/formaldehyde condensate          
#> 72                DTXSID7020637                        Formaldehyde          
#> 73                DTXSID7020637                        Formaldehyde          
#> 74                DTXSID7020637                        Formaldehyde          
#> 75                DTXSID7020637                        Formaldehyde          
#> 76                DTXSID7020637                        Formaldehyde          
#> 77                DTXSID7020637                        Formaldehyde          
#> 78                DTXSID7020637                        Formaldehyde          
#> 79                DTXSID7020637                        Formaldehyde          
#> 80                DTXSID7020637                        Formaldehyde          
#> 81                DTXSID7020637                        Formaldehyde          
#> 82                DTXSID7020637                        Formaldehyde          
#> 83                DTXSID7020637                        Formaldehyde          
#> 84                DTXSID7020637                        Formaldehyde          
#> 85                DTXSID7020637                        Formaldehyde          
#> 86                DTXSID7020637                        Formaldehyde          
#> 87                DTXSID7020637                        Formaldehyde          
#> 88                DTXSID7020637                        Formaldehyde          
#> 89                DTXSID7020637                        Formaldehyde          
#> 90                DTXSID7020637                        Formaldehyde          
#> 91                DTXSID7020637                        Formaldehyde          
#> 92                DTXSID7020637                        Formaldehyde          
#> 93                DTXSID7020637                        Formaldehyde          
#> 94                DTXSID7020637                        Formaldehyde          
#> 95                DTXSID7020637                        Formaldehyde          
#> 96                DTXSID7020637                        Formaldehyde          
#> 97                DTXSID7020637                        Formaldehyde          
#> 98                DTXSID7020637                        Formaldehyde          
#> 99                DTXSID7020637                        Formaldehyde          
#> 100               DTXSID7020637                        Formaldehyde          
#> 101               DTXSID7020637                        Formaldehyde          
#> 102               DTXSID7020637                        Formaldehyde          
#> 103               DTXSID7020637                        Formaldehyde          
#> 104               DTXSID7020637                        Formaldehyde          
#> 105               DTXSID7020637                        Formaldehyde          
#> 106               DTXSID7020637                        Formaldehyde          
#> 107               DTXSID7020637                        Formaldehyde          
#> 108               DTXSID7020637                        Formaldehyde          
#> 109               DTXSID7020637                        Formaldehyde          
#> 110               DTXSID7020637                        Formaldehyde          
#> 111               DTXSID7020637                        Formaldehyde          
#> 112               DTXSID7020637                        Formaldehyde          
#> 113               DTXSID7020637                        Formaldehyde          
#> 114               DTXSID7020637                        Formaldehyde          
#> 115               DTXSID7020637                        Formaldehyde          
#> 116               DTXSID7020637                        Formaldehyde          
#> 117               DTXSID7020637                        Formaldehyde          
#> 118               DTXSID7020637                        Formaldehyde          
#> 119               DTXSID7020637                        Formaldehyde          
#> 120               DTXSID7020637                        Formaldehyde          
#> 121               DTXSID7020637                        Formaldehyde          
#> 122               DTXSID7020637                        Formaldehyde          
#> 123               DTXSID7020637                        Formaldehyde          
#> 124               DTXSID7020637                        Formaldehyde          
#> 125               DTXSID7020637                        Formaldehyde          
#> 126               DTXSID7020637                        Formaldehyde          
#> 127               DTXSID7020637                        Formaldehyde          
#> 128               DTXSID7020637                        Formaldehyde          
#> 129               DTXSID7020637                        Formaldehyde          
#> 130               DTXSID7020637                        Formaldehyde          
#> 131               DTXSID7020637                        Formaldehyde          
#> 132               DTXSID7020637                        Formaldehyde          
#> 133               DTXSID7020637                        Formaldehyde          
#> 134               DTXSID7020637                        Formaldehyde          
#> 135               DTXSID7020637                        Formaldehyde          
#> 136               DTXSID7020637                        Formaldehyde          
#> 137               DTXSID7020637                        Formaldehyde          
#> 138               DTXSID7020637                        Formaldehyde          
#> 139               DTXSID7020637                        Formaldehyde          
#> 140               DTXSID7020637                        Formaldehyde          
#> 141               DTXSID7020637                        Formaldehyde          
#> 142               DTXSID7020637                        Formaldehyde          
#> 143               DTXSID7020637                        Formaldehyde          
#> 144               DTXSID7020637                        Formaldehyde          
#> 145               DTXSID7020637                        Formaldehyde          
#> 146               DTXSID7020637                        Formaldehyde          
#> 147               DTXSID7020637                        Formaldehyde          
#> 148               DTXSID7020637                        Formaldehyde          
#> 149               DTXSID7020637                        Formaldehyde          
#> 150               DTXSID7020637                        Formaldehyde          
#> 151               DTXSID7020637                        Formaldehyde          
#> 152               DTXSID7020637                        Formaldehyde          
#> 153               DTXSID7020637                        Formaldehyde          
#> 154               DTXSID7020637                        Formaldehyde          
#> 155               DTXSID7020637                        Formaldehyde          
#> 156               DTXSID7020637                        Formaldehyde          
#> 157               DTXSID7020637                        Formaldehyde          
#> 158               DTXSID7020637                        Formaldehyde          
#> 159               DTXSID7020637                        Formaldehyde          
#> 160               DTXSID7020637                        Formaldehyde          
#> 161               DTXSID7020637                        Formaldehyde          
#> 162               DTXSID7020637                        Formaldehyde          
#> 163                             dicyanamine/formaldehyde condensate          
#> 164               DTXSID7020637                        Formaldehyde          
#> 165               DTXSID7020637                        Formaldehyde          
#> 166                             dicyanamine/formaldehyde condensate          
#> 167               DTXSID7020637                        Formaldehyde          
#> 168               DTXSID7020637                        Formaldehyde          
#> 169                             dicyanamine/formaldehyde condensate          
#> 170               DTXSID7020637                        Formaldehyde          
#> 171               DTXSID7020637                        Formaldehyde          
#> 172                             dicyanamine/formaldehyde condensate          
#> 173               DTXSID7020637                        Formaldehyde          
#> 174               DTXSID7020637                        Formaldehyde          
#> 175                             dicyanamine/formaldehyde condensate          
#> 176               DTXSID7020637                        Formaldehyde          
#> 177               DTXSID7020637                        Formaldehyde          
#> 178               DTXSID7020637                        Formaldehyde          
#> 179               DTXSID7020637                        Formaldehyde          
#> 180               DTXSID7020637                        Formaldehyde          
#> 181               DTXSID7020637                        Formaldehyde          
#> 182               DTXSID7020637                        Formaldehyde          
#> 183               DTXSID7020637                        Formaldehyde          
#> 184               DTXSID7020637                        Formaldehyde          
#> 185               DTXSID7020637                        Formaldehyde          
#> 186               DTXSID7020637                        Formaldehyde          
#> 187               DTXSID7020637                        Formaldehyde          
#> 188               DTXSID7020637                        Formaldehyde          
#> 189               DTXSID7020637                        Formaldehyde          
#> 190               DTXSID7020637                        Formaldehyde          
#> 191               DTXSID7020637                        Formaldehyde          
#> 192               DTXSID7020637                        Formaldehyde          
#> 193               DTXSID7020637                        Formaldehyde          
#> 194               DTXSID7020637                        Formaldehyde          
#> 195               DTXSID7020637                        Formaldehyde          
#> 196               DTXSID7020637                        Formaldehyde          
#> 197               DTXSID7020637                        Formaldehyde          
#> 198                             dicyanamine/formaldehyde condensate          
#> 199               DTXSID7020637                        Formaldehyde          
#> 200               DTXSID7020637                        Formaldehyde          
#> 201               DTXSID7020637                        Formaldehyde          
#> 202               DTXSID7020637                        Formaldehyde          
#> 203               DTXSID7020637                        Formaldehyde          
#> 204               DTXSID7020637                        Formaldehyde          
#> 205               DTXSID7020637                        Formaldehyde          
#> 206               DTXSID7020637                        Formaldehyde          
#> 207               DTXSID7020637                        Formaldehyde          
#> 208               DTXSID7020637                        Formaldehyde          
#> 209               DTXSID7020637                        Formaldehyde          
#> 210               DTXSID7020637                        Formaldehyde          
#> 211               DTXSID7020637                        Formaldehyde          
#> 212               DTXSID7020637                        Formaldehyde          
#> 213               DTXSID7020637                        Formaldehyde          
#> 214               DTXSID7020637                        Formaldehyde          
#> 215               DTXSID7020637                        Formaldehyde          
#> 216               DTXSID7020637                        Formaldehyde          
#> 217               DTXSID7020637                        Formaldehyde          
#> 218               DTXSID7020637                        Formaldehyde          
#> 219               DTXSID7020637                        Formaldehyde          
#> 220               DTXSID7020637                        Formaldehyde          
#> 221               DTXSID7020637                        Formaldehyde          
#> 222               DTXSID7020637                        Formaldehyde          
#> 223               DTXSID7020637                        Formaldehyde          
#> 224               DTXSID7020637                        Formaldehyde          
#> 225               DTXSID7020637                        Formaldehyde          
#> 226               DTXSID7020637                        Formaldehyde          
#> 227               DTXSID7020637                        Formaldehyde          
#> 228               DTXSID7020637                        Formaldehyde          
#> 229               DTXSID7020637                        Formaldehyde          
#> 230               DTXSID7020637                        Formaldehyde          
#> 231               DTXSID7020637                        Formaldehyde          
#> 232               DTXSID7020637                        Formaldehyde          
#> 233               DTXSID7020637                        Formaldehyde          
#> 234               DTXSID7020637                        Formaldehyde          
#> 235               DTXSID7020637                        Formaldehyde          
#> 236               DTXSID7020637                        Formaldehyde          
#> 237               DTXSID7020637                        Formaldehyde          
#> 238               DTXSID7020637                        Formaldehyde          
#> 239               DTXSID7020637                        Formaldehyde          
#> 240               DTXSID7020637                        Formaldehyde          
#> 241               DTXSID7020637                        Formaldehyde          
#> 242               DTXSID7020637                        Formaldehyde          
#> 243               DTXSID7020637                        Formaldehyde          
#> 244               DTXSID7020637                        Formaldehyde          
#> 245               DTXSID7020637                        Formaldehyde          
#> 246               DTXSID7020637                        Formaldehyde          
#> 247               DTXSID7020637                        Formaldehyde          
#> 248               DTXSID7020637                        Formaldehyde          
#> 249               DTXSID7020637                        Formaldehyde          
#> 250               DTXSID7020637                        Formaldehyde          
#> 251               DTXSID7020637                        Formaldehyde          
#> 252               DTXSID7020637                        Formaldehyde          
#> 253               DTXSID7020637                        Formaldehyde          
#> 254               DTXSID7020637                        Formaldehyde          
#> 255               DTXSID7020637                        Formaldehyde          
#> 256               DTXSID7020637                        Formaldehyde          
#> 257               DTXSID7020637                        Formaldehyde          
#> 258               DTXSID7020637                        Formaldehyde          
#> 259               DTXSID7020637                        Formaldehyde          
#> 260               DTXSID7020637                        Formaldehyde          
#> 261               DTXSID7020637                        Formaldehyde          
#> 262               DTXSID7020637                        Formaldehyde          
#> 263               DTXSID7020637                        Formaldehyde          
#> 264               DTXSID7020637                        Formaldehyde          
#> 265               DTXSID7020637                        Formaldehyde          
#> 266               DTXSID7020637                        Formaldehyde          
#> 267               DTXSID7020637                        Formaldehyde          
#> 268               DTXSID7020637                        Formaldehyde          
#> 269               DTXSID7020637                        Formaldehyde          
#> 270               DTXSID7020637                        Formaldehyde          
#> 271               DTXSID7020637                        Formaldehyde          
#> 272               DTXSID7020637                        Formaldehyde          
#> 273               DTXSID7020637                        Formaldehyde          
#> 274               DTXSID7020637                        Formaldehyde          
#> 275               DTXSID7020637                        Formaldehyde          
#>       query
#> 1   50-00-0
#> 2   50-00-0
#> 3   50-00-0
#> 4   50-00-0
#> 5   50-00-0
#> 6   50-00-0
#> 7   50-00-0
#> 8   50-00-0
#> 9   50-00-0
#> 10  50-00-0
#> 11  50-00-0
#> 12  50-00-0
#> 13  50-00-0
#> 14  50-00-0
#> 15  50-00-0
#> 16  50-00-0
#> 17  50-00-0
#> 18  50-00-0
#> 19  50-00-0
#> 20  50-00-0
#> 21  50-00-0
#> 22  50-00-0
#> 23  50-00-0
#> 24  50-00-0
#> 25  50-00-0
#> 26  50-00-0
#> 27  50-00-0
#> 28  50-00-0
#> 29  50-00-0
#> 30  50-00-0
#> 31  50-00-0
#> 32  50-00-0
#> 33  50-00-0
#> 34  50-00-0
#> 35  50-00-0
#> 36  50-00-0
#> 37  50-00-0
#> 38  50-00-0
#> 39  50-00-0
#> 40  50-00-0
#> 41  50-00-0
#> 42  50-00-0
#> 43  50-00-0
#> 44  50-00-0
#> 45  50-00-0
#> 46  50-00-0
#> 47  50-00-0
#> 48  50-00-0
#> 49  50-00-0
#> 50  50-00-0
#> 51  50-00-0
#> 52  50-00-0
#> 53  50-00-0
#> 54  50-00-0
#> 55  50-00-0
#> 56  50-00-0
#> 57  50-00-0
#> 58  50-00-0
#> 59  50-00-0
#> 60  50-00-0
#> 61  50-00-0
#> 62  50-00-0
#> 63  50-00-0
#> 64  50-00-0
#> 65  50-00-0
#> 66  50-00-0
#> 67  50-00-0
#> 68  50-00-0
#> 69  50-00-0
#> 70  50-00-0
#> 71  50-00-0
#> 72  50-00-0
#> 73  50-00-0
#> 74  50-00-0
#> 75  50-00-0
#> 76  50-00-0
#> 77  50-00-0
#> 78  50-00-0
#> 79  50-00-0
#> 80  50-00-0
#> 81  50-00-0
#> 82  50-00-0
#> 83  50-00-0
#> 84  50-00-0
#> 85  50-00-0
#> 86  50-00-0
#> 87  50-00-0
#> 88  50-00-0
#> 89  50-00-0
#> 90  50-00-0
#> 91  50-00-0
#> 92  50-00-0
#> 93  50-00-0
#> 94  50-00-0
#> 95  50-00-0
#> 96  50-00-0
#> 97  50-00-0
#> 98  50-00-0
#> 99  50-00-0
#> 100 50-00-0
#> 101 50-00-0
#> 102 50-00-0
#> 103 50-00-0
#> 104 50-00-0
#> 105 50-00-0
#> 106 50-00-0
#> 107 50-00-0
#> 108 50-00-0
#> 109 50-00-0
#> 110 50-00-0
#> 111 50-00-0
#> 112 50-00-0
#> 113 50-00-0
#> 114 50-00-0
#> 115 50-00-0
#> 116 50-00-0
#> 117 50-00-0
#> 118 50-00-0
#> 119 50-00-0
#> 120 50-00-0
#> 121 50-00-0
#> 122 50-00-0
#> 123 50-00-0
#> 124 50-00-0
#> 125 50-00-0
#> 126 50-00-0
#> 127 50-00-0
#> 128 50-00-0
#> 129 50-00-0
#> 130 50-00-0
#> 131 50-00-0
#> 132 50-00-0
#> 133 50-00-0
#> 134 50-00-0
#> 135 50-00-0
#> 136 50-00-0
#> 137 50-00-0
#> 138 50-00-0
#> 139 50-00-0
#> 140 50-00-0
#> 141 50-00-0
#> 142 50-00-0
#> 143 50-00-0
#> 144 50-00-0
#> 145 50-00-0
#> 146 50-00-0
#> 147 50-00-0
#> 148 50-00-0
#> 149 50-00-0
#> 150 50-00-0
#> 151 50-00-0
#> 152 50-00-0
#> 153 50-00-0
#> 154 50-00-0
#> 155 50-00-0
#> 156 50-00-0
#> 157 50-00-0
#> 158 50-00-0
#> 159 50-00-0
#> 160 50-00-0
#> 161 50-00-0
#> 162 50-00-0
#> 163 50-00-0
#> 164 50-00-0
#> 165 50-00-0
#> 166 50-00-0
#> 167 50-00-0
#> 168 50-00-0
#> 169 50-00-0
#> 170 50-00-0
#> 171 50-00-0
#> 172 50-00-0
#> 173 50-00-0
#> 174 50-00-0
#> 175 50-00-0
#> 176 50-00-0
#> 177 50-00-0
#> 178 50-00-0
#> 179 50-00-0
#> 180 50-00-0
#> 181 50-00-0
#> 182 50-00-0
#> 183 50-00-0
#> 184 50-00-0
#> 185 50-00-0
#> 186 50-00-0
#> 187 50-00-0
#> 188 50-00-0
#> 189 50-00-0
#> 190 50-00-0
#> 191 50-00-0
#> 192 50-00-0
#> 193 50-00-0
#> 194 50-00-0
#> 195 50-00-0
#> 196 50-00-0
#> 197 50-00-0
#> 198 50-00-0
#> 199 50-00-0
#> 200 50-00-0
#> 201 50-00-0
#> 202 50-00-0
#> 203 50-00-0
#> 204 50-00-0
#> 205 50-00-0
#> 206 50-00-0
#> 207 50-00-0
#> 208 50-00-0
#> 209 50-00-0
#> 210 50-00-0
#> 211 50-00-0
#> 212 50-00-0
#> 213 50-00-0
#> 214 50-00-0
#> 215 50-00-0
#> 216 50-00-0
#> 217 50-00-0
#> 218 50-00-0
#> 219 50-00-0
#> 220 50-00-0
#> 221 50-00-0
#> 222 50-00-0
#> 223 50-00-0
#> 224 50-00-0
#> 225 50-00-0
#> 226 50-00-0
#> 227 50-00-0
#> 228 50-00-0
#> 229 50-00-0
#> 230 50-00-0
#> 231 50-00-0
#> 232 50-00-0
#> 233 50-00-0
#> 234 50-00-0
#> 235 50-00-0
#> 236 50-00-0
#> 237 50-00-0
#> 238 50-00-0
#> 239 50-00-0
#> 240 50-00-0
#> 241 50-00-0
#> 242 50-00-0
#> 243 50-00-0
#> 244 50-00-0
#> 245 50-00-0
#> 246 50-00-0
#> 247 50-00-0
#> 248 50-00-0
#> 249 50-00-0
#> 250 50-00-0
#> 251 50-00-0
#> 252 50-00-0
#> 253 50-00-0
#> 254 50-00-0
#> 255 50-00-0
#> 256 50-00-0
#> 257 50-00-0
#> 258 50-00-0
#> 259 50-00-0
#> 260 50-00-0
#> 261 50-00-0
#> 262 50-00-0
#> 263 50-00-0
#> 264 50-00-0
#> 265 50-00-0
#> 266 50-00-0
#> 267 50-00-0
#> 268 50-00-0
#> 269 50-00-0
#> 270 50-00-0
#> 271 50-00-0
#> 272 50-00-0
#> 273 50-00-0
#> 274 50-00-0
#> 275 50-00-0
# }
```
