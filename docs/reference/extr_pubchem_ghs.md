# Extract GHS Codes from PubChem

This function extracts GHS (Globally Harmonized System) codes from
PubChem. It relies on the `webchem` package to interact with PubChem.

## Usage

``` r
extr_pubchem_ghs(casrn, verbose = TRUE, delay = 0)
```

## Arguments

- casrn:

  Character vector of CAS Registry Numbers (CASRN).

- verbose:

  A logical value indicating whether to print detailed messages. Default
  is TRUE.

- delay:

  A numeric value indicating the delay (in seconds) between API
  requests. This controls the time between successive PubChem queries.
  Default is 0. See Details for more info.

## Value

A dataframe containing GHS information.

## Details

The function performs two queries to PubChem:

1.  The first query retrieves the PubChem Compound Identifier (CID) for
    each IUPAC name.

2.  The second query retrieves additional information using the obtained
    CIDs. In cases of multiple rapid successive requests, the PubChem
    server may deny access. Introducing a delay between requests (using
    the `delay` parameter) can help prevent this issue.

## See also

[PubChem](https://pubchem.ncbi.nlm.nih.gov/)

## Examples

``` r
# \donttest{
extr_pubchem_ghs(casrn = c("50-00-0", "64-17-5"))
#> ℹ Checking Internet Connection...
#> ℹ Internet connection OK...
#> Querying 50-00-0. 
#> OK (HTTP 200).
#> 
#> Querying 712. 
#> OK (HTTP 200).
#> 
#> Querying 64-17-5. 
#> OK (HTTP 200).
#> 
#> Querying 702. 
#> OK (HTTP 200).
#> 
#>     cid   casrn   IUPAC_name
#> 1   712 50-00-0 Formaldehyde
#> 2   712 50-00-0 Formaldehyde
#> 3   712 50-00-0 Formaldehyde
#> 4   712 50-00-0 Formaldehyde
#> 5   712 50-00-0 Formaldehyde
#> 6   712 50-00-0 Formaldehyde
#> 7   712 50-00-0 Formaldehyde
#> 8   712 50-00-0 Formaldehyde
#> 9   712 50-00-0 Formaldehyde
#> 10  712 50-00-0 Formaldehyde
#> 11  712 50-00-0 Formaldehyde
#> 12  712 50-00-0 Formaldehyde
#> 13  712 50-00-0 Formaldehyde
#> 14  712 50-00-0 Formaldehyde
#> 15  712 50-00-0 Formaldehyde
#> 16  712 50-00-0 Formaldehyde
#> 17  712 50-00-0 Formaldehyde
#> 18  712 50-00-0 Formaldehyde
#> 19  712 50-00-0 Formaldehyde
#> 20  712 50-00-0 Formaldehyde
#> 21  712 50-00-0 Formaldehyde
#> 22  712 50-00-0 Formaldehyde
#> 23  712 50-00-0 Formaldehyde
#> 24  712 50-00-0 Formaldehyde
#> 25  712 50-00-0 Formaldehyde
#> 26  712 50-00-0 Formaldehyde
#> 27  712 50-00-0 Formaldehyde
#> 28  712 50-00-0 Formaldehyde
#> 29  712 50-00-0 Formaldehyde
#> 30  712 50-00-0 Formaldehyde
#> 31  712 50-00-0 Formaldehyde
#> 32  712 50-00-0 Formaldehyde
#> 33  712 50-00-0 Formaldehyde
#> 34  712 50-00-0 Formaldehyde
#> 35  712 50-00-0 Formaldehyde
#> 36  712 50-00-0 Formaldehyde
#> 37  712 50-00-0 Formaldehyde
#> 38  712 50-00-0 Formaldehyde
#> 39  712 50-00-0 Formaldehyde
#> 40  712 50-00-0 Formaldehyde
#> 41  712 50-00-0 Formaldehyde
#> 42  712 50-00-0 Formaldehyde
#> 43  712 50-00-0 Formaldehyde
#> 44  712 50-00-0 Formaldehyde
#> 45  712 50-00-0 Formaldehyde
#> 46  712 50-00-0 Formaldehyde
#> 47  712 50-00-0 Formaldehyde
#> 48  712 50-00-0 Formaldehyde
#> 49  712 50-00-0 Formaldehyde
#> 50  712 50-00-0 Formaldehyde
#> 51  712 50-00-0 Formaldehyde
#> 52  712 50-00-0 Formaldehyde
#> 53  712 50-00-0 Formaldehyde
#> 54  712 50-00-0 Formaldehyde
#> 55  712 50-00-0 Formaldehyde
#> 56  712 50-00-0 Formaldehyde
#> 57  712 50-00-0 Formaldehyde
#> 58  712 50-00-0 Formaldehyde
#> 59  712 50-00-0 Formaldehyde
#> 60  712 50-00-0 Formaldehyde
#> 61  712 50-00-0 Formaldehyde
#> 62  712 50-00-0 Formaldehyde
#> 63  712 50-00-0 Formaldehyde
#> 64  712 50-00-0 Formaldehyde
#> 65  712 50-00-0 Formaldehyde
#> 66  712 50-00-0 Formaldehyde
#> 67  712 50-00-0 Formaldehyde
#> 68  712 50-00-0 Formaldehyde
#> 69  712 50-00-0 Formaldehyde
#> 70  712 50-00-0 Formaldehyde
#> 71  712 50-00-0 Formaldehyde
#> 72  712 50-00-0 Formaldehyde
#> 73  712 50-00-0 Formaldehyde
#> 74  712 50-00-0 Formaldehyde
#> 75  712 50-00-0 Formaldehyde
#> 76  712 50-00-0 Formaldehyde
#> 77  712 50-00-0 Formaldehyde
#> 78  712 50-00-0 Formaldehyde
#> 79  712 50-00-0 Formaldehyde
#> 80  712 50-00-0 Formaldehyde
#> 81  712 50-00-0 Formaldehyde
#> 82  712 50-00-0 Formaldehyde
#> 83  712 50-00-0 Formaldehyde
#> 84  712 50-00-0 Formaldehyde
#> 85  712 50-00-0 Formaldehyde
#> 86  712 50-00-0 Formaldehyde
#> 87  712 50-00-0 Formaldehyde
#> 88  712 50-00-0 Formaldehyde
#> 89  712 50-00-0 Formaldehyde
#> 90  712 50-00-0 Formaldehyde
#> 91  712 50-00-0 Formaldehyde
#> 92  712 50-00-0 Formaldehyde
#> 93  712 50-00-0 Formaldehyde
#> 94  702 64-17-5      Ethanol
#> 95  702 64-17-5      Ethanol
#> 96  702 64-17-5      Ethanol
#> 97  702 64-17-5      Ethanol
#> 98  702 64-17-5      Ethanol
#> 99  702 64-17-5      Ethanol
#> 100 702 64-17-5      Ethanol
#> 101 702 64-17-5      Ethanol
#> 102 702 64-17-5      Ethanol
#> 103 702 64-17-5      Ethanol
#> 104 702 64-17-5      Ethanol
#> 105 702 64-17-5      Ethanol
#> 106 702 64-17-5      Ethanol
#> 107 702 64-17-5      Ethanol
#> 108 702 64-17-5      Ethanol
#> 109 702 64-17-5      Ethanol
#> 110 702 64-17-5      Ethanol
#> 111 702 64-17-5      Ethanol
#> 112 702 64-17-5      Ethanol
#> 113 702 64-17-5      Ethanol
#> 114 702 64-17-5      Ethanol
#> 115 702 64-17-5      Ethanol
#> 116 702 64-17-5      Ethanol
#> 117 702 64-17-5      Ethanol
#> 118 702 64-17-5      Ethanol
#> 119 702 64-17-5      Ethanol
#> 120 702 64-17-5      Ethanol
#> 121 702 64-17-5      Ethanol
#> 122 702 64-17-5      Ethanol
#> 123 702 64-17-5      Ethanol
#> 124 702 64-17-5      Ethanol
#> 125 702 64-17-5      Ethanol
#> 126 702 64-17-5      Ethanol
#> 127 702 64-17-5      Ethanol
#> 128 702 64-17-5      Ethanol
#> 129 702 64-17-5      Ethanol
#> 130 702 64-17-5      Ethanol
#> 131 702 64-17-5      Ethanol
#> 132 702 64-17-5      Ethanol
#> 133 702 64-17-5      Ethanol
#> 134 702 64-17-5      Ethanol
#> 135 702 64-17-5      Ethanol
#> 136 702 64-17-5      Ethanol
#> 137 702 64-17-5      Ethanol
#> 138 702 64-17-5      Ethanol
#> 139 702 64-17-5      Ethanol
#> 140 702 64-17-5      Ethanol
#> 141 702 64-17-5      Ethanol
#> 142 702 64-17-5      Ethanol
#> 143 702 64-17-5      Ethanol
#> 144 702 64-17-5      Ethanol
#> 145 702 64-17-5      Ethanol
#> 146 702 64-17-5      Ethanol
#> 147 702 64-17-5      Ethanol
#> 148 702 64-17-5      Ethanol
#> 149 702 64-17-5      Ethanol
#> 150 702 64-17-5      Ethanol
#> 151 702 64-17-5      Ethanol
#> 152 702 64-17-5      Ethanol
#> 153 702 64-17-5      Ethanol
#> 154 702 64-17-5      Ethanol
#> 155 702 64-17-5      Ethanol
#> 156 702 64-17-5      Ethanol
#> 157 702 64-17-5      Ethanol
#> 158 702 64-17-5      Ethanol
#> 159 702 64-17-5      Ethanol
#> 160 702 64-17-5      Ethanol
#> 161 702 64-17-5      Ethanol
#> 162 702 64-17-5      Ethanol
#> 163 702 64-17-5      Ethanol
#> 164 702 64-17-5      Ethanol
#> 165 702 64-17-5      Ethanol
#> 166 702 64-17-5      Ethanol
#> 167 702 64-17-5      Ethanol
#> 168 702 64-17-5      Ethanol
#> 169 702 64-17-5      Ethanol
#> 170 702 64-17-5      Ethanol
#> 171 702 64-17-5      Ethanol
#> 172 702 64-17-5      Ethanol
#> 173 702 64-17-5      Ethanol
#> 174 702 64-17-5      Ethanol
#> 175 702 64-17-5      Ethanol
#> 176 702 64-17-5      Ethanol
#>                                                                                                                                                                                                                                                                                                                                                       result
#> 1                                                                                                                                                                                                                                                                                                                                                           
#> 2                                                                                                                                                                                                                                                                                                                                                     Danger
#> 3                                                                                                                                                                                                                                                                                                  H302: Harmful if swallowed [Warning Acute toxicity, oral]
#> 4                                                                                                                                                                                                                                                                           H314: Causes severe skin burns and eye damage [Danger Skin corrosion/irritation]
#> 5                                                                                                                                                                                                                                                                                    H317: May cause an allergic skin reaction [Warning Sensitization, Skin]
#> 6                                                                                                                                                                                                                                                                                                 H330: Fatal if inhaled [Danger Acute toxicity, inhalation]
#> 7                                                                                                                                                                                                                                                                                H341: Suspected of causing genetic defects [Warning Germ cell mutagenicity]
#> 8                                                                                                                                                                                                                                                                                                            H350: May cause cancer [Danger Carcinogenicity]
#> 9                                                                                                                                 P203, P260, P261, P264, P270, P271, P272, P280, P284, P301+P317, P301+P330+P331, P302+P352, P302+P361+P354, P304+P340, P305+P354+P338, P316, P318, P320, P321, P330, P333+P317, P362+P364, P363, P403+P233, P405, and P501
#> 10                                                                                                                                                                                                                                                                       This chemical does not meet GHS hazard criteria for < 0.1% (3  of 6451) of reports.
#> 11                                                                                                                                                                                                                                                                                                                                                          
#> 12                                                                                                                                                                                                                                                                                                                                                    Danger
#> 13                                                                                                                                                                                                                                                                                            H301 (83.1%): Toxic if swallowed [Danger Acute toxicity, oral]
#> 14                                                                                                                                                                                                                                                                                  H311 (83.1%): Toxic in contact with skin [Danger Acute toxicity, dermal]
#> 15                                                                                                                                                                                                                                                                  H314 (83.1%): Causes severe skin burns and eye damage [Danger Skin corrosion/irritation]
#> 16                                                                                                                                                                                                                                                                           H317 (90.5%): May cause an allergic skin reaction [Warning Sensitization, Skin]
#> 17                                                                                                                                                                                                                                                                        H318 (48.3%): Causes serious eye damage [Danger Serious eye damage/eye irritation]
#> 18                                                                                                                                                                                                                                                                                          H330 (14%): Fatal if inhaled [Danger Acute toxicity, inhalation]
#> 19                                                                                                                                                                                                                                                                                        H331 (77.6%): Toxic if inhaled [Danger Acute toxicity, inhalation]
#> 20                                                                                                                                                                                                                                                                         H341 (19%): Suspected of causing genetic defects [Warning Germ cell mutagenicity]
#> 21                                                                                                                                                                                                                                                                                                     H350 (19%): May cause cancer [Danger Carcinogenicity]
#> 22                                                                                                                                                                                                                                                                                       H351 (64.1%): Suspected of causing cancer [Warning Carcinogenicity]
#> 23                                                                                              P203, P260, P261, P262, P264, P264+P265, P270, P271, P272, P280, P284, P301+P316, P301+P330+P331, P302+P352, P302+P361+P354, P304+P340, P305+P354+P338, P316, P317, P318, P320, P321, P330, P333+P317, P361+P364, P362+P364, P363, P403+P233, P405, and P501
#> 24                                                                                                                                                                          Aggregated GHS information provided per 6451 reports by companies from 102 notifications to the ECHA C&L Inventory. Each notification may be associated with multiple companies.
#> 25                                                                                                                                                                                                                                                                           Reported as not meeting GHS hazard criteria per 3 of 6451 reports by companies.
#> 26                                                                                                                                                                                                                                                  There are 101 notifications provided by 6448 of 6451 reports by companies with hazard statement code(s).
#> 27  Information may vary between notifications depending on impurities, additives, and other factors. The percentage value in parenthesis indicates the notified classification ratio from companies that provide hazard codes. Only hazard codes with percentage values above 10% are shown. For more detailed information, please visit  ECHA C&L website.
#> 28                                                                                                                                                                                                                                                                                                                                                          
#> 29                                                                                                                                                                                                                                                                                                                                                    Danger
#> 30                                                                                                                                                                                                                                                                          H314: Causes severe skin burns and eye damage [Danger Skin corrosion/irritation]
#> 31                                                                                                                                                                                                                                                                                   H317: May cause an allergic skin reaction [Warning Sensitization, Skin]
#> 32                                                                                                                                                                                                                                                                          H401: Toxic to aquatic life [Hazardous to the aquatic environment, acute hazard]
#> 33                                                                                                                                                                                                                                          H412: Harmful to aquatic life with long lasting effects [Hazardous to the aquatic environment, long-term hazard]
#> 34                                                                                                                                                                                          P260, P261, P264, P272, P273, P280, P301+P330+P331, P302+P352, P302+P361+P354, P304+P340, P305+P354+P338, P316, P321, P333+P317, P362+P364, P363, P405, and P501
#> 35                                                                                                                                                                                                                                                                                                                                                          
#> 36                                                                                                                                                                                                                                                                                                                                                    Danger
#> 37                                                                                                                                                                                                                                                                                                    H220: Extremely flammable gas [Danger Flammable gases]
#> 38                                                                                                                                                                                                                                                                   H280: Contains gas under pressure; may explode if heated [Warning Gases under pressure]
#> 39                                                                                                                                                                                                                                                                                                 H302: Harmful if swallowed [Warning Acute toxicity, oral]
#> 40                                                                                                                                                                                                                                                                                          H311: Toxic in contact with skin [Danger Acute toxicity, dermal]
#> 41                                                                                                                                                                                                                                                                                          H315: Causes skin irritation [Warning Skin corrosion/irritation]
#> 42                                                                                                                                                                                                                                                                                   H317: May cause an allergic skin reaction [Warning Sensitization, Skin]
#> 43                                                                                                                                                                                                                                                                           H319: Causes serious eye irritation [Warning Serious eye damage/eye irritation]
#> 44                                                                                                                                                                                                                                                                                                H330: Fatal if inhaled [Danger Acute toxicity, inhalation]
#> 45                                                                                                                                                                                                                                       H334: May cause allergy or asthma symptoms or breathing difficulties if inhaled [Danger Sensitization, respiratory]
#> 46                                                                                                                                                                                                                                                                               H341: Suspected of causing genetic defects [Warning Germ cell mutagenicity]
#> 47                                                                                                                                                                                                                                                                                                           H350: May cause cancer [Danger Carcinogenicity]
#> 48                                                                                                                                                                                                                                                                    H370: Causes damage to organs [Danger Specific target organ toxicity, single exposure]
#> 49                                                                                                                                                                                                                           H372: Causes damage to organs through prolonged or repeated exposure [Danger Specific target organ toxicity, repeated exposure]
#> 50                                                                                                                                                                                                                                                                          H401: Toxic to aquatic life [Hazardous to the aquatic environment, acute hazard]
#> 51                                                                                                                                                                                                                                          H412: Harmful to aquatic life with long lasting effects [Hazardous to the aquatic environment, long-term hazard]
#> 52                                   P203, P210, P222, P233, P260, P261, P262, P264, P264+P265, P270, P271, P272, P273, P280, P284, P301+P317, P302+P352, P304+P340, P305+P351+P338, P308+P316, P316, P318, P319, P320, P321, P330, P332+P317, P333+P317, P337+P317, P342+P316, P361+P364, P362+P364, P377, P381, P403, P403+P233, P405, P410+P403, and P501
#> 53                                                                                                                                                                                                                                                                                                                                                          
#> 54                                                                                                                                                                                                                                                                                                                                                    Danger
#> 55                                                                                                                                                                                                                                                                                                    H220: Extremely flammable gas [Danger Flammable gases]
#> 56                                                                                                                                                                                                                                                                                                      H227: Combustible liquid [Warning Flammable liquids]
#> 57                                                                                                                                                                                                                                                                   H280: Contains gas under pressure; may explode if heated [Warning Gases under pressure]
#> 58                                                                                                                                                                                                                                                                                                 H302: Harmful if swallowed [Warning Acute toxicity, oral]
#> 59                                                                                                                                                                                                                                                                                          H311: Toxic in contact with skin [Danger Acute toxicity, dermal]
#> 60                                                                                                                                                                                                                                                                                          H315: Causes skin irritation [Warning Skin corrosion/irritation]
#> 61                                                                                                                                                                                                                                                                                   H317: May cause an allergic skin reaction [Warning Sensitization, Skin]
#> 62                                                                                                                                                                                                                                                                           H319: Causes serious eye irritation [Warning Serious eye damage/eye irritation]
#> 63                                                                                                                                                                                                                                                                                                H330: Fatal if inhaled [Danger Acute toxicity, inhalation]
#> 64                                                                                                                                                                                                                                       H334: May cause allergy or asthma symptoms or breathing difficulties if inhaled [Danger Sensitization, respiratory]
#> 65                                                                                                                                                                                                                                                                               H341: Suspected of causing genetic defects [Warning Germ cell mutagenicity]
#> 66                                                                                                                                                                                                                                                                                                           H350: May cause cancer [Danger Carcinogenicity]
#> 67                                                                                                                                                                                                                                                                    H370: Causes damage to organs [Danger Specific target organ toxicity, single exposure]
#> 68                                                                                                                                                                                                                           H372: Causes damage to organs through prolonged or repeated exposure [Danger Specific target organ toxicity, repeated exposure]
#> 69                              P203, P210, P222, P233, P260, P261, P262, P264, P264+P265, P270, P271, P272, P280, P284, P301+P317, P302+P352, P304+P340, P305+P351+P338, P308+P316, P316, P318, P319, P320, P321, P330, P332+P317, P333+P317, P337+P317, P342+P316, P361+P364, P362+P364, P370+P378, P377, P381, P403, P403+P233, P405, P410+P403, and P501
#> 70                                                                                                                                                                                                                                                                                                                                                          
#> 71                                                                                                                                                                                                                                                                                                                                                    Danger
#> 72                                                                                                                                                                                                                                                                                                    H301: Toxic if swallowed [Danger Acute toxicity, oral]
#> 73                                                                                                                                                                                                                                                                                          H311: Toxic in contact with skin [Danger Acute toxicity, dermal]
#> 74                                                                                                                                                                                                                                                                          H314: Causes severe skin burns and eye damage [Danger Skin corrosion/irritation]
#> 75                                                                                                                                                                                                                                                                                   H317: May cause an allergic skin reaction [Warning Sensitization, Skin]
#> 76                                                                                                                                                                                                                                                                                                H330: Fatal if inhaled [Danger Acute toxicity, inhalation]
#> 77                                                                                                                                                                                                                                                                                            H350i: May cause cancer by inhalation [Danger Carcinogenicity]
#> 78                                                                                                                                                                                                                                                                          H401: Toxic to aquatic life [Hazardous to the aquatic environment, acute hazard]
#> 79                                                                                                         P203, P260, P261, P262, P264, P270, P271, P272, P273, P280, P284, P301+P316, P301+P330+P331, P302+P352, P302+P361+P354, P304+P340, P305+P354+P338, P316, P318, P320, P321, P330, P333+P317, P361+P364, P362+P364, P363, P403+P233, P405, and P501
#> 80                                                                                                                                                                                                                                                                                                                                                          
#> 81                                                                                                                                                                                                                                                                                                                                                    Danger
#> 82                                                                                                                                                                                                                                                                                                      H227: Combustible liquid [Warning Flammable liquids]
#> 83                                                                                                                                                                                                                                                                                                    H301: Toxic if swallowed [Danger Acute toxicity, oral]
#> 84                                                                                                                                                                                                                                                                                          H311: Toxic in contact with skin [Danger Acute toxicity, dermal]
#> 85                                                                                                                                                                                                                                                                          H314: Causes severe skin burns and eye damage [Danger Skin corrosion/irritation]
#> 86                                                                                                                                                                                                                                                                                   H317: May cause an allergic skin reaction [Warning Sensitization, Skin]
#> 87                                                                                                                                                                                                                                                                                H318: Causes serious eye damage [Danger Serious eye damage/eye irritation]
#> 88                                                                                                                                                                                                                                                                                                H331: Toxic if inhaled [Danger Acute toxicity, inhalation]
#> 89                                                                                                                                                                                                                                                                               H341: Suspected of causing genetic defects [Warning Germ cell mutagenicity]
#> 90                                                                                                                                                                                                                                                                                                           H350: May cause cancer [Danger Carcinogenicity]
#> 91                                                                                                                                                                                                                                                                    H370: Causes damage to organs [Danger Specific target organ toxicity, single exposure]
#> 92                                                                                                                                                                                                                                                                        H402: Harmful to aquatic life [Hazardous to the aquatic environment, acute hazard]
#> 93                                                                  P203, P210, P260, P261, P262, P264, P264+P265, P270, P271, P272, P273, P280, P301+P316, P301+P330+P331, P302+P352, P302+P361+P354, P304+P340, P305+P354+P338, P308+P316, P316, P317, P318, P321, P330, P333+P317, P361+P364, P362+P364, P363, P370+P378, P403, P403+P233, P405, and P501
#> 94                                                                                                                                                                                                                                                                                                                                                          
#> 95                                                                                                                                                                                                                                                                                                                                                    Danger
#> 96                                                                                                                                                                                                                                                                                        H225: Highly Flammable liquid and vapor [Danger Flammable liquids]
#> 97                                                                                                                                                                                                                                                                  P210, P233, P240, P241, P242, P243, P280, P303+P361+P353, P370+P378, P403+P235, and P501
#> 98                                                                                                                                                                                                                                                                      This chemical does not meet GHS hazard criteria for < 0.1% (5  of 13886) of reports.
#> 99                                                                                                                                                                                                                                                                                                                                                          
#> 100                                                                                                                                                                                                                                                                                                                                                   Danger
#> 101                                                                                                                                                                                                                                                                             H225 (> 99.9%): Highly Flammable liquid and vapor [Danger Flammable liquids]
#> 102                                                                                                                                                                                                                                                                  H319 (37.7%): Causes serious eye irritation [Warning Serious eye damage/eye irritation]
#> 103                                                                                                                                                                                                                           P210, P233, P240, P241, P242, P243, P264+P265, P280, P303+P361+P353, P305+P351+P338, P337+P317, P370+P378, P403+P235, and P501
#> 104                                                                                                                                                                         Aggregated GHS information provided per 13886 reports by companies from 79 notifications to the ECHA C&L Inventory. Each notification may be associated with multiple companies.
#> 105                                                                                                                                                                                                                                                                         Reported as not meeting GHS hazard criteria per 5 of 13886 reports by companies.
#> 106                                                                                                                                                                                                                                                There are 76 notifications provided by 13881 of 13886 reports by companies with hazard statement code(s).
#> 107 Information may vary between notifications depending on impurities, additives, and other factors. The percentage value in parenthesis indicates the notified classification ratio from companies that provide hazard codes. Only hazard codes with percentage values above 10% are shown. For more detailed information, please visit  ECHA C&L website.
#> 108                                                                                                                                                                                                                                                                                                                                                         
#> 109                                                                                                                                                                                                                                                                                                                                                  Warning
#> 110                                                                                                                                                                                                                                                                   H319 (100%): Causes serious eye irritation [Warning Serious eye damage/eye irritation]
#> 111                                                                                                                                                                                                                                  H412 (100%): Harmful to aquatic life with long lasting effects [Hazardous to the aquatic environment, long-term hazard]
#> 112                                                                                                                                                                                                                                                                                               P264+P265, P273, P280, P305+P351+P338, P337+P317, and P501
#> 113                                                                                                                                                                                                                                                                 The GHS information provided by 1 company from 1 notification to the ECHA C&L Inventory.
#> 114                                                                                                                                                                                                                                                                                                                                                         
#> 115                                                                                                                                                                                                                                                                                                                                                  Warning
#> 116                                                                                                                                                                                                                                                                                  H315 (100%): Causes skin irritation [Warning Skin corrosion/irritation]
#> 117                                                                                                                                                                                                                                                                   H319 (100%): Causes serious eye irritation [Warning Serious eye damage/eye irritation]
#> 118                                                                                                                                                                                                                                  H412 (100%): Harmful to aquatic life with long lasting effects [Hazardous to the aquatic environment, long-term hazard]
#> 119                                                                                                                                                                                                                                                  P264, P264+P265, P273, P280, P302+P352, P305+P351+P338, P321, P332+P317, P337+P317, P362+P364, and P501
#> 120                                                                                                                                                                                                                                                                 The GHS information provided by 1 company from 1 notification to the ECHA C&L Inventory.
#> 121                                                                                                                                                                                                                                                                                                                                                         
#> 122                                                                                                                                                                                                                                                                                                                                                  Warning
#> 123                                                                                                                                                                                                                                                                                  H315 (100%): Causes skin irritation [Warning Skin corrosion/irritation]
#> 124                                                                                                                                                                                                                                                                   H319 (100%): Causes serious eye irritation [Warning Serious eye damage/eye irritation]
#> 125                                                                                                                                                                                                                                  H412 (100%): Harmful to aquatic life with long lasting effects [Hazardous to the aquatic environment, long-term hazard]
#> 126                                                                                                                                                                                                                                                  P264, P264+P265, P273, P280, P302+P352, P305+P351+P338, P321, P332+P317, P337+P317, P362+P364, and P501
#> 127                                                                                                                                                                                                                                                                 The GHS information provided by 1 company from 1 notification to the ECHA C&L Inventory.
#> 128                                                                                                                                                                                                                                                                                                                                                         
#> 129                                                                                                                                                                                                                                                                                                                                                  Warning
#> 130                                                                                                                                                                                                                                                                                  H315 (100%): Causes skin irritation [Warning Skin corrosion/irritation]
#> 131                                                                                                                                                                                                                                                                   H319 (100%): Causes serious eye irritation [Warning Serious eye damage/eye irritation]
#> 132                                                                                                                                                                                                                                  H412 (100%): Harmful to aquatic life with long lasting effects [Hazardous to the aquatic environment, long-term hazard]
#> 133                                                                                                                                                                                                                                                  P264, P264+P265, P273, P280, P302+P352, P305+P351+P338, P321, P332+P317, P337+P317, P362+P364, and P501
#> 134                                                                                                                                                                                                                                                                 The GHS information provided by 1 company from 1 notification to the ECHA C&L Inventory.
#> 135                                                                                                                                                                                                                                                                                                                                                         
#> 136                                                                                                                                                                                                                                                                                                                                                   Danger
#> 137                                                                                                                                                                                                                                                                                       H225: Highly Flammable liquid and vapor [Danger Flammable liquids]
#> 138                                                                                                                                                                                                                                                                                  H320: Causes eye irritation [Warning Serious eye damage/eye irritation]
#> 139                                                                                                                                                                                                                           H335: May cause respiratory irritation [Warning Specific target organ toxicity, single exposure; Respiratory tract irritation]
#> 140                                                                                                                                                                                                                                      H336: May cause drowsiness or dizziness [Warning Specific target organ toxicity, single exposure; Narcotic effects]
#> 141                                                                                                                                                                                                                                                                                                          H350: May cause cancer [Danger Carcinogenicity]
#> 142                                                                                                                                                                                                                                                                            H360: May damage fertility or the unborn child [Danger Reproductive toxicity]
#> 143                                                                                                                                                                                                                          H372: Causes damage to organs through prolonged or repeated exposure [Danger Specific target organ toxicity, repeated exposure]
#> 144                                                                                                                                                                                                                     H373: May causes damage to organs through prolonged or repeated exposure [Warning Specific target organ toxicity, repeated exposure]
#> 145                                                                                                                                               P203, P210, P233, P240, P241, P242, P243, P260, P261, P264, P264+P265, P270, P271, P280, P303+P361+P353, P304+P340, P305+P351+P338, P318, P319, P337+P317, P370+P378, P403+P233, P403+P235, P405, and P501
#> 146                                                                                                                                                                                                                                                                                                                                                         
#> 147                                                                                                                                                                                                                                                                                                                                                   Danger
#> 148                                                                                                                                                                                                                                                                                       H225: Highly Flammable liquid and vapor [Danger Flammable liquids]
#> 149                                                                                                                                                                                                                                                                                  H320: Causes eye irritation [Warning Serious eye damage/eye irritation]
#> 150                                                                                                                                                                                                                           H335: May cause respiratory irritation [Warning Specific target organ toxicity, single exposure; Respiratory tract irritation]
#> 151                                                                                                                                                                                                                                                                                          H340: May cause genetic defects [Danger Germ cell mutagenicity]
#> 152                                                                                                                                                                                                                                                                            H360: May damage fertility or the unborn child [Danger Reproductive toxicity]
#> 153                                                                                                                                                                                                                          H372: Causes damage to organs through prolonged or repeated exposure [Danger Specific target organ toxicity, repeated exposure]
#> 154                                                                                                                                                                                                                     H373: May causes damage to organs through prolonged or repeated exposure [Warning Specific target organ toxicity, repeated exposure]
#> 155                                                                                                                                               P203, P210, P233, P240, P241, P242, P243, P260, P261, P264, P264+P265, P270, P271, P280, P303+P361+P353, P304+P340, P305+P351+P338, P318, P319, P337+P317, P370+P378, P403+P233, P403+P235, P405, and P501
#> 156                                                                                                                                                                                                                                                                                                                                                         
#> 157                                                                                                                                                                                                                                                                                                                                                   Danger
#> 158                                                                                                                                                                                                                                                                                       H225: Highly Flammable liquid and vapor [Danger Flammable liquids]
#> 159                                                                                                                                                                                                                                                                          H319: Causes serious eye irritation [Warning Serious eye damage/eye irritation]
#> 160                                                                                                                                                                                                                           H335: May cause respiratory irritation [Warning Specific target organ toxicity, single exposure; Respiratory tract irritation]
#> 161                                                                                                                                                                                                                                      H336: May cause drowsiness or dizziness [Warning Specific target organ toxicity, single exposure; Narcotic effects]
#> 162                                                                                                                                                                                                                                                                                          H340: May cause genetic defects [Danger Germ cell mutagenicity]
#> 163                                                                                                                                                                                                                                                                            H360: May damage fertility or the unborn child [Danger Reproductive toxicity]
#> 164                                                                                                                                                                                                                          H372: Causes damage to organs through prolonged or repeated exposure [Danger Specific target organ toxicity, repeated exposure]
#> 165                                                                                                                                                                                                                     H373: May causes damage to organs through prolonged or repeated exposure [Warning Specific target organ toxicity, repeated exposure]
#> 166                                                                                                                                               P203, P210, P233, P240, P241, P242, P243, P260, P261, P264, P264+P265, P270, P271, P280, P303+P361+P353, P304+P340, P305+P351+P338, P318, P319, P337+P317, P370+P378, P403+P233, P403+P235, P405, and P501
#> 167                                                                                                                                                                                                                                                                                                                                                         
#> 168                                                                                                                                                                                                                                                                                                                                                   Danger
#> 169                                                                                                                                                                                                                                                                                       H225: Highly Flammable liquid and vapor [Danger Flammable liquids]
#> 170                                                                                                                                                                                                                                                                          H319: Causes serious eye irritation [Warning Serious eye damage/eye irritation]
#> 171                                                                                                                                                                                                                           P210, P233, P240, P241, P242, P243, P264+P265, P280, P303+P361+P353, P305+P351+P338, P337+P317, P370+P378, P403+P235, and P501
#> 172                                                                                                                                                                                                                                                                                                                                                         
#> 173                                                                                                                                                                                                                                                                                                                                                   Danger
#> 174                                                                                                                                                                                                                                                                                       H225: Highly Flammable liquid and vapor [Danger Flammable liquids]
#> 175                                                                                                                                                                                                                                                                          H319: Causes serious eye irritation [Warning Serious eye damage/eye irritation]
#> 176                                                                                                                                                                                                                           P210, P233, P240, P241, P242, P243, P264+P265, P280, P303+P361+P353, P305+P351+P338, P337+P317, P370+P378, P403+P235, and P501
#>                                                                    source_name
#> 1   Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 2   Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 3   Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 4   Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 5   Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 6   Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 7   Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 8   Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 9   Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 10                                            European Chemicals Agency (ECHA)
#> 11                                            European Chemicals Agency (ECHA)
#> 12                                            European Chemicals Agency (ECHA)
#> 13                                            European Chemicals Agency (ECHA)
#> 14                                            European Chemicals Agency (ECHA)
#> 15                                            European Chemicals Agency (ECHA)
#> 16                                            European Chemicals Agency (ECHA)
#> 17                                            European Chemicals Agency (ECHA)
#> 18                                            European Chemicals Agency (ECHA)
#> 19                                            European Chemicals Agency (ECHA)
#> 20                                            European Chemicals Agency (ECHA)
#> 21                                            European Chemicals Agency (ECHA)
#> 22                                            European Chemicals Agency (ECHA)
#> 23                                            European Chemicals Agency (ECHA)
#> 24                                            European Chemicals Agency (ECHA)
#> 25                                            European Chemicals Agency (ECHA)
#> 26                                            European Chemicals Agency (ECHA)
#> 27                                            European Chemicals Agency (ECHA)
#> 28                                                                    NITE-CMC
#> 29                                                                    NITE-CMC
#> 30                                                                    NITE-CMC
#> 31                                                                    NITE-CMC
#> 32                                                                    NITE-CMC
#> 33                                                                    NITE-CMC
#> 34                                                                    NITE-CMC
#> 35                                                                    NITE-CMC
#> 36                                                                    NITE-CMC
#> 37                                                                    NITE-CMC
#> 38                                                                    NITE-CMC
#> 39                                                                    NITE-CMC
#> 40                                                                    NITE-CMC
#> 41                                                                    NITE-CMC
#> 42                                                                    NITE-CMC
#> 43                                                                    NITE-CMC
#> 44                                                                    NITE-CMC
#> 45                                                                    NITE-CMC
#> 46                                                                    NITE-CMC
#> 47                                                                    NITE-CMC
#> 48                                                                    NITE-CMC
#> 49                                                                    NITE-CMC
#> 50                                                                    NITE-CMC
#> 51                                                                    NITE-CMC
#> 52                                                                    NITE-CMC
#> 53                                                                    NITE-CMC
#> 54                                                                    NITE-CMC
#> 55                                                                    NITE-CMC
#> 56                                                                    NITE-CMC
#> 57                                                                    NITE-CMC
#> 58                                                                    NITE-CMC
#> 59                                                                    NITE-CMC
#> 60                                                                    NITE-CMC
#> 61                                                                    NITE-CMC
#> 62                                                                    NITE-CMC
#> 63                                                                    NITE-CMC
#> 64                                                                    NITE-CMC
#> 65                                                                    NITE-CMC
#> 66                                                                    NITE-CMC
#> 67                                                                    NITE-CMC
#> 68                                                                    NITE-CMC
#> 69                                                                    NITE-CMC
#> 70           Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 71           Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 72           Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 73           Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 74           Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 75           Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 76           Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 77           Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 78           Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 79           Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 80                                       Hazardous Substances Data Bank (HSDB)
#> 81                                       Hazardous Substances Data Bank (HSDB)
#> 82                                       Hazardous Substances Data Bank (HSDB)
#> 83                                       Hazardous Substances Data Bank (HSDB)
#> 84                                       Hazardous Substances Data Bank (HSDB)
#> 85                                       Hazardous Substances Data Bank (HSDB)
#> 86                                       Hazardous Substances Data Bank (HSDB)
#> 87                                       Hazardous Substances Data Bank (HSDB)
#> 88                                       Hazardous Substances Data Bank (HSDB)
#> 89                                       Hazardous Substances Data Bank (HSDB)
#> 90                                       Hazardous Substances Data Bank (HSDB)
#> 91                                       Hazardous Substances Data Bank (HSDB)
#> 92                                       Hazardous Substances Data Bank (HSDB)
#> 93                                       Hazardous Substances Data Bank (HSDB)
#> 94  Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 95  Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 96  Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 97  Regulation (EC) No 1272/2008 of the European Parliament and of the Council
#> 98                                            European Chemicals Agency (ECHA)
#> 99                                            European Chemicals Agency (ECHA)
#> 100                                           European Chemicals Agency (ECHA)
#> 101                                           European Chemicals Agency (ECHA)
#> 102                                           European Chemicals Agency (ECHA)
#> 103                                           European Chemicals Agency (ECHA)
#> 104                                           European Chemicals Agency (ECHA)
#> 105                                           European Chemicals Agency (ECHA)
#> 106                                           European Chemicals Agency (ECHA)
#> 107                                           European Chemicals Agency (ECHA)
#> 108                                           European Chemicals Agency (ECHA)
#> 109                                           European Chemicals Agency (ECHA)
#> 110                                           European Chemicals Agency (ECHA)
#> 111                                           European Chemicals Agency (ECHA)
#> 112                                           European Chemicals Agency (ECHA)
#> 113                                           European Chemicals Agency (ECHA)
#> 114                                           European Chemicals Agency (ECHA)
#> 115                                           European Chemicals Agency (ECHA)
#> 116                                           European Chemicals Agency (ECHA)
#> 117                                           European Chemicals Agency (ECHA)
#> 118                                           European Chemicals Agency (ECHA)
#> 119                                           European Chemicals Agency (ECHA)
#> 120                                           European Chemicals Agency (ECHA)
#> 121                                           European Chemicals Agency (ECHA)
#> 122                                           European Chemicals Agency (ECHA)
#> 123                                           European Chemicals Agency (ECHA)
#> 124                                           European Chemicals Agency (ECHA)
#> 125                                           European Chemicals Agency (ECHA)
#> 126                                           European Chemicals Agency (ECHA)
#> 127                                           European Chemicals Agency (ECHA)
#> 128                                           European Chemicals Agency (ECHA)
#> 129                                           European Chemicals Agency (ECHA)
#> 130                                           European Chemicals Agency (ECHA)
#> 131                                           European Chemicals Agency (ECHA)
#> 132                                           European Chemicals Agency (ECHA)
#> 133                                           European Chemicals Agency (ECHA)
#> 134                                           European Chemicals Agency (ECHA)
#> 135                                                                   NITE-CMC
#> 136                                                                   NITE-CMC
#> 137                                                                   NITE-CMC
#> 138                                                                   NITE-CMC
#> 139                                                                   NITE-CMC
#> 140                                                                   NITE-CMC
#> 141                                                                   NITE-CMC
#> 142                                                                   NITE-CMC
#> 143                                                                   NITE-CMC
#> 144                                                                   NITE-CMC
#> 145                                                                   NITE-CMC
#> 146                                                                   NITE-CMC
#> 147                                                                   NITE-CMC
#> 148                                                                   NITE-CMC
#> 149                                                                   NITE-CMC
#> 150                                                                   NITE-CMC
#> 151                                                                   NITE-CMC
#> 152                                                                   NITE-CMC
#> 153                                                                   NITE-CMC
#> 154                                                                   NITE-CMC
#> 155                                                                   NITE-CMC
#> 156                                                                   NITE-CMC
#> 157                                                                   NITE-CMC
#> 158                                                                   NITE-CMC
#> 159                                                                   NITE-CMC
#> 160                                                                   NITE-CMC
#> 161                                                                   NITE-CMC
#> 162                                                                   NITE-CMC
#> 163                                                                   NITE-CMC
#> 164                                                                   NITE-CMC
#> 165                                                                   NITE-CMC
#> 166                                                                   NITE-CMC
#> 167          Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 168          Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 169          Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 170          Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 171          Hazardous Chemical Information System (HCIS), Safe Work Australia
#> 172                                      Hazardous Substances Data Bank (HSDB)
#> 173                                      Hazardous Substances Data Bank (HSDB)
#> 174                                      Hazardous Substances Data Bank (HSDB)
#> 175                                      Hazardous Substances Data Bank (HSDB)
#> 176                                      Hazardous Substances Data Bank (HSDB)
#>                source_id other   query
#> 1           605-001-00-5    NA 50-00-0
#> 2           605-001-00-5    NA 50-00-0
#> 3           605-001-00-5    NA 50-00-0
#> 4           605-001-00-5    NA 50-00-0
#> 5           605-001-00-5    NA 50-00-0
#> 6           605-001-00-5    NA 50-00-0
#> 7           605-001-00-5    NA 50-00-0
#> 8           605-001-00-5    NA 50-00-0
#> 9           605-001-00-5    NA 50-00-0
#> 10                 55163    NA 50-00-0
#> 11                 55163    NA 50-00-0
#> 12                 55163    NA 50-00-0
#> 13                 55163    NA 50-00-0
#> 14                 55163    NA 50-00-0
#> 15                 55163    NA 50-00-0
#> 16                 55163    NA 50-00-0
#> 17                 55163    NA 50-00-0
#> 18                 55163    NA 50-00-0
#> 19                 55163    NA 50-00-0
#> 20                 55163    NA 50-00-0
#> 21                 55163    NA 50-00-0
#> 22                 55163    NA 50-00-0
#> 23                 55163    NA 50-00-0
#> 24                 55163    NA 50-00-0
#> 25                 55163    NA 50-00-0
#> 26                 55163    NA 50-00-0
#> 27                 55163    NA 50-00-0
#> 28  R04_C_056_JNIOSH,MOE    NA 50-00-0
#> 29  R04_C_056_JNIOSH,MOE    NA 50-00-0
#> 30  R04_C_056_JNIOSH,MOE    NA 50-00-0
#> 31  R04_C_056_JNIOSH,MOE    NA 50-00-0
#> 32  R04_C_056_JNIOSH,MOE    NA 50-00-0
#> 33  R04_C_056_JNIOSH,MOE    NA 50-00-0
#> 34  R04_C_056_JNIOSH,MOE    NA 50-00-0
#> 35             H29_B_039    NA 50-00-0
#> 36             H29_B_039    NA 50-00-0
#> 37             H29_B_039    NA 50-00-0
#> 38             H29_B_039    NA 50-00-0
#> 39             H29_B_039    NA 50-00-0
#> 40             H29_B_039    NA 50-00-0
#> 41             H29_B_039    NA 50-00-0
#> 42             H29_B_039    NA 50-00-0
#> 43             H29_B_039    NA 50-00-0
#> 44             H29_B_039    NA 50-00-0
#> 45             H29_B_039    NA 50-00-0
#> 46             H29_B_039    NA 50-00-0
#> 47             H29_B_039    NA 50-00-0
#> 48             H29_B_039    NA 50-00-0
#> 49             H29_B_039    NA 50-00-0
#> 50             H29_B_039    NA 50-00-0
#> 51             H29_B_039    NA 50-00-0
#> 52             H29_B_039    NA 50-00-0
#> 53                    69    NA 50-00-0
#> 54                    69    NA 50-00-0
#> 55                    69    NA 50-00-0
#> 56                    69    NA 50-00-0
#> 57                    69    NA 50-00-0
#> 58                    69    NA 50-00-0
#> 59                    69    NA 50-00-0
#> 60                    69    NA 50-00-0
#> 61                    69    NA 50-00-0
#> 62                    69    NA 50-00-0
#> 63                    69    NA 50-00-0
#> 64                    69    NA 50-00-0
#> 65                    69    NA 50-00-0
#> 66                    69    NA 50-00-0
#> 67                    69    NA 50-00-0
#> 68                    69    NA 50-00-0
#> 69                    69    NA 50-00-0
#> 70                  4750    NA 50-00-0
#> 71                  4750    NA 50-00-0
#> 72                  4750    NA 50-00-0
#> 73                  4750    NA 50-00-0
#> 74                  4750    NA 50-00-0
#> 75                  4750    NA 50-00-0
#> 76                  4750    NA 50-00-0
#> 77                  4750    NA 50-00-0
#> 78                  4750    NA 50-00-0
#> 79                  4750    NA 50-00-0
#> 80                   164    NA 50-00-0
#> 81                   164    NA 50-00-0
#> 82                   164    NA 50-00-0
#> 83                   164    NA 50-00-0
#> 84                   164    NA 50-00-0
#> 85                   164    NA 50-00-0
#> 86                   164    NA 50-00-0
#> 87                   164    NA 50-00-0
#> 88                   164    NA 50-00-0
#> 89                   164    NA 50-00-0
#> 90                   164    NA 50-00-0
#> 91                   164    NA 50-00-0
#> 92                   164    NA 50-00-0
#> 93                   164    NA 50-00-0
#> 94          603-002-00-5    NA 64-17-5
#> 95          603-002-00-5    NA 64-17-5
#> 96          603-002-00-5    NA 64-17-5
#> 97          603-002-00-5    NA 64-17-5
#> 98                 49769    NA 64-17-5
#> 99                 49769    NA 64-17-5
#> 100                49769    NA 64-17-5
#> 101                49769    NA 64-17-5
#> 102                49769    NA 64-17-5
#> 103                49769    NA 64-17-5
#> 104                49769    NA 64-17-5
#> 105                49769    NA 64-17-5
#> 106                49769    NA 64-17-5
#> 107                49769    NA 64-17-5
#> 108               277502    NA 64-17-5
#> 109               277502    NA 64-17-5
#> 110               277502    NA 64-17-5
#> 111               277502    NA 64-17-5
#> 112               277502    NA 64-17-5
#> 113               277502    NA 64-17-5
#> 114               277529    NA 64-17-5
#> 115               277529    NA 64-17-5
#> 116               277529    NA 64-17-5
#> 117               277529    NA 64-17-5
#> 118               277529    NA 64-17-5
#> 119               277529    NA 64-17-5
#> 120               277529    NA 64-17-5
#> 121               277530    NA 64-17-5
#> 122               277530    NA 64-17-5
#> 123               277530    NA 64-17-5
#> 124               277530    NA 64-17-5
#> 125               277530    NA 64-17-5
#> 126               277530    NA 64-17-5
#> 127               277530    NA 64-17-5
#> 128               277898    NA 64-17-5
#> 129               277898    NA 64-17-5
#> 130               277898    NA 64-17-5
#> 131               277898    NA 64-17-5
#> 132               277898    NA 64-17-5
#> 133               277898    NA 64-17-5
#> 134               277898    NA 64-17-5
#> 135              25B0007    NA 64-17-5
#> 136              25B0007    NA 64-17-5
#> 137              25B0007    NA 64-17-5
#> 138              25B0007    NA 64-17-5
#> 139              25B0007    NA 64-17-5
#> 140              25B0007    NA 64-17-5
#> 141              25B0007    NA 64-17-5
#> 142              25B0007    NA 64-17-5
#> 143              25B0007    NA 64-17-5
#> 144              25B0007    NA 64-17-5
#> 145              25B0007    NA 64-17-5
#> 146              21B3016    NA 64-17-5
#> 147              21B3016    NA 64-17-5
#> 148              21B3016    NA 64-17-5
#> 149              21B3016    NA 64-17-5
#> 150              21B3016    NA 64-17-5
#> 151              21B3016    NA 64-17-5
#> 152              21B3016    NA 64-17-5
#> 153              21B3016    NA 64-17-5
#> 154              21B3016    NA 64-17-5
#> 155              21B3016    NA 64-17-5
#> 156                  662    NA 64-17-5
#> 157                  662    NA 64-17-5
#> 158                  662    NA 64-17-5
#> 159                  662    NA 64-17-5
#> 160                  662    NA 64-17-5
#> 161                  662    NA 64-17-5
#> 162                  662    NA 64-17-5
#> 163                  662    NA 64-17-5
#> 164                  662    NA 64-17-5
#> 165                  662    NA 64-17-5
#> 166                  662    NA 64-17-5
#> 167                 1888    NA 64-17-5
#> 168                 1888    NA 64-17-5
#> 169                 1888    NA 64-17-5
#> 170                 1888    NA 64-17-5
#> 171                 1888    NA 64-17-5
#> 172                   82    NA 64-17-5
#> 173                   82    NA 64-17-5
#> 174                   82    NA 64-17-5
#> 175                   82    NA 64-17-5
#> 176                   82    NA 64-17-5
# }
```
