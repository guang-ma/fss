# Financial Statement Similarity (FSS)" Data File
<p>The 'FSS' measure proposed by Brown, Ma, and Tucker(2023). <a href ="https://doi.org/10.1111/1911-3846.12885">'Financial statement similarity'</a>. Contemporary Accounting Research, 40(4), 2577–2615. </p>

- Data Format: Stata

- Data Range: Fiscal Year 1997-2021

# firm-year version (transformation at 3-digit SIC industry)
- Frequency: Firm-Year

- Variables: GVKEY, FYEAR (fiscal year), FSS
- <a href='fss.dta'>Download Here</a>

# firmpair version (transformation at 3-digit SIC industry)
- Frequency: Firm i-j pair, Year

- Variables: SICH3 (3-digit historical SIC code), FYEAR (fiscal year), GVKEY_i, GVKEY_j, FSS_Pair
- <a href='fss_pair.7z'>Download Here</a>
# firmpair version (transformation at 2-digit SIC industry)
- Frequency: Firm i-j pair, Year

- Variables: SICH2 (2-digit historical SIC code), FYEAR (fiscal year), GVKEY_i, GVKEY_j, FSS_Pair
- <a href='fss_pair_SIC2.7z'>Download Here</a>
# coding guidance for calculating FSS in R
- <a href='fss.R'>Download Here</a>
