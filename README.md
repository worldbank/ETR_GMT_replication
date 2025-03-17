# Effective Tax Rates, Firm Size and the Global Minimum Tax
## Pierre Bachas, Anne Brockmeyer, Roel Dom and Camille Semelet.

### Working Papers:
#### <a href="https://cepr.org/system/files/publication-files/DP17985.pdf">CEPR Working paper, March 2023</a>
#### <a href=""> Updated, April 2025 (TBC)</a>

### Abstract:
We document new facts on corporate taxation and the revenue potential of corporate minimum
taxes, leveraging firm-level tax returns from 16 countries. First, effective taxes rates
(ETRs) follow a humped-shaped pattern with firm size: small firms benefit from reduced rates,
while large firms take up tax incentives, leaving mid-sized firms with the highest ETRs. On average,
the ETR for the largest 1% of firms is 2.2 percentage points lower than the average ETR
for top decile firms. Second, although statutory tax rates are above 15% in all sample countries,
over a quarter of top firms face an ETR below 15%, challenging the simple tax haven
vs non-haven dichotomy. Third, a simple 15% domestic minimum tax for the top 1% firms
could raise corporate taxes by 14% on average across countries, absent behavioral responses.
In contrast, the global minimum top-up tax would only raise a quarter of this revenue due to
its generous deductions and a smaller number of firms in scope.  

### Replication codes:
The study uses confidential administrative tax data that cannot be shared publicly. 
However, we provide the replications codes of the study so that the analysis can be run on another country based on Corporate Income Tax returns by tax administrations or other researchers with access to such data. 
 

#### Descriptions of the codes to be adapted:
- The data is cleaned using the do file cleaning_template.do
- Once the data is cleaned, use the 0_master_ETR_GMT.R Script. This script will successively call the other scripts and produce the output
- Several adjustments need to be made to run the file 

#### Questions? <a href="mailto:semelet@ifo.de">Contact here</a>
