# Ambient Temperature and Dengue Hospitalisation in Brazil over a 10 year period, 2010-2019: a times series analysis

[Rafael Lopes Paixão da Silva<sup>1,2,3,4</sup>](https://orcid.org/0000-0002-9416-6145), 
[Xavier Basagaña<sup>2,3,4</sup>](https://orcid.org/0000-0002-8457-1489), 
[Leonardo S. L. Bastos<sup>5</sup>](https://orcid.org/0000-0001-7833-0403), 
[Fernando A. Bozza<sup>6</sup>](https://orcid.org/0000-0003-4878-0256 ), 
[Otavio T. Ranzani<sup>2,3,4,7</sup>](https://orcid.org/0000-0002-4677-6862)

1 - Instituto de Física Teórica, IFT, UNESP, São Paulo, Brazil; 

2 - Barcelona Institute for Global Health, ISGlobal, Barcelona, Spain; 

3 - Universitat Pompeu Fabra, UPF, Barcelona, Spain; 

4- CIBER Epidemiología y Salud Pública, CIBERESP, Madrid, Spain; 

5 - Department of Industrial Engineering (DEI), Pontifical Catholic University of Rio de Janeiro (PUC-Rio), Rio de Janeiro, RJ, Brazil; 

6 - National Institute of Infectious Disease Evandro Chagas (INI), Oswaldo Cruz Foundation (FIOCRUZ), Rio de Janeiro, RJ, Brazil; 

7 - Pulmonary Division, Heart Institute (InCor), HCFMUSP, Faculdade de Medicina da Universidade de São Paulo, Brazil


**Corresponding author**: 

Rafael Lopes Paixão da Silva [rafael.lp.silva@unesp.br](rafael.lp.silva@unesp.br)
IFT-Unesp, R. Dr. Bento Teobaldo Ferraz, 271, 01140-070 São Paulo, Brazil

Otavio T. Ranzani [otavio.ranzani@isglobal.org](otavio.ranzani@isglobal.org)
ISGlobal, Dr. Aiguader, 88, 08003 Barcelona, Spain

<!--
**Summary**:

**Background**: Climate factors are known to influence seasonal patterns of dengue transmission. However, little is known about the effect of extreme heat on the severity of dengue infection, such as hospital admission. We aimed to quantify the effect of ambient temperature on dengue hospitalisation risk in Brazil.

**Methods**: We retrieved daily dengue hospitalisation counts by each of 5,570 municipalities across the 27 states of Brazil from 1st of January 2010 to 31st of December 2019, from the Brazilian Public Hospital Admission System (“SIH”). We obtained average daily ambient temperature for each municipality from the ERA5-land product reanalysis. We combined distributed lag non-linear models with time stratified design model framework to pool an estimate for dose-response and lag-response structures for the association of Dengue hospitalisation relative risk (RR) and temperature. We estimated the overall dengue hospitalisation RR for the whole country as well as for each of the five macro-regions by meta-analysing state level estimates.

**Findings**: 579,703 hospital admissions due to dengue occurred over the 10 years period of 2010 to 2019. We observed a positive association between high temperatures and high risk of hospitalisation for Brazil and each of the five macro-regions. The overall RR for dengue hospitalisation was at the 50th percentile of temperature distribution 1·25 (95% IC 1·18-1·32) and at 95th percentile of temperature the RR was 1·32 (1·19-1·46) for Brazil, relative to the minimum temperature, which was the one with the lowest risk. We also found lagged effects of heat on hospitalisation, particularly observed on the same day (lag 0) both at the 50th percentile and 95th.

**Interpretation**: High temperatures are associated with an increase in the risk of hospitalisation by dengue infection. These findings may guide preparedness and mitigation policies during dengue season outbreaks, particularly on the health-care demand.

**Funding**: Conselho Nacional de Pesquisa, Coordenação Nacional de Aperfeiçoamento de Pessoal, Institut de Salud Carlos III.
-->


**Codes and data folder Structure**:

Due to the limiting of file size in github repository, the data used to this paper is not available here


    .
    ├── Outputs/                # Folder with Outputs figures and tables for the paper and supplementary material
          .
          ├── Brazil/               # Outputs whole Brazil
          ├── Plots/                # Plots folder
          ├── Region/               # Outputs for each Region
          ├── State/                # Outputs for each State
          ├── Tables/               # Tables Folder
    ├── Code/                   # Scripts Folder
          .
          1_finalizing_database.R
          ├── Fig_Tables            # Figures and Tables codes
          ├── Main                  # Main analysis codes
          ├── S1_var                # S1 var analysis codes
          ├── S2_lag                # S2 lag analysis codes
          ├── S3_cases              # S3 cases analysis codes
          
    ├── Data/                   # This folder is not uploaded due to the size of files, if needed can be sent upon request to authors
    ├── .gitignore 
    ├── README.md
    └── Severe_Dengue.Rproj # .Rproj to help when running the analysis    

# Citation

```
@article{da2022ambient,
  title={Ambient Temperature and Dengue Hospitalisation in Brazil over a 10-year period, 2010-2019: a times series analysis},
  author={da Silva, Rafael Lopes Paixao and Basagana, Xavier and Bastos, Leonardo SL and Bozza, Fernando A and Ranzani, Otavio T},
  journal={medRxiv},
  year={2022},
  publisher={Cold Spring Harbor Laboratory Press}
}
```