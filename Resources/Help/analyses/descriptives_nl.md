Beschrijvende statistieken
===

Met beschrijvende statistieken kunt u basis beschrijvende statistieken verkrijgen zoals histogrammen, dichtheidsgrafieken, correlatiegrafieken, boxplots en frequentietabellen. 

### Invoer
-------

#### Invoerveld 
- Variabelen: Alle variabelen waarin u geïnteresseerd bent. 
- Splits: Variabelen kunnen worden gesplitst aan de hand van een categorische variabele zoals een experimentele conditie. 
- Frequentietabellen: Geeft frequentietabellen weer voor elke variabele.

### Grafieken 
- Verdelingsgrafieken: Voor een continue variabele wordt een histogram weergegeven. Voor nominale en ordinale variabelen wordt een frequentieverdeling weergegeven.
  - Geef dichtheid weer (alleen continue variabelen): Geeft dichtheid weer op basis van een nonparametrische dichtheidschatter. 
- Correlatiediagram: Geeft voor een continue variabele histogrammen, dichtheidsgrafieken en spreidingsdiagrammen weer. 
- Boxplots: Geeft voor continue variabelen een boxplot weer.
  - Label uitschieters: De uitschieters worden gelabeld. Uitschieters worden gebaseerd op de interkwartielafstand (IQR). Bijv., [25e percentiel] - 1.5 × IQR en [75e percentiel] +  1.5 × IQR.
  - Kleur: Geeft weer in kleur.
	- Heeft selecteerbare boxplot, viool- en jitterelementen voor het weergeven van de verdeling van de data. 

### Statistieken
- Percentielwaarden: 
  - Kwartielen: Geeft het 25e, 50e en 75e percentiel van de datapunten. 
  - Snij punten af voor x gelijke groepen: Geeft de punten aan die de data in x gelijke groepen opdelen; de standaardoptie is 4 gelijke groepen. 
  - Percentielen: Geeft het xde percentiel aan; percentielwaarden moeten met komma's worden gescheiden. 
- Centrale tendens (alleen continue variabelen):
  - Gemiddelde: Rekenkundig gemiddelde van de datapunten.
  - Mediaan: Mediaan van de datapunten.
  - Modus: Modus van de datapunten; als er meerdere modi zijn, wordt slechts de eerste gerapporteerd. 
  - Som: De som van de datapunten. 
- Spreiding (alleen voor continue variabelen): 
  - S.E. Gemiddelde: De standaardfout van het gemiddelde. 
  - Std. Afwijking. De standaarddeviatie van de datapunten. 
  - MAD: Mediaan absolute afwijking van de datapunten. 
  - MAD robuust: Mediaan absolute afwijking van de datapunten, bijgesteld door een factor voor asymptotische normale consistentie. 
  - IQR: Interkwartielafstand van de datapunten; 75e percentiel - 25e percentiel. 
  - Variantie: Variantie van de datapunten. 
  - Bereik: De spreidingsbreedte van de datapunten; maximum - minimum. 
  - Minimum: Minimale waarde van de datapunten. 
  - Maximum: Maximale waarde van de datapunten. 
- Verdeling: 
  - Scheefheid: De scheefheid van de verdeling van de data punten.
  - Gepiektheid: De gepiektheid (kurtosis) van de verdeling van de datapunten. 
  - Shapiro-Wilk toets: Een toets om de normaliteit van de verdeling te toetsen.

### Uitvoer
-------
#### Beschrijvende statistieken
- Geldig: Het aantal geldige waarnemingen. 
- Ontbrekend: Het aantal ontbrekende waarden. 
- Gemiddelde: Rekenkundig gemiddelde van de datapunten.
- Std. Fout van het Gemiddelde: De standaardfout van het gemiddelde. 
- Mediaan: Mediaan van de datapunten.
- Modus: Modus van de datapunten; als er meerdere modi zijn, wordt slechts de eerste gerapporteerd. 
- Standaardafwijking: De standaarddeviatie van de datapunten. 
- MAD: Mediaan absolute afwijking van de datapunten. 
- MAD Robuust: Mediaan absolute afwijking van de datapunten, bijgesteld door een factor voor asymptotische normale consistentie. 
- IQR: Interkwartielafstand van de datapunten; 75e percentiel - 25e percentiel. 
- Variantie: Variantie van de datapunten.
- Scheefheid: De scheefheid van de verdeling van de datapunten.
- Std. Fout van Scheefheid: De standaardfout van de scheefheid. 
- Gepiektheid: De gepiektheid (kurtosis) van de verdeling van de datapunten. 
- Std. Fout van Gepiektheid: De standaardfout van de kurtose. 
- Shapiro-Wilk: waarde van de Shapiro-Wilk statistiek.
- P-waarde van Shapiro-Wilk: p-waarde van de Shapiro-Wilk statistiek.
- Bereik: De spreidingsbreedte van de datapunten; maximum - mimimum. 
- Minimum: Minimale waarde van de datapunten. 
- Maximum: Maximale waarde van de datapunten.
- Som: De som van de datapunten. 
- Percentielen: Geeft het xde percentiel aan. 

#### Verdelingsgrafieken
- Geeft voor continue variabelen een histogram en de passing van een niet parametrische dichtheidsschatting weer. 
- Geeft voor nominale en ordinale variabelen een frequentieverdeling weer. 

#### Correlatie grafiek
- Geeft een matrix van grafieken weer tussen continue variabelen met spreidingsdiagrammen tussen de variabelen in de niet-diagonaal-cellen, en histogrammen en dichtheidsplots in de cellen op de diagonaal. De lijn representeert de passing van een 1e-, 2e-, 3e- of 4e-orde polynomiaal (de selectie is gebaseerd op het Bayesiaanse informatiecriterium ; Schwarz, 1978).

#### Boxplots
- Geeft voor continue variabelen een boxplot weer. De uitschieters kunnen gelabeld worden op basis van de interkwartiel afstand (IQR), bijv., [25e percentiel] - 1.5 × IQR en [75e percentiel] + 1.5 × IQR. Kan ook in kleur worden weergegeven en heeft selecteerbare boxplot-, viool- en jitterelementen voor het weergeven van de verdeling van de data. Dit kan worden gesplitst op basis van een categorische variabele zoals een experimentele conditie. 

#### Frequentietabellen (voor categorische variabelen) 
- Geeft een frequentietabel voor elke variabele weer. 
  - Frequentie: Frequentie van het voorkomen van elke waarde.
  - Percentage: Percentage van voorkomen van elke waarde.
  - Geldig percentage: Percentage van het voorkomen van elke geldige waarde.
  - Cumulatieve percentage: Het cumulatieve percentage. 

### Referenties
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Schwarz, G. (1978). Estimating the dimension of a model. *Annals of Statistics, 6*(2), 461-464.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.

### R Packages
---
- ggplot2
- ggrepel
- grid
- stats
- stringr

