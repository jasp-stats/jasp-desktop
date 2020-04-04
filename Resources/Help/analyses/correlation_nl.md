Correlatie Matrix
===

De Correlatie Matrix maakt het mogelijk de populatiecorrelatie te schatten en de nulhypothese dat de populatiecorrelatie tussen paren van variabelen gelijk is aan 0, te toetsen. Alle mogelijke paren van de gespecificeerde variabelen worden geanalyseerd.


### Assumpties (Pearson's rho)
- Continue variabelen.
- De data zijn een aselecte steekproef van de populatie.
- De paren van variabelen hebben een bivariate normale verdeling in de populatie.
- De relatie tussen de paren van variabelen is lineair.


### Assumpties (Spearman's rho & Kendall's tau)
- Ordinale of continue variabelen.
- De data zijn een aselecte steekproef van de populatie.
- De relatie tussen de paren van variabelen is monotoon.

### Invoer
---

#### Steekproef Correlatiecoëfficiënt
- Pearson's r: Pearson's productmoment correlatiecoëfficiënt.
- Spearman: Spearman's rangorde correlatiecoëfficiënt om de monotone associatie tussen twee variabelen te kwantificeren.
- Kendall's tau-b: Kendall's tau-b rangorde correlatiecoëfficiënt om de monotone associatie tussen twee variabelen te kwantificeren.

#### Alt. Hypotheses
- Correlatie: Tweezijdige alternatieve hypothese dat de populatiecorrelatie niet gelijk is aan 0.
- Positieve correlatie: Eénzijdige alternatieve hypothese dat de populatiecorrelatie hoger is dan 0.
- Negatieve correlatie: Eénzijdige alternatieve hypothese dat de populatiecorrelatie lager is dan 0.

#### Aanvullende Opties
- Geef paarsgewijs weer: Geef een tabel weer waarin op elke rij één paar variabelen staat, en de spreidingsdiagrammen worden individueel voor ieder paar weergegeven. Indien onaangevinkt, dan zijn de resultaten weergegeven als matrix, met de namen van variabelen in zowel de kolommen als de rijen.
- Rapporteer significantie: Geef de p-waarde weer, overeenkomend met de geobserveerde correlatie.
- Markeer significante correlaties: Markeer statistisch significante correlaties.
- Betrouwbaarheidsintervallen: Betrouwbaarheidsintervallen voor de populatiecorrelatie (enkel beschikbaar voor de Pearson correlatie).
  - Interval: Dekking van het betrouwbaarheidsinterval in percentages.
- Vovk-Selke maximum p-ratio: De grens 1/(-e p log(p)) is berekend aan de hand van de p-waarde verdeling. Onder de nullhypothese (H<sub>0</sub>) is het uniform (0,1), en onder de alternatieve (H<sub>1</sub>) verlaagt de p-waarde, e.g., een beta (α, 1) verdeling, met 0 < α < 1. De Vovk-Sellke MPR is verkregen door de vorm α van de verdeling te kiezen onder H1 zodat de behaalde p-waarde diagnostisch gemaximaliseerd is. De waarde is dan de ratio van de verdelingen op punt p onder H<sub>0</sub> en H<sub>1</sub>. Bijvoorbeeld, als de tweezijdige p-waarde gelijk is aan .05, de Vovk-Sellke MPR gelijk is aam 2.46, dit geeft aan dat deze p-waarde is hoogstens 2.46 keer meer aannemelijk is voor H<sub>1</sub> dan voor H<sub>0</sub>.
- Steekproefgrootte: Het aantal volledige observaties voor een gegeven paar variabelen.

#### Grafieken
- Spreidingsdiagram: Geef een spreidingsdiagram weer voor elke mogelijke combinatie van de geselecteerde variabelen. Deze worden in matrix formaat weergegeven boven het diagonaal.
  - Verdelingen van variabelen: Toon histogram en de overeenkomende verdelingsplot voor elke variabele. Deze worden in matrix formaat weergegeven op de diagonaal.
  - Statistieken: Geef de correlatiecoëfficiënt(en) weer in de grafiek. Deze optie voegt ook de x% betrouwbaarheidsinterval(len) toe zoals gespecificeerd in de "Betrouwbaarheidsintervallen" optie.
- Heatmap: Geef een correlatie heatmap weer voor Pearson, Spearman, and Kendall's tau B coëfficiënten gescheiden.

#### Verificatie van aannames

- Multivariate normaliteit
  - Shapiro: Berekend de Shapiro-Wilk statistiek om de nulhypothese dat de geselecteerde variabelen een multivariate normale verdeling hebben, te toetsen.

- Paarsgewijze normaliteit
  - Shapiro: Voor elke mogelijke combinatie van de geselecteerde variabelen berekent de Shapiro-Wilk statistiek om de nulhypothese te toetsen dat de paar variabelen een bivariate normale verdeling hebben.

#### Opties

- Ontbrekende waarden
  - Sluit paarsgewijs waarnemingen uit: Gebruik alle complete observaties for elk individuele paar van variabelen.
  - Sluit lijstgewijs waarnemingen uit: Gebruik enkel volledige waarnemingen van alle variabelen.

### Uitvoer
---
#### Correlatietabel
- Pearson r: Pearson's product-moment correlatiecoëfficiënt.
- Spearman rho: Spearman's rank correlatiecoëfficiënt.
- Kendall tau:  Kendall's tau b rank correlatiecoëfficiënt.
- p: De p-waarde.
  - Significante correlaties indien gemarkeerd met:
	- *p < 0.05 als de correlatie significant is op het alfa=.05 niveau.
	- **p < .01 als de correlatie significant is op het alfa=.01 niveau.
	- ***p < .001 als de correlatie significant is op het alfa=.001 niveau.
- Vovk-Sellke Maximum *p*-Ratio: Voor een uitleg, zie Vovk-Sellke onder `Opties`.
- Boven x% BI: Bovenste grens van de x% betrouwbaarheidsinterval voor de populatiecorrelatie.
- Onder x% BI: Onderste grens of the x% betrouwbaarheidsinterval voor de populatiecorrelatie.
- n: Steekproefgrootte.

#### Verificatie van aannames

- Shapiro-Wilk: De Shapiro-Wilk statistiek
- p: De p-waarde van de assumptiecheck

#### Correlatie Grafiek
- Spreidingsdiagram: Geeft een (matrix van) spreidingsdiagram(men) weer tussen de variabelen (in de bovenste niet-diagonale vakken van de matrix). De zwarte lijn is de kleinste-kwadraten regressielijn.
    - Verdelingen van variabelen: Geeft een histogram en de bijbehorende verdelingsgrafiek weer voor elke variabele.
    - Statistieken: Geeft de correlatiecoëfficiënt(en) weer en, indien gevraagd, ook de overeenkomende x% betrouwbaarheidsinterval(len).
      - r: Pearson's product-moment correlatiecoëfficiënt
      - rho: Spearman's order correlatiecoëfficiënt
      - tau: Kendall's tau b order correlatiecoëfficiënt

#### Heatmap
- Geef de correlatie heatmap weer voor Pearson, Spearman, and Kendall's tau gescheiden. De heatmap is symmetrisch ten opzichte van het diagonaal. Blauwe kleuren komen overeen met de positieve correlatiecoëfficiënten, rode kleuren komen overeen met negatieve correlatiecoëfficiënten. De kleurverzadiging weerspiegelt de absolute waarde van de correlatiecoëfficiënt. Als "Markeer significante correlaties" geselecteerd is, dan zijn de significante correlaties gemarkeerd met:
  - *p < 0.05 als de correlatie significant is op het alpha=.05 niveau.
  - **p < .01 als de correlatie significant is op het alpha=.01 niveau.
  - ***p < .001 als de correlatie significant is op het alpha=.001 niveau.

### Referenties
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.

### R Packages
---
- ggplot2
- grid
- stats
- ppcor
