Gelijkwaardigheids Bayesiaanse Gepaarde T-Toets
===

Met de gelijkwaardigheids Beyeisaanse gepaarde t-toets kan de gebruiker de effectgrootte schatten en de nulhypothese testen dat het populatiegemiddelde van het verschil tussen gepaarde waarnemingen binnen een interval valt, de zogenaamde gelijkwaardigheids regio.
Het verschil met de klassieke nul hypothese significantie toets is dat in gelijkwaardigheids toetsen, de nul hypothese een interval rond de test waarde beslaat in plaats van een punt-nul.

### Assumpties
- De verschilscore is continu.
- De verschilscores komen uit een aselecte steekproef uit de populatie.
- De verschilscore is normaal verdeeld in de populatie.

### Invoer
---

#### Invoerveld
- Variabelen: In deze box zijn de variabelen geselecteerd waarvoor het verschil is berekend. Meerdere verschillen kunnen tegelijkertijd worden geanalyseerd door het specificeren van verschillende rijen met twee variabelen waarvoor het verschil is berekend. Met andere woorden, iedere rij geeft andere verschilscores weer.

#### Gelijkwaardigheids regio
- Bovengrens: De bovengrens van de gelijkswaardigheids regio.
- Ondergrens: De ondergrens van de gelijkswaardigheids regio.

#### Grafieken
- Prior en posterior: Geeft de verdeling van de prior en posterior van de effectgrootte onder de alternatieve hypothese.
  - Aanvullende informatie: Voegt de Bayes Factor berekend met de door de gebruiker gedefinieerde prior toe; voegt een kanswiel toe die de kans van de data onder de nulhypothese vs. de alternatieve hypothese laat zien; voegt de mediaan en de 95% geloofwaardigheidsinterval van de verdeling van de posterior van de effectgrootte toe.
- Sequentiële analyse: Geeft de ontwikkeling van de Bayes factor weer terwijl de data binnenkomen, gebruik makende van de door de gebruiker gedefinieerde prior.
  - Robuustheidscheck: Voegt de resultaten van de sequentiële analyse toe, gebruik makende van de brede (scale=1) en ultrabrede prior (scale=sqrt(2)).

#### Aanvullende Statistieken
  - Beschrijvend: Steekproefgrootte, steekproefgemiddelde, steekproefstandaarddeviatie, standaardfout van het gemiddelde voor elke maat.

#### Ontbrekende Waarden
- Het uitsluiten van gevallen, analyse voor analyse: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke test uitgevoerd met alle gevallen die valide data bevatten voor de afhankelijke variabele in de t-toets. De steekproefgroottes kunnen daardoor verschillen per toets.
- Het uitsluiten van gevallen, lijstgewijs: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke t-toets uitgevoerd met enkel de gevallen die valide data voor alle afhankelijke variabelen bevatten. De steekproefgrootte is daardoor hetzelfde over alle toetsen.

### Prior
- Standaard:
  - Cauchy: Schaal van de Cauchy prior voor de effectgrootte onder de alternatieve hypothese; de standaardoptie is .707.
- Geinformeerd:
  - Cauchy: Schaal en locatie.
  - Normaal: Gemiddelde en standaardafwijking.
  - Student's t: Schaal, locatie en vrijheidsgraden (df).

### Uitvoer
---
#### Gelijkwaardigheids Bayesiaanse Gepaarde T-Toets
- De eerste kolom bevat de twee variabelen waarvoor het verschil is berekend.
- Model Vergelijking:
  - $\delta$ $\in$ I vs. H1: De Bayes factor die bewijs geeft voor de gelijkwaardigheidsinterval hypothese ten opzichte van de ongelimiteerde alternative hypothese.
  - $\delta$ $\notin$ I vs. H1: De Bayes factor die bewijs geeft voor de hypothese dat de effect grootte buiten het gelijkwaardigheidsinterval ligt ten opzichte van de ongelimiteerde alternative hypothese.
  - $\delta$ $\in$ I vs. $\delta$ $\notin$ I: De Bayes factor die bewijs geeft voor de gelijkwaardigheidsinterval hypothese ten opzichte van e hypothese dat de effect grootte buiten het gelijkwaardigheidsinterval ligt.
  - $\delta$ $\notin$ I vs. $\delta$ $\in$ I: De Bayes factor die bewijs geeft voor de hypothese dat de effect grootte buiten het gelijkwaardigheidsinterval ligt ten opzichte van de gelijkwaardigheidsinterval hypothese.
- BF: De Bayes factor.
- fout %: De fout van de Gaussiaanse kwadratuur intergratie methode die wordt gebruikt op de Bayes factor te berekenen.

#### Beschrijvende Statistiek
- De eerste kolom bevat the afhankelijke variabele.
- N: De steekproefgrootte.
- Gemiddelde: Het steekproefgemiddelde.
- SD: Standaarddeviatie van het gemiddelde.
- SE: Standaardfout van het gemiddelde.
- Geloofwaardigheidsinterval: Standaardoptie is 95%.

#### Grafieken
- Prior en posterior: Geeft de prior (stippellijn) en posterior (lijn) verdeling van de effectgrootte onder de alternatieve hypothese; het grijze gebied geeft de gelijkwaardigheids regio van de dichtheid van de prior en de posterior weer. De horizontale lijn geeft de breedte van een 95% geloofwaardigheidsinterval van de posterior verdeling weer.
  - Aanvullende info: Geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior weer; geeft een kanswiel weer met de kans van de data onder de gelijkwaardigheidsinterval hypothese ten opzichte van de non-gelijkwaardigheidsinterval hypothese; geeft de mediaan en het 95% geloofwaardigheidsinterval van de posterior verdeling weergeeft.
- Sequentiële analyse: Geeft de ontwikkeling van de Bayes factor weer, als een functie van het aantal datapunten (n), gebruik makenende van de door de gebruiker gedefinieerde prior; geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior weer; geeft een kanswiel weer die de kans van de data onder de gelijkwaardigheidsinterval hypothese ten opzichte van de non-gelijkwaardigheidsinterval hypothese weergeeft; geeft de mediaan en de 95% geloofwaardigheidsinterval van de verdeling van de posterior weer; laat zien hoe besluitvaardig het bewijs is met Jeffreys' (1961) bewijscategorieën.
  - Robuustheidscheck: Geeft de ontwikkeling van de Bayes factor als een functie van het aantal datapunten (n), met de brede en ultrabrede verdeling van de prior. De zwarte cirkel geeft de Bayes factor berekend met een brede prior verdeling weer; de witte cirkel geeft de Bayes factor berekend met een ultrabrede prior verdeling weer; de grijze cirkel geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior verdeling weer.

### Referenties
---
- Morey, R. D., & Rouder, J. N. (2011). Bayes factor approaches for testing interval null hypotheses. *Psychological methods*, 16(4), 406. <a href="https://psycnet.apa.org/buy/2011-15467-001">https://psycnet.apa.org/buy/2011-15467-001</a>
- Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (in press). Informed Bayesian t-tests. *The American Statistician*. <a href="https://arxiv.org/abs/1704.02479">https://arxiv.org/abs/1704.02479</a>
- Jeffreys, H. (1961).  *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.

### R-pakketten
---
- stats
- metaBMA
