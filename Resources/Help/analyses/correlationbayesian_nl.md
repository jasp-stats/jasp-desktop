Bayesiaanse Correlatie
===

De Bayesiaanse Correlatie analyse maakt het mogelijk om de populatiecorrelatie te schatten en om hypothesen te vergelijken. De drie hypothesevergelijkingen zijn: (i) tussen de nulhypothese (H0) dat de populatiecorrelatie tussen paren van variabelen gelijk is aan 0 en de alternatieve hypothese (H1) dat de populatiecorrelatie een (onbekende) waarde aanneemt tussen -1 en 1, (ii) tussen H0 en de alternatieve hypothese (H+) dat de populatiecorrelatie positief is, en (iii) tussen H0 en de alternatieve hypothese (H-) dat de populatie correlatie negatief is. Alle mogelijke paren van de gespecificeerde variabelen worden geanalyseerd.

### Assumpties (Pearson's rho)
- Continue variabelen.
- De data zijn een aselecte steekproef van de populatie.
- De paren van variabelen hebben een bivariate normale verdeling in de populatie.
- De relatie tussen de paren van variabelen is lineair.

### Assumpties (Kendall's tau)
- Ordinale of continue variabelen.
- De data zijn een aselecte steekproef van de populatie.
- De relatie tussen de paren van variabelen is monotoon.

### Invoer
---

#### Populatie Correlatiecoëfficiënt
- Pearson's rho: Pearson's productmoment correlatiecoëfficiënt.
- Kendall's tau-b: Kendall's tau-b rangorde correlatiecoëfficiënt om de monotone associatie tussen twee variabelen te kwantificeren.

#### Alt. Hypotheses
- Correlatie: Tweezijdige alternatieve hypothese dat de populatiecorrelatie niet gelijk is aan 0.
- Positieve correlatie: Eénzijdige alternatieve hypothese dat de populatiecorrelatie hoger is dan 0.
- Negatieve correlatie: Eénzijdige alternatieve hypothese dat de populatiecorrelatie lager is dan 0.

#### Bayes Factor  
- BF<sub>10</sub>: Als u deze optie selecteert, geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese. Dit is de standaardoptie.
- BF<sub>01</sub>: Als u deze optie selecteert, geeft de Bayes factor bewijs voor de nulhypothese ten opzichte van de alternatieve hypothese. Dit is gelijk aan 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : Als u deze optie selecteert, wordt het natuurlijke logaritme <sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusie</sub>, BF<sub>10, U</sub> weergegeven in de uitvoer.

#### Aanvullende Opties
- Geef tabel paarsgewijs weer: Geef een tabel weer waarin op elke rij één paar variabelen staat. Indien onaangevinkt, dan zijn de resultaten weergegeven als matrix, met de namen van variabelen in zowel de kolommen als de rijen.
- Rapporteer Bayes factoren: Geeft de Bayes factor voor iedere toets.
- Markeer ondersteunende correlaties: Markeer correlatiecoëfficiënten met een Bayes factor groter dan 10, 30, en 100.
- Steekproefgrootte: Geeft de grootte van de steekproef voor iedere toets.
- Geloofwaardigheidsinterval: Geeft het geloofwaardigheidsinterval weer voor de correlatiecoëfficiënt.

#### Grafieken
- Correlatie matrix: Geeft correlatiematrices weer voor iedere mogelijke combinatie van de geselecteerde variabelen. Deze staan boven de diagonaal.
  - Verdelingen van variabelen: Geeft een histogram en de overeenkomende verdelingsgrafieken weer voor elke variabele. Deze staan op de diagonaal.
  - Posteriors onder H<sub>1</sub>: Geeft posterior vergelijkingen van de correlatiecoëfficiënt weer voor iedere mogelijke combinatie van de geselecteerde variabelen. Deze staan onder de diagonaal.

#### Prior
- Gespreide beta prior breedte: Breedte van de geschaalde beta verdeling op de correlatie onder de alternatieve hypothese; de standaardwaarde is 1. Hoe lager deze waarde, hoe meer de prior verdeling is geconcentreerd rond 0. De waarde moet tussen de 0 en de 2 liggen.

### Maak Grafieken Van Losse Paren
- Correlatiecoëfficiënt in grafieken
  - Pearson's rho: Pearson's productmoment correlatiecoëfficiënt.
  - Kendall's tau-b: Kendall's tau-b rangorde correlatiecoëfficiënt om de monotone associatie tussen twee variabelen te kwantificeren.

- Spreidingsdiagram: Geef een spreidingsdiagram weer voor elk paar geselecteerde variabelen.

- Prior en posterior: Geeft de dichtheid van de prior en de posterior van de effectgrootte onder de alternatieve hypothese.
  - Schatting info: Geeft de mediaan en het 95% geloofwaardigheidsinterval van de posterior weer.
  - Toetsing info: Voegt de Bayes factor berekend met de door de gebruiker gedefinieerde prior toe; voegt een kanswiel toe die de kans van de data onder de nulhypothese vs. de alternatieve hypothese laat zien; voegt twee cirkels toe voor de prior en posterior op het toetsingspunt (rho=0), de ratio van de twee punten is gelijk aan de Bayes factor.
- Bayes factor robuustheidsgrafiek: Geeft de Bayes factor weer als een functie van de gerekte beta prior breedte voor de correlatie tussen de paren. De schaal van kappa varieert tussen 0 en 2.
  - Aanvullende informatie: Voegt de Bayes factor berekend met de door de gebruiker gedefinieerde prior toe en de maximaal haalbare Bayes factor.

- Sequentiële analyse: Geeft de ontwikkeling van de Bayes factor weer terwijl de data binnenkomen, gebruik makende van de door de gebruiker gedefinieerde prior.
  - Aanvullende info: Voegt de Bayes factor berekend met de door de gebruiker gedefinieerde prior toe; voegt een kanswiel toe die de kans van de data onder de nulhypothese vs. de alternatieve hypothese laat zien; geeft de interpretatie van het bewijs in termen van Jeffreys' (1961) bewijscategorieën.

### Opties
- Ontbrekende waarden:
  - Sluit paarsgewijs waarnemingen uit.
  - Sluit lijstgewijs waarnemingen uit.
- Reproduceerbaarheid:
  - Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse.

### Uitvoer
---

#### Bayesiaanse Correlatie Tabel
- n: steekproefgrootte.
- Pearson r: Pearson's product-moment correlatiecoëfficiënt.
- Kendall tau:  Kendall's tau b rank correlatiecoëfficiënt.
- BF10 (of BF01): Bayes factor. Als een eenzijdige toets wordt opgevraagd:
  - BF+0: Bayes factor die bewijs geeft voor de eenzijdige alternatieve hypothese dat de populatiecorrelatie hoger is dan 0.
  - BF-0: Bayes factor die bewijs geeft voor de eenzijdige alternatieve hypothese dat de populatiecorrelatie lager is dan 0.
  - BF0+: Bayes factor die bewijs geeft voor de nulhypothese ten opzichte van de eenzijdige alternatieve hypothese dat de populatiecorrelatie hoger is dan 0.
  - BF0-: Bayes factor die bewijs geeft voor de nulhypothese ten opzichte van de eenzijdige alternatieve hypothese dat de populatiecorrelatie lager is dan 0.
- Geloofwaardigheidsinterval: Centrale geloofwaardigheidsinterval voor de correlatiecoëfficiënt.

- Markeer ondersteunde correlaties: Correlaties die worden ondersteund door de Bayes factor zijn gemarkeerd met (zie Jeffreys (1961) voor bewijscategorieën):
	- *BF < 10 als de data minimaal 10 keer aannemelijker zijn onder de gekozen hypothese (zie Hypothese).
	- **BF < 30 als de data minimaal 30 keer aannemelijker zijn onder de gekozen hypothese (zie Hypothese).
	- ***BF < 100 als de data minimaal 100 keer aannemelijker zijn onder de gekozen hypothese (zie Hypothese).

#### Bayesiaanse Correlatie Grafiek
- Correlatiematrix: Geeft een (matrix van) spreidingsdiagram(men) weer tussen de variabelen (in de bovenste niet-diagonale vakken van de matrix). De zwarte lijn is de kleinste-kwadraten regressielijn.
    - Verdeling van variabelen: Geeft een histogram en de bijbehorende verdelingsgrafiek weer voor elke variabele in de diagonale delen van de matrix.
    - Posteriors onder H1: Geeft (een) verdelingsgrafiek(en) weer van de posterior van de correlatie(s) onder de alternatieve hypothese in de lagere niet-diagonale delen van de matrix.

#### Paarsgewijze Bayesiaanse Correlatie Grafieken
- Spreidingsdiagram:
  - Geeft de spreidingsdiagrammen weer voor elk paar geselecteerde variabelen.

- Prior en posterior:
  - Geeft de dichtheid van de prior (stippellijn) en de posterior (lijn) van de effectgrootte onder de alternatieve hypothese; de horizontale lijn representeert de breedte van een 95% geloofwaardigheidsinterval van de posterior.
    - Schatting info: Geeft de mediaan en het 95% geloofwaardigheidsinterval van de posterior weer.
    - Toetsing info: Voegt de Bayes factor berekend met de door de gebruiker gedefinieerde prior toe; voegt een kanswiel toe die de kans van de data onder de nulhypothese vs. de alternatieve hypothese laat zien; voegt twee cirkels toe voor de prior en posterior op het toetsingspunt (rho=0), de ratio van de twee punten is gelijk aan de Bayes factor.


- Bayes factor robuustheidsgrafiek:
  - Geeft de Bayes factor weer als een functie van de beta prior breedte voor de correlatie tussen de paren.
    - Aanvullende informatie: De rode cirkel is de maximaal haalbare Bayes factor; de grijze cirkel is de Bayes factor berekend met de door de gebruiker gedefinieerde prior.

- Sequentiële analyse:
  - Geeft de ontwikkeling van de Bayes factor weer als een functie van het aantal observaties (n) op basis van de door de gebruiker gedefinieerde prior.
    - Aanvullende info: Voegt de Bayes factor berekend met de door de gebruiker gedefinieerde prior toe; voegt een kanswiel toe die de kans van de data onder de nulhypothese vs. de alternatieve hypothese laat zien; geeft de interpretatie van het bewijs in termen van Jeffreys' (1961) bewijscategorieën.

### Referenties
---
- Jeffreys, H. (1961). *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Ly, A., Verhagen, A. J., & Wagenmakers, E.-J. (2016). Harold Jeffreys's default Bayes factor hypothesis tests: Explanation, extension, and application in psychology. *Journal of Mathematical Psychology, 72*, 19-31.
- Ly, A., Marsman, M., & Wagenmakers, E.-J. (2018).  Analytic Posteriors for Pearson’s Correlation Coefficient. *Statistica Neerlandica, 72*(1), 4-13
- Rouder, J. N., & Morey R. D. (2012). Default Bayes factors for model selection in regression. *Multivariate Behavioral Research, 47*, 877-903.
- van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2018). Bayesian Inference for Kendall’s Rank Correlation Coefficient. *The American Statistician*,  72, 303-308.
- Wagenmakers, E.-J., Lodewyckx, T., Kuriyal, H., and Grasman, R. (2010). Bayesian hypothesis testing for psychologists: A tutorial on the Savage-Dickey method. *Cognitive Psychology, 60*, 158-189.
- Wetzels, R., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for correlations and partial correlations. *Psychonomic Bulletin & Review, 19*, 1057-1064.

### R Packages
---
- hypergeo
- ggplot2
- stats
