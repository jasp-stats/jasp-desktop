Samenvatting statistiek Bayesiaanse Correlatie
==========

Met Bayesiaanse correlatie analyse kan men de nulhypothese testen dat de populatiecorrelatie (Pearson product-moment correlatie) tussen twee variabelen 0 is.

### Invoer
---

#### Invoerveld
- *n*: Steekproefgrootte (minimaal 2).

#### Correlatiecoëfficiënt
  - *Pearson's r*: De geobserveerde Pearson's product-moment correlatie coëfficiënt.
  - *Kendall's tau-b*: De geobserveerde Kendall's tau-b rangorde correlatiecoëfficiënt.

#### Alt. Hypothese
- *gecorreleerd*: Tweezijdige alternatieve hypothese dat de populatiecorrelatie niet 0 is.
- *positief gecorreleerd*: Eenzijdige alternatieve hypothese dat de populatiecorrelatie hoger is dan 0.
- *negatief gecorreleerd*: Eenzijdige alternatieve hypothese dat de populatiecorrelatie lager is dan 0.

- *Geloofwaardigheidsinterval*: Geeft het geloofwaardigheidsinterval weer voor de correlatiecoëfficiënt.
-
#### Bayes Factor
- BF<sub>10</sub>: Als u deze optie selecteert geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese. Dit is de standaardoptie.
- BF<sub>01</sub>: Als u deze optie selecteert geeft de Bayes factor bewijs voor de nulhypothese ten opzichte van de alternatieve hypothese. Dit is gelijk aan 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : Natuurlijk logaritme van BF10.

#### Grafieken
- *Prior en posterior*: Geeft de dichtheid van de prior (stippellijn) en de posterior (lijn) van de effectgrootte onder de alternatieve hypothese; de grijze cirkels representeren de hoogte van de prior en de posterior bij een effectgrootte delta = 0. De horizontale lijn representeert de breedte van een 95% geloofwaardigheidsinterval van de posterior.
  - *Schattingsinformatie*: Geeft de mediaan en het 95% geloofwaardigheidsinterval van de posterior weer.
  - *Toetsingsinformatie*: Voegt de Bayes factor berekend met de door de gebruiker gedefinieerde prior toe; voegt een kanswiel toe die de kans van de data onder de nulhypothese vs. de alternatieve hypothese laat zien; voegt twee cirkels toe voor de prior en posterior op het toetsingspunt (rho=0), de ratio van de twee punten is gelijk aan de Bayes factor.
- *Bayes factor robuustheidsgrafiek*: Geeft de Bayes factor weer als een functie van de breedte van de Cauchy prior voor effectgrootte. De schaal van de Cauchy prior varieert tussen 0 en 1.5 (tussen 0 en 2 als de prior van de gebruiker hoger is dan 1.5), op zo'n manier dat de priors steeds minder informatief worden.

#### Prior

*Gerekte beta prior breedte*: Standaardoptie is 1.

### Uitvoer
---
#### Bayesiaanse Pearson Correlaties
- **Bayes factor**: Als er een eenzijdige toets is geselecteerd:
  - BF+0: De Bayes factor die bewijs geeft voor de eenzijdige hypothese dat de populatiecorrelatie hoger is dan 0.
  - BF-0: De Bayes factor die bewijs geeft voor de eenzijdige hypothese dat de populatiecorrelatie lager is dan 0.
  - BF0+: De Bayes factor de bewijs geeft voor de nulhypothese ten opzichte van de eenzijdige hypothese dat de populatiecorrelatie hoger is dan 0.
  - BF0-: De Bayes factor de bewijs geeft voor de nulhypothese ten opzichte van de eenzijdige hypothese dat de populatiecorrelatie lager is dan 0.
- **p**: De p-waarde van de t-statistiek.

### Referenties
---
- Jeffreys, H. (1961). *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Ly, A., Verhagen, A. J., & Wagenmakers, E.-J. (2017). Harold Jeffreys's default Bayes factor hypothesis tests: Explanation, extension, and application in psychology. *Journal of Mathematical Psychology, 72*, 19-31.
- Ly, A., Marsman, M., & Wagenmakers, E.-J. (2018).  Analytic Posteriors for Pearson’s Correlation Coefficient. *Statistica Neerlandica, 72*(1), 4-13
- Rouder, J. N., & Morey R. D. (2012). Default Bayes factors for model selection in regression. *Multivariate Behavioral Research, 47*, 877-903.
- Wetzels, R., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for correlations and partial correlations. *Psychonomic Bulletin & Review, 19*, 1057-1064.
- van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (in press). Bayesian Inference for Kendall's Rank Correlation Coefficient. *The American Statistician*.

### R Packages
---
- hypergeo
- ggplot2
- stats
