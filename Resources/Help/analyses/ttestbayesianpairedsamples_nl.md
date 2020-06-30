Bayesiaanse Gepaarde T-Toets
===

Met de gepaarde t-toets kan de gebruiker de effectgrootte schatten en de nulhypothese testen dat het populatiegemiddelde van het verschil tussen gepaarde observaties gelijk is aan 0.

### Assumpties
- De verschilscore is continu. 
- De verschilscores komen uit een aselecte steekproef uit de populatie. 
- De verschilscore is normaal verdeeld in de populatie. 

### Invoer
---

#### Invoerveld
- Variabelen: In deze box zijn de variabelen geselecteerd waarvoor het verschil is berekend. Meerdere verschillen kunnen tegelijkertijd worden geanalyseerd door het specificeren van verschillende rijen met twee variabelen waarvoor het verschil is berekend. Met andere woorden, iedere rij geeft andere verschilscores weer.  

#### Hypothese 
- Maat 1 &ne; Maat 2: Tweezijdige alternatieve hypothese dat het populatiegemiddelde van het verschil niet gelijk is aan 0.
- Maat 1 &gt; Maat 2: Eenzijdige alternatieve hypothese dat het populatiegemiddelde van het verschil groter is dan 0. 
- Maat 1 &lt; Maat 2: Eenzijdige alternatieve hypothese dat het populatiegemiddelde kleiner is dan 0. 

#### Bayes Factor
-  BF10: Als u deze optie selecteert geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese.
-  BF01: Als u deze optie selecteert geeft de Bayes factor bewijs voor de nulhypothese ten opzichte van de alternatieve hypothese. 
-  Log(BF10): Natuurlijk logaritme van BF10. 

#### Toetsen
- Student: De student t-toets. Dit is de standaardoptie. 
- Wilcoxon rangtekentoets: Wilcoxon rangtekentoets. 
  - Aantal steekproeven: Het aantal MCMC steekproeven.

#### Aanvullende Statistieken
-  Beschrijvend: Steekproef grootte, steekproefgemiddelde, steekproef standaardafwijking, standaardfout van het gemiddelde voor iedere maat.

#### Grafieken
- Prior en posterior: Geeft de verdeling van de prior en posterior van de effectgrootte onder de alternatieve hypothese.
  - Aanvullende informatie: Voegt de Bayes Factor berekend met de door de gebruiker gedefinieerde prior toe; voegt een kanswiel toe die de kans van de data onder de nulhypothese vs. de alternatieve hypothese laat zien; voegt de mediaan en de 95% geloofwaardigheidsinterval van de verdeling van de posterior van de effectgrootte toe.
- Bayes factor robuustheidscheck: Geeft de Bayes factor weer als een functie van de breedte van de Cauchy prior voor de effectgrootte. De schaal van de Cauchy prior varieert tussen 0 en 1.5, zodat de priors steeds minder informatief worden.
- Sequentiële analyse: Geeft de ontwikkeling van de Bayes factor weer terwijl de data binnenkomen, gebruik makende van de door de gebruiker gedefinieerde prior.
  - Robuustheidscheck: Voegt de resultaten van de sequentiële analyse toe, gebruik makende van de brede (scale=1) en ultrabrede prior (scale=sqrt(2)).
- Beschrijvende grafieken:
  - Geloofwaardigheidsinterval: De standaardoptie is 95%.
 
 #### Ontbrekende Waarden
 - Het uitsluiten van waarnemingen, analyse voor analyse: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke test uitgevoerd met alle waarnemingen die valide data bevatten voor de afhankelijke variabele in de t-toets. De steekproefgroottes kunnen daardoor verschillen per toets. 
 - Het uitsluiten van waarnemingen, lijstgewijs: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke t-toets uitgevoerd met enkel de waarnemingen die valide data voor alle afhankelijke variabelen bevatten. De steekproefgrootte is daardoor hetzelfde over alle toetsen. 

### Prior
- Standaard:
   - Cauchy: Schaal van de Cauchy prior voor de effectgrootte onder de alternatieve hypothese; de standaardoptie is .707.
- Geinformeerd:
   - Cauchy: Schaal en locatie.
   - Normaal: Gemiddelde en standaardafwijking.
   - Student's t: Schaal, locatie en vrijheidsgraden (vg). 

### Uitvoer
---
#### Bayesiaanse T-Toets voor Onafhankelijke Steekproeven
- Bayes factor: als een eenzijdige test wordt gevraagd:
  - BF+0: De Bayes factor die bewijs geeft voor de eenzijdige alternatieve hypothese dat het verschil groter is dan 0, ten opzichte van de nulhypothese.
  - BF-0: De Bayes factor die bewijs geeft voor de eenzijdige alternatieve hypothese dat het verschil kleiner is dan 0, ten opzichte van de nulhypothese.
  - BF0+: De Bayes factor die bewijs geeft voor de nulhypothese, ten opzichte van de eenzijdige alternatieve hypothese dat het verschil groter is dan 0.
  - BF0-: De Bayes factor die bewijs geeft voor de nulhypothese, ten opzichte van de eenzijdige alternatieve hypothese dat het verschil kleiner is dan 0.
- error %: De fout van de Gaussiaanse kwadratuur intergratie methode die wordt gebruikt op de Bayes factor te berekenen.
- W: The toets statistiek van de rangtekentoets.
- Rhat: Indicatie van convergentie voor de rangtekentoets. De ratio van variantie binnen elke MCMC keten, en de totale variantie van de MCMC ketens voor de delta parameter. Waarden gelijk aan 1 duiden op convergentie.

#### Beschrijvende Statistiek
- N: De steekproefgrootte.
- Gemiddelde: Het steekproefgemiddelde.
- SD: Standaarddeviatie van het gemiddelde. 
- Std. Fout: Standaardfout van het gemiddelde.

#### Grafieken
- Prior en posterior: Geeft de prior (stippellijn) en posterior (lijn) verdeling van de effectgrootte onder de alternatieve hypothese; de grijze cirkels geven de hoogte van de dichtheid van de prior en de posterior bij een effectgrootte delta = 0 weer. De horizontale lijn geeft de breedte van een 95% geloofwaardigheidsinterval van de posterior verdeling weer.
  - Aanvullende info: Geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior weer; geeft een kanswiel weer met de kans van de data onder de nulhypothese en alternatieve hypothese; geeft de mediaan en het 95% geloofwaardigheidsinterval van de posterior verdeling weer.
- Bayes factor robuustheidsgrafiek: Geeft de Bayes factor weer als een functie van de breedte van de Cauchy prior voor effectgrootte. De zwarte cirkel geeft de Bayes factor berekend met een brede prior weer; de witte cirkel geeft de Bayes factor berekend met een ultrabrede prior weer; de grijze cirkel geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior verdeling weer.
- Sequentiële analyse: Geeft de ontwikkeling van de Bayes factor weer, als een functie van het aantal datapunten (n), gebruik makenende van de door de gebruiker gedefinieerde prior; geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior weer; geeft een kanswiel weer die de kans van de data onder de nulhypothese vs. de alternatieve hypothese weergeeft; geeft de mediaan en het 95% geloofwaardigheidsinterval van de verdeling van de posterior weer; laat zien hoe besluitvaardig het bewijs is met Jeffreys' (1961) bewijscategorieën. 
  - Robuustheidscheck: Geeft de ontwikkeling van de Bayes factor als een functie van het aantal datapunten (n), met de brede en ultrabrede verdeling van de prior. De zwarte cirkel geeft de Bayes factor berekend met een brede prior verdeling weer; de witte cirkel geeft de Bayes factor berekend met een ultrabrede prior verdeling weer; de grijze cirkel geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior verdeling weer.
- Beschrijvende grafieken:
  - Geloofwaardigheidsinterval: De standaardoptie is 95%.

### Referenties
--- 
- Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (in press). Informed Bayesian t-tests. *The American Statistician*. <a href="https://arxiv.org/abs/1704.02479">https://arxiv.org/abs/1704.02479</a>
- Jeffreys, H. (1961). *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Morey, R. D., Rouder, J. N., Pratte, M. S., & Speckman, P. L. (2011). Using MCMC chain outputs to efficiently estimate Bayes factors. *Journal of Mathematical Psychology, 55*, 368-378.
- Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t-tests for accepting and rejecting the null hypothesis. *Psychonomic Bulletin & Review, 16*, 225-237.
- van Doorn, J, Ly, A, Marsman, M, & Wagenmakers, E.-J. (2020). Bayesian rank-based hypothesis testing for the rank sum test, the signed rank test, and Spearman's rho. *Journal of Applied Statistics*.

### R-packages 
---
- BayesFactor
- ggplot2
- logspline
- stats
