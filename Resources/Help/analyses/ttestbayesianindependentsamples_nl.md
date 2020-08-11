Bayesiaanse T-Toets voor Onafhankelijke Steekproeven
===
Met de t-toets voor onafhankelijke steekproeven kan de gebruiker de effectgrootte schatten en de nulhypothese dat steekproefgemiddelden van twee onafhankelijke steekproeven gelijk zijn toetsen. 


### Assumpties
---

- De afhankelijke variabele is continu. 
- De data van beide groepen komen uit een aselecte steekproef uit de populatie. 
- De afhankelijke variabele is normaal verdeeld in beide populaties. 
- De populatie varianties in beide groepen zijn homogeen.

### Invoer
---

#### Invul veld
- Variabelen: In deze box wordt de afhankelijke variabele geselecteerd.  
- Groeperende Variabele: In deze box wordt de variabele die de groepen definieert geselecteerd. 


#### Hypothese 
-  Group 1 &ne; Group 2: Tweezijdige alternatieve hypothese dat de populatiegemiddelden gelijk zijn. 
-  Group 1 &gt; Group 2: Eenzijdige alternatieve hypothese dat het populatiegemiddelde van groep een hoger is dan het populatiegemiddelde van groep twee. 
-  Group 1 &lt; Group 2: Eenzijdige alternatieve hypothese dat het populatiegemiddelde van groep een lager is dan het populatiegemiddelde van groep twee. 

#### Bayes Factor
-  BF10: Als u deze optie selecteert geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nul hypothese.
-  BF01: Als u deze optie selecteert geeft de Bayes factor bewijs voor de nul hypothese ten opzichte van de alternatieve hypothese. 
-  Log(BF10): Natuurlijk logaritme van BF10. 

#### Toetsen
- Student: De student t-toets. Dit is de standaardoptie. 
- Mann-Whitney: Mann-Whitney test. 
  - Aantal steekproeven: Het aantal MCMC steekproeven.
  
#### Aanvullende statistieken
-  Beschrijvend: Steekproef grootte, steekproefgemiddelde, steekproef standaardafwijking, standaardfout van het gemiddelde voor iedere groep.

#### Ontbrekende Waarden
 - Het uitsluiten van waarnemingen, analyse voor analyse: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke test uitgevoerd met alle waarnemingen die valide data bevatten voor de afhankelijke variabele in de t-toets. De steekproefgroottes kunnen daardoor verschillen per toets. 
 - Het uitsluiten van waarnemingen, lijstgewijs: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke t-toets uitgevoerd met enkel de waarnemingen die valide data voor alle afhankelijke variabelen bevatten. De steekproefgrootte is daardoor hetzelfde over alle toetsen.

#### Reproduceerbaarheid
- Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse.

#### Grafieken
- Prior en posterior: Geeft de verdeling van de prior en posterior van de effectgrootte onder de alternatieve hypothese.
  - Aanvullende informatie: Voegt de Bayes Factor berekend met de door de gebruiker gedefinieerde prior toe; voegt een kanswiel toe die de kans van de data onder de nulhypothese vs. de alternatieve hypothese laat zien; voegt de mediaan en de 95% geloofwaardigheidsinterval van de verdeling van de posterior van de effectgrootte toe.
- Bayes factor robuustheidscheck: Geeft de Bayes factor weer als een functie van de breedte van de Cauchy prior voor de effectgrootte. De schaal van de Cauchy prior varieert tussen 0 en 1.5, zodat de priors steeds minder informatief worden.
  - Aanvullende informatie: Voegt de Bayes factor berekend met de door de gebruiker gedefinieerde prior toe; voegt een kanswiel toe die de kans van de data onder de nulhypothese vs. de alternatieve hypothese laat zien; voegt de mediaan en de 95% geloofwaardigheidsinterval van de verdeling van de posterior van de effectgrootte toe.
- Sequentiële analyse: Geeft de ontwikkeling van de Bayes factor weer terwijl de data binnenkomen, gebruik makende van de door de gebruiker gedefinieerde prior.
  - Robuustheidscheck: Voegt de resultaten van de sequentiële analyse toe, gebruik makende van de brede (scale=1) en ultrabrede prior (scale=sqrt(2)).
- Beschrijvende grafieken
  - Geloofwaardigheidsinterval: Geeft de centrale geloofwaardigheidsintervallen weer. De standaardoptie is 95%.
 
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
  - BF+0: De Bayes factor die bewijs geeft voor de eenzijdige alternatieve hypothese dat groep een > groep twee, ten opzichte van de nulhypothese.
  - BF-0: De Bayes factor die bewijs geeft voor de eenzijdige alternatieve hypothese dat groep een < groep twee, ten opzichte van de nulhypothese.
  - BF0+: De Bayes factor die bewijs geeft voor de nulhypothese, ten opzichte van de eenzijdige alternatieve hypothese dat groep een > groep twee.
  - BF0-: De Bayes factor die bewijs geeft voor de nulhypothese, ten opzichte van de eenzijdige alternatieve hypothese dat groep een < groep twee.
- error %: De fout van de Gaussiaanse kwadratuur intergratie methode die wordt gebruikt op de Bayes factor te berekenen.
- W: De toets statistiek van de Wilcoxon toets. Deze wordt berekend door de rangordes van de eerste groep op te tellen (dezelfde procedure als door R wordt gebruikt). 
- R-dakje: Mate van convergentie voor de MCMC procedure van de Wilcoxon toets. Een ratio die de varianties vergelijkt binnen en tussen de MCMC ketens voor de delta parameter. Waarden minder dan, of gelijk aan, 1 duiden op convergentie.


#### Groep Beschrijvende Statistiek
- N: De steekproefgrootte.
- Gemiddelde: Het steekproefgemiddelde.
- SD: Standaarddeviatie van het gemiddelde. 
- Std. Fout: Standaardfout van het gemiddelde.

#### Grafieken
- Prior en posterior: Geeft de prior (stippellijn) en posterior (lijn) verdeling van de effectgrootte onder de alternatieve hypothese; de grijze cirkels geven de hoogte van de dichtheid van de prior en de posterior bij een effectgrootte delta = 0 weer. De horizontale lijn geeft de breedte van een 95% geloofwaardigheidsinterval van de posterior verdeling weer.
  - Aanvullende info: Geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior weer; geeft een kanswiel weer met de kans van de data onder de nulhypothese en alternatieve hypothese; geeft de mediaan en het 95% geloofwaardigheidsinterval van de posterior verdeling weer.
- Bayes factor robuustheidsgrafiek: Geeft de Bayes factor weer als een functie van de breedte van de Cauchy prior voor effectgrootte. De zwarte cirkel geeft de Bayes factor berekend met een brede prior weer; de witte cirkel geeft de Bayes factor berekend met een ultrabrede prior weer; de grijze cirkel geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior verdeling weer.
- Sequentiële analyse: Geeft de ontwikkeling van de Bayes factor weer, als een functie van het aantal datapunten (n), gebruik makenende van de door de gebruiker gedefinieerde prior; geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior weer; geeft een kanswiel weer die de kans van de data onder de nulhypothese vs. de alternatieve hypothese weergeeft; geeft de mediaan en de 95% geloofwaardigheidsinterval van de verdeling van de posterior weer; laat zien hoe besluitvaardig het bewijs is met Jeffreys' (1961) bewijscategorieën. 
  - Robuustheidscheck: Geeft de ontwikkeling van de Bayes factor als een functie van het aantal datapunten (n), met de brede en ultrabrede verdeling van de prior. De zwarte cirkel geeft de Bayes factor berekend met een brede prior verdeling weer; de witte cirkel geeft de Bayes factor berekend met een ultrabrede prior verdeling weer; de grijze cirkel geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior verdeling weer.
- Beschrijvende grafieken:
  - Geloofwaardigheidsinterval: Centrale geloofwaardigheidsinterval. De standaardoptie is 95%.

### Referenties
---
- Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (in press). Informed Bayesian t-tests. *The American Statistician*. <a href="https://arxiv.org/abs/1704.02479">https://arxiv.org/abs/1704.02479</a>
- Jeffreys, H. (1961).  *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Morey, R. D., Rouder, J. N., Pratte, M. S., & Speckman, P. L. (2011). Using MCMC chain outputs to efficiently estimate Bayes factors.  *Journal of Mathematical Psychology, 55*, 368-378.
- Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t-tests for accepting and rejecting the null hypothesis.  *Psychonomic Bulletin & Review, 16*, 225-237.
- van Doorn, J, Ly, A, Marsman, M, & Wagenmakers, E.-J. (2020). Bayesian rank-based hypothesis testing for the rank sum test, the signed rank test, and Spearman's rho. *Journal of Applied Statistics*.


### R Packages
---
- BayesFactor
- ggplot2
- logspline
- stats
