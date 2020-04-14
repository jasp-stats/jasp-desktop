Gelijkwaardigheids T-Toets voor Eén Gemiddelde
==========================

Met de gelijkwaardigheids t-toets voor één gemiddelde kan de gebruiker de nulhypothese toetsen dat de effectgrootte van het populatiegemiddelde binnen een door de gebruiker gedefinieerd interval valt, d.w.z. de gelijkwaardigheids regio.

### Assumpties
- De afhankelijke variabele is continu.
- De data komen uit een aselecte steekproef uit de populatie.
- De afhankelijke variabele is normaal verdeeld in de populatie.

### Invoer
-------

#### Invoerveld
- Variabelen: De variabelen in dit veld geselecteerde variabelen worden meegenomen in de analyse.

#### Toets waarde
The nul hypothese dat het populatiegemiddelde gelijk is aan de toetswaarde.

#### Gelijkwaardigheids regio
- van ... tot ... : Bepaalt de gelijkwaardigheids regio door de boven- en ondergrens te specificeren.

#### Grafieken
- Prior en posterior: Geeft de verdeling van de prior en posterior van de effectgrootte onder de alternatieve hypothese.
  - Aanvullende informatie: Voegt de Bayes Factor berekend met de door de gebruiker gedefinieerde prior toe; voegt een kanswiel toe die de kans van de data onder de nulhypothese vs. de alternatieve hypothese laat zien; voegt de mediaan en de 95% geloofwaardigheidsinterval van de verdeling van de posterior van de effectgrootte toe.
  - Sequentiële analyse: Geeft de ontwikkeling van de Bayes factor weer terwijl de data binnenkomen, gebruik makende van de door de gebruiker gedefinieerde prior.
    - Robuustheidscheck: Voegt de resultaten van de sequentiële analyse toe, gebruik makende van de brede (scale=1) en ultrabrede prior (scale=sqrt (2)).

#### Aanvullende statistieken
- Beschrijvend: Steekproef grootte, steekproefgemiddelde, steekproef standaarddeviatie, standaard fout van het gemiddelde.
- Prior en posterior massa: Geeft de prior en posterior massa binnen en buiten de ingestelde gelijkswaardigheids regio weer.

#### Ontbrekende waarden
 - Het uitsluiten van waarnemingen, analyse bij analyse: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke test uitgevoerd met alle waarnemingen die valide data bevatten voor de afhankelijke variabele in de t-toets. De steekproefgroottes kunnen daardoor verschillen per toets.
 - Het uitsluiten van waarnemingen, lijstgewijs: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke t-toets uitgevoerd met enkel de waarnemingen die valide data voor alle afhankelijke variabelen bevatten. De steekproefgrootte is daardoor hetzelfde voor alle toetsen.

#### Prior
- Gestandaardiseerde effectgrootte
  - Standaard
    - Cauchy: Schaal van de Cauchy prior verdeling van de effectgrootte onder de alternatieve hypothese; de standaardoptie is 0.707.
  - Geinformeerd
    - Cauchy: Schaal en locatie.
    - Normal: Gemiddelde en standaarddeviatie.
    - Student's t: Schaal, locatie en vrijheidsgraden (vg).

### Uitvoer
---

#### Gelijkwaardigheids Bayesiaanse T-Toets voor één gemiddelde
- De eerste kolom bevat the afhankelijke variabele.
- Model Vergelijking:
  - &delta; &in; I vs. H<sub>1</sub>: De Bayes factor die bewijs geeft voor de gelijkwaardigheidsregio hypothese ten opzichte van de ongelimiteerde alternative hypothese.
  - &delta; &notin; I vs. H<sub>1</sub>: De Bayes factor die bewijs geeft voor de hypothese dat de effectgrootte buiten de gelijkwaardigheidsregio ligt ten opzichte van de ongelimiteerde alternative hypothese.
  - &delta; &in; I vs. &delta; &notin; I: De Bayes factor die bewijs geeft voor de gelijkwaardigheidsregio hypothese ten opzichte van de hypothese dat de effect grootte buiten de gelijkwaardigheidsregio ligt.
  - &delta; &notin; I vs. &delta; &in; I: De Bayes factor die bewijs geeft voor de hypothese dat de effectgrootte buiten de gelijkwaardigheidsregio ligt ten opzichte van de gelijkwaardigheidsregio hypothese.
- BF: De Bayes factor.
- fout %: De fout van de Gaussiaanse kwadratuur intergratie methode die wordt gebruikt op de Bayes factor te berekenen.

#### Beschrijvende Statistiek
- De eerste kolom bevat the afhankelijke variabele.
- N: De steekproefgrootte.
- Gemiddelde: Het steekproefgemiddelde.
- SD: Standaarddeviatie van het gemiddelde.
- Std. Fout: Standaardfout van het gemiddelde.
- Geloofwaardigheidsinterval: Standaardoptie is 95%.

#### Prior en Posterior Massa Tabel
- De eerste kolom bevat de afhankelijke variabele.
- Sectie: Het gedeelte waaronder de massa wordt berekend.
  - p(&delta; &in; I | H<sub>1</sub>): Sectie binnen de gelijkwaardigheidsregio van de prior verdeling.
  - p(&delta; &in; I | H<sub>1</sub>, data): Sectie binnen de gelijkwaardigheidsregio van de posterior verdeling.
  - p(&delta; &notin; I | H<sub>1</sub>): Sectie buiten de gelijkwaardigheidsregio van de prior verdeling.
  - p(&delta; &notin; I | H<sub>1</sub>, data): Sectie buiten de gelijkwaardigheidsregio van de posterior verdeling.
- Massa: Geeft de massa onder de sectie weer.

#### Grafieken
- Prior en posterior: Geeft de prior (stippellijn) en posterior (lijn) verdeling van de effectgrootte onder de alternatieve hypothese; de grijze cirkels geven de hoogte van de dichtheid van de prior en de posterior bij een effectgrootte delta = 0 weer. De horizontale lijn geeft de breedte van een 95% geloofwaardigheidsinterval van de posterior verdeling weer.
  - Aanvullende info: Geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior weer; geeft een kanswiel weer met de kans van de data onder de nulhypothese en alternatieve hypothese; geeft de mediaan en het 95% geloofwaardigheidsinterval van de posterior verdeling weer.
- Sequentiële analyse: Geeft de ontwikkeling van de Bayes factor weer, als een functie van het aantal datapunten (n), gebruik makenende van de door de gebruiker gedefinieerde prior; geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior weer; geeft een kanswiel weer die de kans van de data onder de nulhypothese vs. de alternatieve hypothese weergeeft; geeft de mediaan en de 95% geloofwaardigheidsinterval van de verdeling van de posterior weer; laat zien hoe besluitvaardig het bewijs is met Jeffreys' (1961) bewijscategorieën.
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
