bain Welch toets
==========================

Met de bain (Bayesian informative hypotheses evaluation) Welch toets kan de gebruiker de nulhypothese dat twee gemiddelden gelijk zijn testen ten opzichte van eenzijdige en tweezijdige alternatieve hypotheses. Een belangrijk kenmerk van de Bayesiaanse Welch toets is dat het er NIET vanuit gaat dat de variantie van de afhankelijke variabele hetzelfde is in beide groepen.

### Specificatie van de bain Welch toets

- Kies de afhankelijke variabele(n) uit de lijst met variabelen en sleep deze naar het Afhankelijke Variabele veld.
- Kies de factor uit de lijst met variabelen en sleep deze naar het Groeperende Variabele veld. 
- Stel de toevalsgenerator beginwaarde in gelijk aan een geheel getal om een herhaalbare, willekeurige nummerreeks te creëren. Om stabiliteit van de resultaten te verzekeren wordt aangeraden de analyses met twee verschillende seeds uit te voeren.
- Wanneer de bain Welch Toets voor het eerst wordt uitgevoerd, vink dan de aanvullende statistieken en beide grafieken aan. Wanneer de gebruiker dan terug gaat naar de Welch toets is het duidelijk wat deze drie opties inhouden, en kunnen de benodigde opties worden geselecteerd. 
- In de resultaten worden standaard de 95% geloofwaardigheidsintervallen weergegeven. Dit percentage kan naar behoeven worden aangepast. 
- De gebruiker kan uit vijf verschillende toetssituaties kiezen: 

1. H0: m1 = m2 versus H1: m1, m2 (geen beperkingen rondom beide gemiddelden)
2. H0: m1 = m2 versus H1: m1 > m2
3. H0: m1 = m2 versus H1: m1 < m2
4. H1: m1 > m2 versus H2: m1 < m2
5. H0: m1 = m2 versus H1: m1 > m2 versus H2: m1 < m2

- Als onder de Bayes Factor label wordt gekozen voor BF01, dan geven Bayes factor waarden groter dan 1 bewijs voor H0. Als wordt gekozen voor BF10, dan geven Bayes factor waarden groter dan 1 bewijs voor H1 (of H2). 

### Verkregen resultaten na het uitvoeren van de bain Welch toets

- Om de resultaten van de bain Welch toets goed te kunnen interpreteren, kunt u de TUTORIAL van Hoijtink, Mulder, van Lissa, and Gu (2018) lezen. Deze kan worden gevonden op de Psychological Methods website, of op de bain website via https://informative-hypotheses.sites.uu.nl/software/bain/
- Om de technische achtergrond van bain te begrijpen, kunt u Gu, Mulder, en Hoijtink (2017) en Hoijtink, Gu, en Mulder (2018) lezen. Deze kunnen worden gevonden op de website van the British Journal of Mathematical and Statistical Psychology, of op de bain website via https://informative-hypotheses.sites.uu.nl/software/bain/
- Na het uitvoeren van de Bayesiaanse Welch toets worden voor iedere afhankelijke variabele vier resultaten verkregen:

1. De tabel waarin de Bayes factor van H0 versus de alternatieve hypothese wordt weergegeven. Deze tabel bevat ook de posterior model kans van iedere hypothese. 
2. Een tabel met beschrijvende statistieken die voor iedere groep de steekproefgrootte, het steekproefgemiddelde, de steekproef standaarddeviatie (sd), de standaardfout (se) en het 95% geloofwaardigheidsinterval bevat. 
3. Een grafiek van de pmp's die het bewijs gevonden in de data voor iedere hypothese visueel benadrukken. 
4. Een grafiek met de steekproefgemiddeldes en de bijbehorende geloofwaardigheidsintervallen.

### Referenties

- Gu, X., Mulder, J., and Hoijtink, H. (2018). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110
- Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2019). A tutorial on testing hypotheses using the Bayes factor. Psychological Methods, 24, 539-556. DOI: 10.1037/met0000201 
- Hoijtink, H., Gu, X., and Mulder, J. (2019). Bayesian evaluation of informative hypotheses for multiple populations. British Journal of Mathematical and Statistical Psychology, 72, 219-243. DOI: 10.1111/bmsp.12145
