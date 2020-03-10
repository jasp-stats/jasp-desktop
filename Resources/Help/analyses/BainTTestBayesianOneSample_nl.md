bain T-Toets voor Eén Gemiddelde
==========================

Met de bain (Bayesian informative hypotheses evaluation) t-toets voor één gemiddelde kan de gebruiker de nulhypothese dat het gemiddelde van een variabele nul is testen ten opzichte van eenzijdige en tweezijdige alternatieve hypotheses.

### Specificatie van de bain t-toets voor één gemiddelde

- Kies variabelen uit de lijst met variabelen en zet ze in het veld aan de rechterkant. 
- Stel de toevalsgenerator beginwaarde in gelijk aan een geheel (integer) nummer om een herhaalbare, willekeurige nummerreeks te creëren. Om stabiliteit van de resultaten te verzekeren wordt aangeraden de analyses met twee verschillende seeds uit te voeren.
- Wanneer de bain t-toets voor één gemiddelde voor het eerst wordt uitgevoerd, vink dan de aanvullende statistieken en beide grafieken aan. Wanneer de gebruiker dan terug gaat naar de bain t-toets voor één gemiddelde is het duidelijk wat deze drie opties inhouden, en kunnen de benodigde opties worden geselecteerd. 
- In de resultaten worden standaard de 95% geloofwaardigheidsintervallen weergegeven. Dit percentage kan naar wens worden aangepast. 
- De gebruiker kan uit vijf verschillende toetssituaties kiezen (let op: de referentie waarde "ref" kan in het vak "toetswaarde" worden ingevuld): 

1. H0: m = ref versus H1: m (geen beperking rondom het gemiddelde)
2. H0: m = ref versus H1: m > ref
3. H0: m = ref versus H1: m < ref
4. H1: m > ref versus H2: m < ref
5. H0: m = ref versus H1: m > ref versus H2: m < ref

- Als onder de Bayes Factor label wordt gekozen voor BF01, dan geven Bayes factor waarden groter dan 1 bewijs voor H0. Als wordt gekozen voor BF10, dan geven Bayes factor waarden groter dan 1 bewijs voor H1 (of H2). 

### Verkregen resultaten na het uitvoeren van de bain t-toets voor één gemiddelde

- Om de resultaten van de bain t-toets voor één gemiddelde goed te kunnen interpreteren, kunt u de TUTORIAL van Hoijtink, Mulder, van Lissa, and Gu (2018) lezen. Deze kan worden gevonden op de Psychological Methods website, of op de bain website via https://informative-hypotheses.sites.uu.nl/software/bain/
- Om de technische achtergrond van bain te begrijpen, kunt u Gu, Mulder, en Hoijtink (2017) en Hoijtink, Gu, en Mulder (2018) lezen. Dit kan worden gevonden op de website van the British Journal of Mathematical and Statistical Psychology, of op de bain website via https://informative-hypotheses.sites.uu.nl/software/bain/
- Na het uitvoeren van de bain t-toets voor één gemiddelde worden voor ieder paar variabelen vier resultaten verkregen:

1. De tabel waarin de Bayes factor van H0 versus de alternatieve hypothese wordt weergegeven. Deze tabel bevat ook de posterior model kans van iedere hypothese. 
2. Een tabel met beschrijvende statistieken die voor elke variabele de steekproefgrootte, het steekproefgemiddelde, de steekproef standaarddeviatie (sd), de standaardfout (se) en het 95% geloofwaardigheidsinterval bevat. 
3. Een grafiek van de pmp's die het bewijs gevonden in de data voor iedere hypothese visueel benadrukken. 
4. Een grafiek met het gemiddelde verschil tussen paren van metingen en de bijbehorende geloofwaardigheidsinterval.

### Referenties

- Gu, X., Mulder, J., and Hoijtink, H. (2018). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110
- Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2019). A tutorial on testing hypotheses using the Bayes factor. Psychological Methods, 24, 539-556. DOI: 10.1037/met0000201 
- Hoijtink, H., Gu, X., and Mulder, J. (2019). Bayesian evaluation of informative hypotheses for multiple populations. British Journal of Mathematical and Statistical Psychology, 72, 219-243. DOI: 10.1111/bmsp.12145
