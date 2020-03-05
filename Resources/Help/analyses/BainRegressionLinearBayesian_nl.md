bain Lineaire Regressie
==========================

Met de bain (Bayesian informative hypotheses evaluation) lineaire regressie kan de gebruiker (informatieve) hypotheses evalueren met de Bayes factor. Een eenvoudig voorbeeld hiervan is de Bayesiaanse evaluatie van H0: b1 = b2 = b3 versus H1: b1 > b2 > b3 versus Hu: geen beperkingen op de drie regressiecoëfficiënten.

### Specificatie van de bain Lineaire Regressie

- Kies de afhankelijke variabele uit de lijst met variabelen en sleep deze naar het Afhankelijke Variabele veld. Let op: de naam van de afhankelijke variabele moet met een letter beginnen, en mag verder bestaan uit letters, cijfers en _. 
- Stel de toevalsgenerator beginwaarde in gelijk aan een geheel (integer) nummer om een herhaalbare, willekeurige nummerreeks te creëren. Om stabiliteit van de resultaten te verzekeren wordt aangeraden de analyses met twee verschillende seeds uit te voeren.
- Wanneer de bain Lineaire Regressie voor het eerst wordt uitgevoerd, vink dan de beide aanvullende statistieken en de grafiek aan. Wanneer de gebruiker dan terug gaat naar de bain Lineaire Regressie is het duidelijk wat deze vier opties inhouden, en kunnen de benodigde opties worden geselecteerd. 
- Als de hypotheses vergelijkingen van regressie coëfficiënten bevatten, bijvoorbeeld b1 = b2 = b3, dan moeten de regressie coëfficiënten op dezelfde schaal liggen. Dit kan worden gedaan door onder aanvullende opties het veld Standardiseer aan te vinken. Dit geeft aan dat de hypotheses en resultaten in termen van gestandaardiseerde regressie coëfficiënten zijn. 
- Wanneer de gebruiker 'model beperkingen' aanvinkt, zal een veld openen waarin gespecificeerd kan worden welke hypotheses de gebruiker wil evalueren. Het is belangrijk dat aan de volgende regels voor specificatie wordt voldaan:

1. Plaats elke hypothese op een aparte lijn.
2. De regressie coëfficiënten worden benoemd met de namen van de overeenkomende predictoren. Bijvoorbeeld, leeftijd, gewicht en grootte, als dit de namen zijn van de predictoren in het veld van Covariaten.
3. Lineaire combinaties van parameters moeten op de volgende manier worden gespecificeerd:
- Elke parameter naam wordt op zijn hoogst een enkele keer gebruikt. 
- Elke parameter naam kan vooraf worden vermenigvuldigd met een getal. 
- Een constante kan worden toegevoegd of verwijderd van elke parameter naam. 
- Een lineaire combinatie kan ook een enkel cijfer zijn. 

     Voorbeeld: `3 * leeftijd + 5`; `leeftijd + 2 * gewicht + 3 * grootte - 2`; `leeftijd - gewicht`; and `5`.
	 
4. (Lineaire combinaties van) parameters kunnen worden beperkt met <, >, en =. Bijvoorbeeld: `leeftijd > 0` or `leeftijd > gewicht = 0` of `2 * leeftijd < gewicht + grootte > 5`.
5. Met de ampersand & kunnen verschillende delen van een hypothese gecombineerd worden. Bijvoorbeeld: `leeftijd > gewicht & gewicht > grootte` wat gelijk staat aan `leeftijd > gewicht > grootte` of `leeftijd > 0 & gewicht > 0 & grootte > 0`.
6. Sets van (lineaire combinaties van) parameters die zijn onderworpen aan dezelfde beperkingen kunnen worden gespecificeerd met (). Bijvoorbeeld: `leeftijd > (gewicht,grootte)` wat gelijk staat aan `leeftijd > gewicht & leeftijd > grootte`.

Hypotheses moeten compatibel, niet-overbodig en mogelijk zijn. Wat dit precies inhoudt wordt hier beneden uitgelegd. 

*De sets hypotheses moeten compatibel zijn*. Voor de statistische achtergrond van deze eis, zie Gu, Mulder, en Hoijtink (2018). Het is gebruikelijk dat de hypothese-sets zoals gespecificeerd door de onderzoekers compatibel zijn, en zo niet, dan geeft bain een foutmelding. Door het doorlopen van de volgende stappen kan worden vastgesteld of een hypothese set compatibel is:

- Vervang een bereik beperking, bijvoorbeeld `1 < leeftijd < 3`, met een gelijkwaardigheidsbeperking waarin de meegenomen parameter gelijk wordt gesteld aan het midden van het bereik, zoals `leeftijd = 2`. 
- Vervang in elke hypothese de < en > met =. Bijvoorbeeld: `leeftijd = gewicht > grootte > ses` wordt `leeftijd = gewicht = grootte = ses`.
- De hypotheses zijn compatibel wanneer er op zijn minst een oplossing is op de resulterende set vergelijkingen. Voor de twee hypotheses genoemd hierboven, de oplossing is `leeftijd = gewicht = grootte = ses = 2`. Een voorbeeld van niet compatibele hypotheses is `leeftijd= 0` en `leeftijd > 2`, omdat er geen oplossing is voor de vergelijkingen `leeftijd = 0` en `leeftijd= 2`.

*Elke hypothese in een set van hypotheses moet niet-overbodig zijn.* Een hypothese is overbodig wanneer hij ook kan worden gespecificeerd met minder beperkingen. Bijvoorbeeld: `leeftijd = grootte & leeftijd > 0 & grootte > 0` is overbodig, omdat deze ook gespecificeerd kan worden als `leeftijd = grootte & leeftijd > 0`. Bain werkt correct wanneer alleen hypotheses met < en > overbodig zijn. Bain geeft een foutmelding wanneer een hypothese gespecificeerd met op zijn minst een = overbodig is. 

*Elke hypothese in een hypothese set moet mogelijk zijn.* Een hypothese is onmogelijk wanneer schattingen die overeenkomen met de hypothese niet bestaan. Bijvoorbeeld: waarden voor `leeftijd` die overeenkomen met `leeftijd = 0 & leeftijd > 2` bestaan niet. Het is de verantwoordelijkheid van de gebruiker dat de gespecificeerde hypotheses mogelijk zijn. Zo niet, dan geeft bain een foutmelding, of een uitvoer tabel met `Inf`'s. 

### Verkregen resultaten na het uitvoeren van de bain Lineaire Regressie

- Om de resultaten van de bain Lineaire Regressie goed te kunnen interpreteren, kunt u de TUTORIAL van Hoijtink, Mulder, van Lissa, and Gu (2018) lezen. Deze kan worden gevonden op de Psychological Methods website, of op de bain website via https://informative-hypotheses.sites.uu.nl/software/bain/
- Om de technische achtergrond van bain te begrijpen, kunt u Gu, Mulder, en Hoijtink (2017) en Hoijtink, Gu, en Mulder (2018) lezen. Dit kan worden gevonden op de website van the British Journal of Mathematical and Statistical Psychology, of op de bain website via https://informative-hypotheses.sites.uu.nl/software/bain/
- Na het uitvoeren van de bain Lineaire Regressie worden vier resultaten verkregen:

1. De tabel waarin de Bayes factor voor elke hypothese is gespecificeerd tegenover zijn ontkenning wordt weergegeven. Deze tabel bevat ook de posterior model kans van iedere hypothese. Dit zowel voor een set zonder als een set met de niet beperkte hypothese. 
2. De Bayes factor matrix waarin de wederzijdse Bayes factoren van de hypotheses gespecificeerd in het vak Model Beperkingen worden weergegeven. 
3. Een beschrijvende tabel die alle schattingen van de regressiecoëfficiënten, hun standaardfout (se) en het 95% geloofwaardigheidsinterval bevat. 
4. Een grafiek van de pmp's (met en zonder niet beperkte hypotheses) die het bewijs gevonden in de data voor iedere hypothese visueel benadrukken. 

### Referenties

- Gu, X., Mulder, J., and Hoijtink, H. (2018). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110
- Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2019). A tutorial on testing hypotheses using the Bayes factor. Psychological Methods, 24, 539-556. DOI: 10.1037/met0000201 
- Hoijtink, H., Gu, X., and Mulder, J. (2019). Bayesian evaluation of informative hypotheses for multiple populations. British Journal of Mathematical and Statistical Psychology, 72, 219-243. DOI: 10.1111/bmsp.12145
