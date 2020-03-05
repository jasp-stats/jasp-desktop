bain ANCOVA
==========================

Met de bain (Bayesian informative hypothese evaluation) ANCOVA kan de gebruiker (informatieve) hypotheses evalueren met de Bayes factor. Een eenvoudig voorbeeld hiervan is de Bayesiaanse evaluatie van H0: m1 = m2 = m3 versus H1: m1 > m2 > m3 versus Hu: geen beperkingen rondom de drie aangepaste gemiddeldes.

### Specificatie van de bain ANCOVA

- Kies de afhankelijke variabele uit de lijst met variabelen en sleep deze naar het Afhankelijke Variabele veld. Let op: de naam van de afhankelijke variabele moet met een letter beginnen, en mag verder bestaan uit letters, cijfers en _. 
- Kies de factor uit de lijst met variabelen en sleep deze naar het veld Vaste Factoren. Let op: de naam van de factor moet met een letter beginnen, en mag verder bestaan uit letters, cijfers en _. Let daarnaast op dat alle groepen moeten worden verzameld in EEN factor. Als de gebruiker gebruik maakt van, bijvoorbeeld, een factor met de niveaus jong-oud en een factor met de niveaus vrouw-man, moet er EEN nieuwe factor worden gemaakt met de niveaus jongvrouw, oudvrouw, jongman, oudman. De niveaus worden aangegeven met cijfers, of beginnen met een letter. Verder mogen ze bestaan uit letters, cijfers en _.
- Stel de toevalsgenerator beginwaarde in gelijk aan een geheel getal om een herhaalbare, willekeurige nummerreeks te creëren. Om stabiliteit van de resultaten te verzekeren wordt aangeraden de analyses met twee verschillende seeds uit te voeren.
- Kies de covariaat of covariaten uit de lijst met variabelen en sleep deze naar het Covariaten veld. Let op: de naam van de covariaat of covariaten moet met een letter beginnen, en mag verder bestaan uit letters, cijfers en _. 
- Wanneer de bain ANCOVA voor het eerst wordt uitgevoerd, vink dan de beide aanvullende statistieken en beide grafieken aan. Wanneer de gebruiker dan terug gaat naar de bain ANCOVA is het duidelijk wat deze vier opties inhouden, en kunnen de benodigde opties worden geselecteerd. 
- In de resultaten worden standaard de 95% geloofwaardigheidsintervallen weergegeven. Dit percentage kan naar wens worden aangepast. 
- Wanneer de gebruiker model beperkingen aanvinkt, zal een veld openen waarin gespecificeerd kan worden welke hypotheses de gebruiker wil evalueren. Het is belangrijk dat aan de volgende regels voor specificatie wordt voldaan:

1. Plaats elke hypothese op een aparte lijn.
2. Er wordt naar de niveaus van de ENE factor verwezen met: `factor.niveaunaam`. Als er, bijvoorbeeld, een factor leeftijd is met de niveaus y, m, o, wordt hiernaar verwezen met de labels `agey`, `agem`, en `ageo`, respectievelijk.
3. Lineaire combinaties van parameters moeten op de volgende manier worden gespecificeerd:
- Elke parameter naam wordt op zijn hoogst een enkele keer gebruikt. 
- Elke parameter naam kan vooraf worden vermenigvuldigd met een getal. 
- Een constante kan worden toegevoegd of verwijderd van elke parameter naam. 
- Een lineaire combinatie kan ook een enkel cijfer zijn. 

     Voorbeelden: `3 * agey + 5`; `agey + 2 * agem + 3 * ageo - 2`; `agey - ageo`; en `5`.

4. (Lineaire combinaties van) parameters kunnen worden beperkt met <, >, en =. Bijvoorbeeld: `agey > 0` of `agey > agem = 0` of `2 * agey < agem + ageo > 5`.
5. Met het en-teken '&' kunnen verschillende delen van een hypothese gecombineerd worden. Bijvoorbeeld: `agey > agem & agem > ageo` wat gelijk staat aan `agey > agem > ageo` of `agey > 0 & agem > 0 & ageo > 0`.
6. Sets van (lineaire combinaties van) parameters die zijn onderworpen aan dezelfde beperkingen kunnen worden gespecificeerd met (). Bijvoorbeeld: `agey > (agem,ageo)` wat gelijk staat aan `agey > agem & agey > ageo`.

Hypotheses moeten compatibel, niet-overbodig en mogelijk zijn. Wat dit precies inhoudt wordt hier beneden uitgelegd. 

*De sets hypotheses moeten compatibel zijn*. Voor de statistische achtergrond van deze eis, zie Gu, Mulder, en Hoijtink (2018). Het is gebruikelijk dat de hypothese-sets gespecificeerd door de onderzoekers compatibel zijn, en zo niet, dan geeft bain een foutmelding. Door het doorlopen van de volgende stappen kan worden vastgesteld of een hypothese set compatibel is:
- Vervang een bereik beperking, bijvoorbeeld `1 < agey < 3`, met een gelijkwaardigheidsbeperking waarin de meegenomen parameter gelijk wordt gesteld aan het midden van het bereik, zoals `agey = 2`. 
- Vervang in elke hypothese de < en > met =. Bijvoorbeeld: `agey = agem > ageo` wordt `agey = agem = ageo`.
- De hypotheses zijn compatibel wanneer er op zijn minst een oplossing is op de resulterende set vergelijkingen. Voor de twee hypotheses genoemd hierboven, de oplossing is `agey = agem = ageo = 2`. Een voorbeeld van niet compatibele hypotheses is `agey = 0` en `agey > 2`, omdat er geen oplossing is voor de vergelijkingen `agey=0` en `agey=2`.

*Elke hypothese in een set van hypotheses moet niet-overbodig zijn.* Een hypothese is overbodig wanneer hij ook kan worden gespecificeerd met minder beperkingen. Bijvoorbeeld: `agey = agem & agey > 0 & agem > 0` is overbodig, omdat deze ook gespecificeerd kan worden als `agey = agem & agey > 0`. Bain werkt correct wanneer alleen hypotheses met < en > overbodig zijn. Bain geeft een foutmelding wanneer een hypothese gespecificeerd met op zijn minst een = overbodig is. 

*Elke hypothese in een hypothese set moet mogelijk zijn.* Een hypothese is onmogelijk wanneer schattingen die overeenkomen met de hypothese niet bestaan. Bijvoorbeeld: waarden voor `agey` die overeenkomen met `agey = 0 & agey > 2` bestaan niet. Het is de verantwoordelijkheid van de gebruiker dat de gespecificeerde hypotheses mogelijk zijn. Zo niet, dan geeft bain een foutmelding, of een uitvoer tabel met `inf`'s. 

### Verkregen resultaten na het uitvoeren van de bain ANCOVA

- Om de resultaten van de bain ANCOVA goed te kunnen interpreteren, kunt u de TUTORIAL van Hoijtink, Mulder, van Lissa, and Gu (2018) te lezen. Deze kan worden gevonden op de Psychological Methods website, of op de bain website via https://informative-hypotheses.sites.uu.nl/software/bain/
- Om de technische achtergrond van bain te begrijpen, kunt u Gu, Mulder, en Hoijtink (2017) en Hoijtink, Gu, en Mulder (2018) lezen. Dit kan worden gevonden op de website van the British Journal of Mathematical and Statistical Psychology, of op de bain website via https://informative-hypotheses.sites.uu.nl/software/bain/
- Na het uitvoeren van de bain ANCOVA worden vijf resultaten verkregen:

1. De tabel waarin de Bayes factor voor elke hypothese is gespecificeerd tegenover zijn ontkenning wordt weergegeven. Deze tabel bevat ook de posterior model kans van iedere hypothese. Dit zowel voor een set zonder en een set met de niet beperkte hypothese. 
2. De Bayes factor matrix waarin de wederzijdse Bayes factors van de hypotheses gespecificeerd in het vak Model Beperkingen worden weergegeven. 
3. Een tabel met coëfficiënten die voor iedere groep in de ANCOVA steekproefgrootte, het aangepaste gemiddelde, de standaardfout (se) en de 95% geloofwaardigheidsinterval weergeeft. Daarnaast bevat deze tabel dezelfde informatie voor de covariaat of covariaten.
4. Een grafiek van de pmp's (met en zonder niet beperkte hypotheses) die het bewijs gevonden in de data voor iedere hypothese visueel benadrukken. 
5. Een grafiek met steekproefgemiddelden en hun bijbehorende geloofwaardigheidsinterval.

### Referenties

- Gu, X., Mulder, J., and Hoijtink, H. (2018). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110
- Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2019). A tutorial on testing hypotheses using the Bayes factor. Psychological Methods, 24, 539-556. DOI: 10.1037/met0000201 
- Hoijtink, H., Gu, X., and Mulder, J. (2019). Bayesian evaluation of informative hypotheses for multiple populations. British Journal of Mathematical and Statistical Psychology, 72, 219-243. DOI: 10.1111/bmsp.12145
