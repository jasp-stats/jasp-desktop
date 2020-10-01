Geavanceerde Voorkeuren
=========

Met de geavanceerde parameters in JASP kunnen de volgende opties worden gespecificeerd:
(Al deze instellingen blijven werken, zelfs na het opnieuw opstarten van JASP.)

## Modules Opties

### Herinner geactiveerde modules

Als deze optie is geactiveerd, dan zal JASP onthouden welke modules zijn geactiveerd en ervoor zorgen dat deze zo blijven, zelfs als JASP wordt afgesloten. Stelt u dus voor dat `Samenvattende Statistieken` was geactiveerd en dan wordt JASP gesloten. Wanneer JASP de volgende keer wordt opgestart, zal `Samenvattende Statistieken` direct weer geactiveerd worden. 

### Developer Mode (Beta versie)

Dit is waar kan worden gespecificeerd of u gebruik wilt maken van de JASP modules, of niet. 
Een locatiemap kan worden geselecteerd waar de nieuwe modules zijn opgeslagen. 
Modules van ontwikkelaars kunnen direct worden toegevoegd uit deze map, of modules kunnen worden toegevoegd van andere gespecificeerde locaties. 

De checkbox bepaalt of package metadata iedere keer moeten worden gegenereerd.
Zet deze functie uit als u een R-package naar een JASP module omzet, of als u simpelweg de manuele veranderingen van DESCRIPTION en NAMESPACE wilt behouden. 

De CRAN repository URL bepaald waar JASP de benodigde pacakges gespecificeerd in de module van download. 
De standaardoptie is `https://cloud.r-project.org`, maar een goed alternatief (wanneer packages bijvoorbeeld niet lijken te installeren) is `cran.r-project.org`. 

## Windows Workarounds

### LC_CTYPE
LC_CTYPE is een instelling mbt zogenoemde "locales" en het bepaald, op windows, hoe bepaalde karakters geëncodeerd worden.
Dit wordt intern gebruikt door R en kan jammer genoeg niet goed omgaan met internationale karakters onder Windows.
Deze instelling wordt in principe op "C" gezet door JASP zodat alle karakters in de resultaten worden ondersteund (unicode).

Er is echter een probleem in het geval dat je in een andere locale draait, zoals Duits, en je gebruikernaam bevat speciale karakters zoals ringel-s.
In dat scenario kan JASP niet starten als het per-gebruiker is geïnstalleerd of als je een dynamische module probeert te installeren.
Om dat te voorkomen herkent JASP deze situatie en stelt het LC_CTYPE niet in op "C" maar houdt het zoals het is en JASP werkt dan gewoon.
Helaas zullen dan niet alle karakters in de resultaten er goed uitzien.
JASP op een andere plek installeren (dus per-machine) zal dat verhelpen.

De standaard instelling hier is de beste en wij raden iedereen dan ook aan dit gewoon aangevinkt te laten.

Onze excuses voor dit kleine ongemak en we hopen het bij de volgende versie opgelost te hebben.

## Logopties

### Log naar Bestand
Wanneer dit is aangevinkt, zal JASP de uitgevoerde acties loggen in logbestanden.
Loggen is erg handig wanneer u een eigen module ontwikkelt, of wanneer u tegen een probleem aanloopt en hulp wilt krijgen van het ontwikkelaarsteam.
De log zou ons kunnen helpen om de aard van het probleem makkelijker te begrijpen. Let op dat het om het logproces te starten misschien nodig is om JASP opnieuw op te starten.
Het nummer in het invoerveld "Max logbestanden" duidt aan hoeveel logbestanden er op zijn hoogst worden bijgehouden om schrijfruimte te behouden. Extra, oude logbestanden worden verwijderd. 
De bestanden kunnen worden bekeken door op "Laat logs zien" te klikken. 
