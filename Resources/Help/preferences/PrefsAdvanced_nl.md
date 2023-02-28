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

De CRAN repository URL bepaald waar JASP de benodigde pacakges gespecificeerd in de module van download. 
De standaardoptie is `https://cloud.r-project.org`, maar een goed alternatief (wanneer packages bijvoorbeeld niet lijken te installeren) is `cran.r-project.org`. 

## Logopties

### Log naar Bestand
Wanneer dit is aangevinkt, zal JASP de uitgevoerde acties loggen in logbestanden.
Loggen is erg handig wanneer u een eigen module ontwikkelt, of wanneer u tegen een probleem aanloopt en hulp wilt krijgen van het ontwikkelaarsteam.
De log zou ons kunnen helpen om de aard van het probleem makkelijker te begrijpen. Let op dat het om het logproces te starten misschien nodig is om JASP opnieuw op te starten.
Het nummer in het invoerveld "Max logbestanden" duidt aan hoeveel logbestanden er op zijn hoogst worden bijgehouden om schrijfruimte te behouden. Extra, oude logbestanden worden verwijderd. 
De bestanden kunnen worden bekeken door op "Laat logs zien" te klikken. 
