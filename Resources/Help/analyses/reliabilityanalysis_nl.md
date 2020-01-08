Betrouwbaarheidsanalyse
===

Met een betrouwbaarheidsanalyse kan men de vaardigheid van een test om consisten een variabele te meten analyseren.

### Invoer
---

#### Invoerveld
- Variabelen: Alle variabelen die worden meegenomen.

#### Schaalstatistieken
- McDonald's w.
- Cronbach's alpha.
  - Ongestandaardiseerd.
  - Gestandaardiseerd.
- Gutmann's lambda 6.
- Grootste ondergrens.
- Gemiddelde inter-item correlatie. 
- Gemiddelde: Rekenkundig gemiddelde van de datapunten.
- Standaard deviatie: Standaard deviatie van de data punten. 

#### Statistieken voor individuele items
Statistieken voor individuele items. 
- McDonald's w.
- Cronbach's alpha: De meests populaire maat van de betrouwbaarheid van een test.
- Gutmann's lambda 6.
- Gemiddelde: Rekenkundig gemiddelde van de datapunten.
- Standaard deviatie: Standaard deviatie van de data punten.
- Item-rest correlatie: De correlatie tussen een item en de som van de rest van de items. 

### Omgekeerd geschaalde items 
- Hier kan je items selecteren die omgekeerd gecodeerd zijn en dus moeten worden omgedraaid. 

### Geavanceerde opties
- Missende waardes:
  - Sluit gevallen analyse voor analyse uit: Als er meerdere correlatietoetsen in een enkele analyse zijn zal elke toets worden uitgevoerd met alle gevallen waar data is op de relevante variabelen voor de uitgevoerde toets. Steekproefgrootte kan daardoor varieren tussen toetsen.
  - Sluit gevallen uit op lijstwijze: als er meerdere correlatietoetsen in een enkele analyse zijn wordt elke test aleen uitgevoerd op gevaleen waar data is voor alle variabelen. Steekproefgrootte is daardoor gelijk tussen de toeten.
- Betrouwbaarheidsinterval
  - Cronbach's alpha  (analytisch)
    - Betouwbaarheid: Standaardoptie is 95%.

### Uitvoer 
--- 
#### Betrouwbaarheidsanalyse
Test betrouwbaarheids statistieken: 
- Gemiddelde: Rekenkundig gemiddelde van de datapunten. 
- Standaard deviatie: Standaard deviatie van de data punten. 
- McDonald's w.
- Cronbach's alpha: De meest populaire maat van de betrouwbaarheid van een test.
- Gutmann's lambda 6.
- Gemiddelde inter-item correlatie. 
- `...`% Betrouwbaarheids interval: Een betrouwbaarheidsinterval voor Cronbach's alpha. 
  - Linker: De linkergrens van het betrouwbaarheidsinterval. 
  - Rechter: De rechtergrens van het betrouwbaarheidsinterval. 

#### Item Statistieken 
Item Betrouwbaarheidsstatistieken:
- De eerste kolom bevat alle variabelen die zijn meegenomen in de analyse.
- Gemiddelde: Rekenkundig gemiddelde van de datapunten. 
- Standaard deviatie: Standaard deviatie van de data punten. 
- Item-rest correlatie: De correlatie tussen een item en de som van de rest van de items. 
- Als item wordt uitgesloten: 
  - McDonald's w.
  - Cronbach's alpha. 
  - Gutmann's lambda 6. 

### R Packages
---
- Psych

### Voorbeeld
- Ga voor een voorbeeld naar `Open` --> `Data Library` --> `Descriptives` --> `Fear of Statistics`. 
