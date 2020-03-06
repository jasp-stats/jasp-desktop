Betrouwbaarheidsanalyse
===

Met een betrouwbaarheidsanalyse kan men de vaardigheid van een test om consistent een variabele te meten analyseren.

### Invoer
---

#### Invoerveld
- Variabelen: Alle variabelen die worden meegenomen.

#### Schaalstatistieken
- McDonald's ω.
- Cronbach's α: De populairste maat van de betrouwbaarheid van een test
  - Ongestandaardiseerd.
  - Gestandaardiseerd.
- Gutmann's λ6.
- Grootste ondergrens.
- Gemiddelde inter-item correlatie. 
- Gemiddelde: Rekenkundig gemiddelde van de datapunten.
- Standaardafwijking: Standaarddeviatie van de datapunten. 

#### Statistieken voor individuele items
Statistieken voor individuele items. 
- McDonald's ω (als item is verwijderd).
- Cronbach's α (als item is verwijderd).
- Gutmann's λ6 (als item is verwijderd). 
- Gemiddelde: Rekenkundig gemiddelde van de datapunten.
- Standaardafwijking: Standaarddeviatie van de datapunten.
- Item-rest correlatie: De correlatie tussen een item en de som van de rest van de items. 

### Omgekeerd geschaalde items 
- Hier kunt u items selecteren die omgekeerd gecodeerd zijn en dus moeten worden omgedraaid. 

### Geavanceerde opties
- Ontbrekende waarden:
  - Sluit waarnemingen uit per rij (= op lijstwijze): Als er meerdere correlatietoetsen in een enkele analyse zijn, wordt elke toets alleen uitgevoerd op waarnemingen waar data is voor alle variabelen. Steekproefgrootte is daardoor gelijk tussen de toetsen.
  - Sluit waarnemingen paarsgewijs uit: Als er meerdere correlatietoetsen in een enkele analyse zijn, zal elke toets worden uitgevoerd met alle waarnemingen waar data is op de relevante variabelen voor de uitgevoerde toets. Steekproefgrootte kan daardoor variëren tussen toetsen.
- Betrouwbaarheidsinterval
  - Cronbach's α analytisch
    - Betrouwbaarheid: Standaardoptie is 95.0%.

### Uitvoer 
--- 
#### Betrouwbaarheidsanalyse
Test betrouwbaarheidsstatistieken: 
- Gemiddelde: Rekenkundig gemiddelde van de datapunten. 
- Standaarafwijking: Standaarddeviatie van de datapunten. 
- McDonald's ω.
- Cronbach's α: De meest populaire maat van de betrouwbaarheid van een test.
- Gutmann's λ6.
- Gemiddelde inter-item correlatie. 
- `...`% Betrouwbaarheidsinterval: Een betrouwbaarheidsinterval voor Cronbach's alpha. 
  - Onder: De ondergrens van het betrouwbaarheidsinterval. 
  - Boven: De bovengrens van het betrouwbaarheidsinterval. 

#### Item Statistieken 
Item Betrouwbaarheidsstatistieken:
- De eerste kolom bevat alle variabelen die zijn meegenomen in de analyse.
- Gemiddelde: Rekenkundig gemiddelde van de datapunten. 
- Standaardafwijking: Standaarddeviatie van de datapunten. 
- Item-rest correlatie: De correlatie tussen een item en de som van de rest van de items. 
- Als item is verwijderd: 
  - McDonald's ω.
  - Cronbach's α. 
  - Gutmann's λ6. 

### R Packages
---
- Psych

### Voorbeeld
- Ga voor een voorbeeld naar `Open` --> `Bibliotheek` --> `Beschrijvingen` --> `Angst voor Statistiek`. 
