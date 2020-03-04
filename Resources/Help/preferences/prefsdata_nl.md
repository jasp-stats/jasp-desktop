Data Voorkeuren
=========

Met betrekking tot het verwerken van data in JASP kunnen de volgende opties gekozen worden:
(Al deze instellingen blijven werken, zelfs na het opnieuw opstarten van JASP.)

### Automatische synchronisatie met opgeslagen data bestand 

Wanneer de gebruiker een dataset in JASP laadt van buiten JASP (dat wil zeggen, in de editor die de voorkeur heeft), geeft deze checkbox aan of resultaten in JASP automatisch gesynchroniseerd worden, of niet. 
Synchronisatie kan ook handmatig in het hoofdmenu gestart worden, of door middel van toetsenbord shortcuts:

- OSX: &#8984; met Y
- Windows en Linux: Ctrl met Y

### Gebruik standaard spreadsheet editor 

In JASP kan een dataset worden geopend met dubbelklik op het data paneel. 
Dit opent de dataset in de editor die de voorkeur heeft, wat hier gespecificeerd kan worden. Of de standaard editor geselecteerd door het besturingssysteem.

### Importdrempel tussen Categorisch of SchaalImport

Het importeren van data in JASP heeft een drempelwaarde die vaststelt of een kolom moet worden benaderd als een Schaaltype of juist Categorisch (Nominaal of Ordinaal). De standaardwaarde van deze parameter is 10. 
Dit betekent dat wanneer er minder dan(of precies)  10 verschilende gehele getallen in de data zijn, de kolom het type Ordinaal krijgt (Nominaal type alleen wanneer er twee verschillende gehele getallen worden gevonden), anders krijgt de kolom het Schaaltype toegekend. 
Let op dat deze waarde wordt gebruikt wanneer de data wordt geïmporteerd, de data moet dus worden herladen (of gesynchroniseerd) om in werking te treden. 

### Ontbrekende waardenlijst

In deze lijst kan worden gespecificeerd wanneer observaties in de dataset moeten worden behandeld als ontbrekend (dat wil zeggen, als ontbrekende waarden als 420 worden gecodeerd, kan deze waarde hier toegevoegd worden en dan zal JASP alle cellen met de waarde 420 behandelen als ontbrekende waarde). 
Waarden kunnen van deze lijst worden gehaald door ze te selecteren en vervolgens op de minusknop te drukken.
Door op "Reset" te klikken, worden alle standaardwaarden hersteld. 
