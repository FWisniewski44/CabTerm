# CabTerm
____________________________

## Exposé (Versionsverlauf; verändert sich ggf. im Arbeitsprozess)
____________________________

In der im Seminar "Vergleichende Politikwissenschaft II: Modelle der Koalitionsforschung und ihre empirische Anwendung" anzufertigenden Hausarbeit soll sich mit der Thematik der Stabilität von Minderheitsregierungen auseinandergesetzt werden. Bezüglich des Forschungsdesigns soll sich grob am Text von Thomas SAALFELD *Institutions, Chance, and Choices. The Dynamics of Cabinet Survival* (2008) orientiert werden.

Eine (vorläufige) konkrete Frage könnte also wie folgt lauten:

> ***Welche Einflussfaktoren beeinflussen die Stabilität von Minderheitsregierungen positiv bzw. negativ?***

## Aufbau, Design, Methodik
____________________________

Es könnten konkret Hypothesen für einzelne Variablen herausgearbeitet werden (aus den Bereichen wie bei Saalfeld, also strukturell, institutionell, preference range, externe Schocks, aber auch was die Transaktionskosten angeht usw.), wie diese die *Überlebenschance von Minderheitsregierungen* positiv oder negativ beeinflussen könnten, die dann ereignisanalytisch anhand eines Cox-Proportional-Hazards-Modells überprüft werden sollen - das würde dann eine Art exploratives Design darstellen, am Ende steht dann ein best fit-Modell.

## Daten

+ Andersson, Staffan; Bergman, Torbjörn & Ersson, Svante (2014). "The European Representative Democracy Data Archive, Release 3". Main sponsor: Riksbankens Jubileumsfond (In2007-0149:1-E). [www.erdda.se](https://erdda.org/erd/data-archive/).

+ Bergmann, Henning (2019): "Cabinets 2019 Dataset".

+ Strøm, Kaare; Bergmann, Torbjörn & Müller, Wolfgang (2008): "Comparative Parliamentary Democracy Data Archive". [CPD on www.erdda.se](https://erdda.org/cpd/)
____________________________

Daten sind sowohl vom Dozenten gestellt worden als auch über die Webseite des ERDDA abrufbar. Beide Datensätze wurden auf die Verfügbarkeit der erforderlichen Variablen überprüft.

Der Datensatz von Bergmann et al. enthält direkt passende Variablen, welche die vorzeitige und willkürliche Regierungsbeendigung erfassen (hauptsächlich ist hier *discr2019* interessant, enthält jedoch auch für ein competing risks design die Variablen *repl2019* und *early2019*). Als Variable, die die Zeit für die ereignisanalytische Methodik darstellt, fungiert hier *abs_dur*, also die absolute Regierungsdauer in Tagen. 

Im ERDDA-Datensatz befinden sich darüber hinaus weitere für die Analyse evtl. relevante und wichtige unabhängige Variablen, weshalb dieser nicht ganz vernachlässigt werden sollte. Problematisch ist hier nur, dass auf den ersten Blick keine eigenständige Variable im Datensatz vorhanden ist, welche direkt die vorzeitige und willkürliche Regierungsbeendigung erfasst - diese müsste, nach aktuellem Kenntnisstand - zunächst neu codiert werden, wovon (aufgrund des Aufwands und des Vorhandenseins einer solchen Variable im Datensatz von Bergmann et al.) aktuell abgesehen wird. Von Seiten des Dozenten wurde jedoch berechtigterweise auch darauf hingewiesen, dass es eventuell eine Notwendigkeit werden könnte, mehr auf die ERDDA-Daten zurückzugreifen, je näher am Modell von Saalfeld gearbeitet werden wird (da im Datensatz von Bergmann et al. eventuell nicht alle notwendigen Variablen mit aufgeführt sein könnten).

Erste Testmodelle und -kalkulationen wurden hauptsächlich mit den Daten von Bergmann et al. durchgeführt. Es wurde daran anschließend in die Richtung weitergearbeitet, die beiden Datensätze **sinnvoll** zusammenzuführen, sodass eine Arbeit mit beiden Datensätzen gleichzeitig möglich ist, ohne dass diese nur "aneinander geklebt" werden, weshalb nicht auf cbind() zurückgegriffen wurde, um nicht einfach nur relevante Variablen anzukleben. Mithilfe der merge()-Funktion funktionierte dies nicht wirklich, es konnte (aktuell: höchstwahrscheinlich) aber durch die left_join()-Funktion auf die richtige Art behoben werden.



