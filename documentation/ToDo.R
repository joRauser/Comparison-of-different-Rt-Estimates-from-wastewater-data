# ToDos

## 1 
# -> Use another model (not EpiEstim) which is destined for wastewater data

## 3
# Build in the delays of data (zB "Melde-delay", Abwasserdelay) -> Datengetrieben:
# Schreibe Funktion, welche eine bestimmte Metrik (gegeben eines potentiellen Delays) minimiert! 

## 5 Clean Code
# at Rt-Est_Hill  -> reduce code by unnecessary parts (e.g. weekly == "no" - part)
#                 -> delete the plotting part -> should be outsourced by "1"
#                 -> Write things, which are the same for both if-cases, outside the case-clause
# Write inline-comments
# Denglische kommentare in Englisch vereinhaltlichen



# Neue Modelle (speziell für wastewater) anwenden
# Verschiebungen in Daten beachten!!! (Wie groß ist Verschiebung in Daten (zB Abwasser ist hinter den Hospitalizations))

# Für Serial Interval einfach feste Werte nehmen (welche in Brockhaus et al als sinnvoll deklariert wurden) und entsprechend zitieren. 

# Metriken: Einfache -> zB Absolute Differenzen

# Unterscheidung zwischen "getricksten" wöchentlichen Daten und täglichen Daten

# Lieber das gute und saubere Schreiben der Arbeit priorisieren

# Wichtig bei der Seminararbeit: Inhalt, bildliche und sinnvolle Darstellung und Niederschrift 
# und schlüssige Analyse und Formulierung der Fragestellung




# Fragen Bracher
# 1.) Bessere Methode bei MLE?
# 2.) Sollen Werte ohne Timeshift mit denen mit Timeshift verglichen werden?
# 2.2) Wie genau soll darauf in der Arbeit eingegangen werden? Soll beides auch jeweils geplottet werden?

# 3.) Fehlersuche :) Warum wird timeshift nicht übernommen?