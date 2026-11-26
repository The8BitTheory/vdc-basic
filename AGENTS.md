# Hinweise für Agenten

## Tests

- Nur Tests laufen lassen, die mit der Änderung zusammenhängen. Welche das sind, ergibt sich aus dem geänderten Code und seinen Aufrufern. Beispiel: Eine Änderung an der VMC-Zeilenschleife betrifft VMC/VMF/VCL/VMS-Tests, nicht die Zeichentests (`VDL`/`VDR`/`VDB`), die diesen Code nicht benutzen.
- Lange Läufe (z. B. komplette Suites in Z64K) nur, wenn die Änderung sie wirklich betrifft. Im Bericht kurz begründen, welche Tests gelaufen sind und welche bewusst nicht.
- Alte Fassungen nicht neu messen: Vergleichswerte aus den festgehaltenen Ergebnissen nehmen (Specs, Ergebnisdateien) und neue Ergebnisse dort mit Stand/Prüfsumme/Datum ablegen.

## Optimierungen: erst messen, dann umsetzen

- Vor einer Umsetzung einen kleinen Prototyp bauen (nur der Kernpfad, z. B. in sim65 oder mit einem Mikro-Benchmark) und an echten Szenen messen. Zeitrahmen dafür etwa 15–20 Minuten.
- Entscheidung anhand der gemessenen Zahl, nicht anhand einer Schätzung aus Befehlszählung. Ist der Gewinn klein, verwerfen, bevor die volle Umsetzung mit allen Tests beginnt.
- Volle Umsetzung erst danach, mit festem Zeitrahmen. Im Bericht Prognose und Ist-Wert nebeneinander stellen.
- Verworfene Versuche mit Messwerten und Grund in der zugehörigen Spec festhalten, damit sie nicht wiederholt werden.
