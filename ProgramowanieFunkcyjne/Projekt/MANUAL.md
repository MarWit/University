# Instrukcja dla użytkownika

## Instalacja

Po wejściu do katalogu ze źródłami (`src`) należy stworzyć piaskownice dla managera pakietów `cabal` i zainstalować wszystkie potrzebne zależności:

```
$ cabal sandbox init
$ cabal install --only-dependencies
```

Gdy już wszystkie moduły się skompilują będzie można skompilować i uruchomić Ray Tracer poleceniem:

```
$ cabal run -- <ścieżka_do_pliku>
$ cabal run -- scenes/test.scene
```

## Renderowanie

Po uruchomieniu Ray Tracera z wybraną sceną jako argumentem rozpocznie się jej renderowanie. Może to potrwać od parunastu sekund do
paru minut w zależności od złożoności sceny oraz wydajności procesora. Gdy scena się skończy renderować, efekt zostanie wyświetlony w nowym oknie a dodatkowo utworzy się plik graficzny BMP o nazwie `rendered.bmp` w katalogu z którego uruchomiliśmy program (obraz będzie odbity w pionie).

## Własne sceny

Tworzenie własnych scen jest bardzo proste jako że format pliku jest zbiorem "komend" oddzielonymi znakiem nowej linii.
Dokładną specyfikacje formatu można zobaczyć w pliku `SCENE_FORMAT.md`, a dodatkowo w katalogu `src/scenes` znajduje się
parę przykładowych scen.
