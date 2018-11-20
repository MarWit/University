# Napotkane problemy i ich rozwiązania

* Podczas implementowania kolejnych materiałów, w szczególności materiału odbijającego oraz załamującego
program zaczął znacznie spowalniać co też mocno utrudniało testowanie. Problem został rozwiązany przez
zrównoleglenie obliczeń przy użyciu modułu `Controll.Pararell` oraz zamiany naiwnego sprawdzania wszystkich
bytów na scenie po kolei na wykonujący mniej "zderzeń" promienia z prymitywami algorytm wykorzystujący drzewo AABB.

* Przy pierwszych renderach krawędzie wokół narysowanych obiektów były bardzo ostre. Rozwiązaniem tego problemu
jest sprawdzanie wielu (losowych) punktów dla jednego piksela a następnie uśrednienie wyników zamiast liczenia koloru dla
jednego promienia przechodzącego przez środek piksela. (czyli implementacja prostego samplingu)

* Po zaimplementowaniu materiału odbijającego, dla niektórych scen program potrafi się zapętlić. Problemem był brak
ograniczenia dla odbijającego się od lustrzanych powierzchni promienia. Wystarczyło zatem zadać górne ograniczenie dla
ilości odbić.

* Podczas implementowania kolejnych materiałów na wyrenderowanych obrazkach pojawiał się szum o kolorze tła. Okazało się,
że zapomniałem normalizować wektora kierunku dla promienia przez co ten "nie trafiał" w obiekty na scenie. Rozwiązaniem problemu
było znormalizowanie wszystkich wektorów kierunków. Dodatkowo została uwzględniona niedokładność typu zmiennoprzecinkowego
i zamiast sprawdzać np. czy wartość jest zerowa, jest teraz sprawdzane czy wartość bezwzględna jest mniejsza niż mały epsilon.
