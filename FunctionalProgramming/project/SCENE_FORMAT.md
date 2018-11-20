# Format pliku sceny

__Rozszerzenie__: .scene  
__Obiekty__: Płaszczyzna, Kula  
__Światła__: Punktowe  

## Struktura pliku

Będzie to prosty format pliku składającego się z deklaracji, każdej w nowej
linii. (-) przy deklaracji oznacza, że jest wymagane aby dokładnie jedna znalazła się w
pliku.

__scene width height__ (-) - definiuje wielkość sceny  
__camera x y z u v w fov__ (-) - definiuje pozycje kamery, punkt w przestrzeni na które wskazuje oraz kąt widzenia `fov`.  
__background r g b__ - definiuje kolor tła sceny  
__matphong name r g b dif spec specExp__ - definiuje materiał o nazwie `name`, o kolorze `r g b`, współczynniku światła rozpraszanego `dif`,
współczynnik rozbłysku `spec` oraz wykładniku rozbłysku `specExp`  
__matreflec name r g b dif spec specExp reflec__ - tak samo jak `matphong`, z dodatkowym argumentem `reflec` będącym współczynnikiem odbijania światła  
__mattrans name r g b dif spec specExp reflec refrac tranmiss__ - tak samo jak `matreflec`, z dodatkowymi argumentami `refrac` będący współczynnikiem załamania materiału względem powietrza oraz `transmiss` będącym współczynnikiem światła przepuszczanego  
__plane x y z u v w mat__ - tworzy płaszczyznę w pozycji `x y z`, o wektorze normalnym `u v w` i materiale `mat`  
__sphere x y z r mat__ - tworzy kulę w pozycji `x y z` o promieniu `r` i materiale `mat`  
__light x y z i mat__ - tworzy światło punktowe o mocy `i`. Jeżeli `i` jest ujemne to światło ma taką samą intensywność niezależnie od odległośc.  
