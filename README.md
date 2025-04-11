Przedmiotem zadania jest znowu stworzenie funkcji wizualizującej proces redukcji wyrażeń kombinatorowych. W odróżnieniu od poprzedniego zadania, tym razem zestaw kombinatorów nie jest ustalony, ale dany przez użytkownika, w składni będącej podzbiorem Haskella, np.
```
s x y z = x z (y z)
k x y = x
main = s k k t
------------------------------------------------------------
s k k t
k t (k t)
t
```
Występujące w wyrażeniach identyfikatory, które nie są nazwami zdefiniowanego kombinatora ani argumentu (jak t w powyższym przykładzie), traktujemy jako stałe.

# Składnia
Korzystamy tylko z małego podzbioru skladni Haskella, nie ma typów - każda wartość może być zastosowana do dowolnych argumentów. Dzieki temu nie musimy pisać własnego parsera, ale możemy skorzystać z biblioteki haskell-src. Ta biblioteka buduje drzewo struktury dla pełnej składni Haskella, musimy je przekształcić do naszej uproszczonej składni
```
data Def = Def Name [Pat] Expr
data Expr = Var Name | Expr :$ Expr
type Pat = Name
type Name = String

newtype Prog = Prog {progDefs :: [Def]}
```
Zapoznaj się z dokumentacją haskell-src i napisz funkcje
```
fromHsString :: String -> Prog
fromParseResult :: ParseResult HsModule -> [Def]
fromHsModule :: HsModule -> [Def]
(i inne potrzebne)
```
Moduł Language.Haskell.Parser definiuje funkcję
```
parseModule :: String -> ParseResult HsModule
```
# Słownik definicji
W trakcie redukcji będzie nam potrzebne mapowanie nazw kombinatorów na ich definicje. Możemy zdefiniować
```
type DefMap = Data.Map.Map Name Def

buildDefMap :: Prog -> DefMap
```
Moduł Data.Map pochodzi z pakietu containers

# Redukcja
Podobnie jak w poprzednim zadaniu, definiujemy funkcje rstep i rpath obliczające pojedynczy krok i ścieżke redukcji.

Jak poprzednio, redeksem jest kombinator zaaplikowany do właściwej liczby argumentów (być może 0, np. main). Redukujemy w kolejności normalnej (od zewnątrz i od lewej).

Przez kombinator będziemy rozumieć nazwę, która posiada definicję. Nazwy, które nie mają definicji i nie są argumentami w bieżącej definicji będziemy traktować jako stałe. W przykładach piszemy je z wielkiej litery, ale to tylko konwencja (w Haskellu nazwy pisane z wielkiej litery oznaczaja konstruktory czyli w gruncie rzeczy stałe).

Na przykład (patrz plik peano.uhs):
```
zero f z = z
one f z = f z
two = suc one
tre = suc two
suc n f z = f (n f z)
o f g x = f (g x)
add m n f x = m f (n f x)
mul m n = o m n
fyr = add two two
six = mul two tre
main = six S Z
------------------------------------------------------------
six S Z
mul two tre S Z
o two tre S Z
two (tre S) Z
suc one (tre S) Z
tre S (one (tre S) Z)
suc two S (one (tre S) Z)
S (two S (one (tre S) Z))
S (suc one S (one (tre S) Z))
S (S (one S (one (tre S) Z)))
S (S (S (one (tre S) Z)))
S (S (S (tre S Z)))
S (S (S (suc two S Z)))
S (S (S (S (two S Z))))
S (S (S (S (suc one S Z))))
S (S (S (S (S (one S Z)))))
S (S (S (S (S (S Z)))))
```
albo (patrz plik kio.uhs)
```
s x y z = x z (y z)
k x y = x
i = s k k
om x = x x
omega = om om
main = k i omega Z
------------------------------------------------------------
k i omega Z
i Z
s k k Z
k Z (k Z)
Z
```
Dla zrealizowania kroku redukcji potrzebna będzie funkcja, która dokona podstawienia wyrażeń stanowiących parametry faktyczne w miejsce parametrów formalnych w ciele kombinatora, np.
```
subst :: (Name, Expr) -> Expr -> Expr
```
Uwaga o zmiennych: w trakcie redukcji trzeba uważać, żeby nie pomieszać zmiennych, inaczej moze nam się przydarzyć błędna redukcja:
```
s x y z = x z (y z)
k x y = x
main = s k k x
------------------------------------------------------------
s k k x
k k (k k) -- BAD!
k
```
Najprostszym sposobem uniknięcia tego problemu jest przemianowanie parametrów formalnych tak, aby każdy z nich miał unikalną nazwę.

# Wypisywanie
Wypisywanie definicji i kroków redukcji należy zrealizować definiując odpowiednie instancje klasy Show (metoda showsPrec).

Poprawność programu
W poprawnym programie:

każdy kombinator ma dokładnie jedną definicję
w definicji kombinatora argumenty mają różne nazwy (czyli np definicja bad x y x = ... jest niepoprawna).
jest definicja main, bez argumentów
W podstawowej wersji można założyć, że działamy na poprawnych programach. Dla uzyskania lepszej punktacji można dodać sprawdzanie tych warunków i odrzucanie programów niepoprawnych ze stosownym komunikatem.


Aby odpalić program: cabal run -- zadanie2 [ścieżka do pliku]
