import Data.Char

--1

--a
perimetro:: Double -> Double
perimetro r = 2 * pi * r

--b
dist:: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ( (x2-x1)^2 + (y2-y1)^2)

--c
primUlt:: [Char] -> (Char,Char)
primUlt l = (head l, last l)

--d
multiplo:: Int -> Int -> Bool
multiplo m n = if mod m n == 0 then True
               else False

--e
truncaImpar:: [Char] -> [Char]
truncaImpar [] = []
truncaImpar l = if mod (length l) 2 == 0 then l
                else tail l

--f
max2:: Int -> Int -> Int
max2 x y = if x > y then x
           else y

--g
max3:: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z


--2

--a
nRaizes:: Int -> Int -> Int -> Int
nRaizes a b c = if (b^2 - 4*a*c) > 0 then 2
                else if b^2 - 4*a*c == 0 then 1
                else 0

--b
raizes:: Int -> Int -> Int -> [Float]
raizes a b c = if nRaizes a b c == 2 then [(-b + sqrt(b^2 - 4*a*c))/(2*a), (-b - sqrt(b^2 - 4*a*c))/(2*a)]
               else []


--3
type Hora = (Int,Int)

--a
hvalida:: Hora -> Bool
hvalida (h,m) = ( h >=0 && h<24) && (m>= 0 && m<60)

--b
maiorhora:: Hora -> Hora -> Bool
maiorhora (x1,y1) (x2,y2) | x1 > x2 = True
                          | x1 == x2 && y1 > y2 = True
                          | otherwise = False

--c
convMin:: Hora -> Int
convMin (h,m) = h*60 + m

--d
convHora:: Int -> Hora
convHora m = (div m 60 , div m 60)

--e
difHoras:: Hora -> Hora -> Int
difHoras (x1,y1) (x2,y2) = abs ( x2*60 + y2 - (x1*60 + y1) )

--f
addMin:: Hora -> Int -> Hora
addMin (h,m) n = convHora (convMin (h,m) + n)
--ou
addMin2:: Hora -> Int -> Hora
addMin2 (h,m) n = ( div (h * 60 + m + n) 60, mod (h*60 + m + n) 60 )


--5
data Semaforo =  Verde | Amarelo | Vermelho  deriving (Show,Eq)

--a
next:: Semaforo -> Semaforo
next n | n == Verde = Amarelo
       | n == Amarelo = Vermelho
       | n == Vermelho = Verde

--b
stop:: Semaforo -> Bool
stop n = if n == Vermelho then True
            else False

--c
safe:: Semaforo -> Semaforo -> Bool
safe s1 s2 | s1 == Verde && s2 == Vermelho = True
           | s1 == Amarelo && s2 == Vermelho = True
           | s2 == Verde && s1 == Vermelho = True
           | s2 == Amarelo && s1 == Vermelho = True
           | otherwise = False


--6
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

--a
posx:: Ponto -> Double
posx (Cartesiano x y) = x

--b
posy:: Ponto -> Double
posy (Cartesiano x y) = y

--c
raio:: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)

--d

--e igual ao 1 b)

--7
data Ponto2 = Ponto2 Double Double deriving (Show,Eq)
data Figura = Circulo Ponto2 Double
       | Rectangulo Ponto2 Ponto2
       | Triangulo Ponto2 Ponto2 Ponto2
              deriving (Show,Eq)

--a
poligono:: Figura -> Bool
poligono (Circulo p1 d) = False
poligono (Rectangulo p1 p2) = True
poligono (Triangulo p1 p2 p3) = True

--b
vertices:: Figura -> [Ponto2]
vertices (Circulo p1 d) = []
vertices (Rectangulo p1 p2) = [p1,p2]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]

--c
dist':: Ponto2 -> Ponto2 -> Double
dist' (Ponto2 x1 y1) (Ponto2 x2 y2) = sqrt ( (x2-x1)^2 + (y2-y1)^2)

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
       let a = dist' p1 p2
           b = dist' p2 p3
           c = dist' p3 p1
           s = (a+b+c) / 2 -- semi-perimetro
       in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Rectangulo (Ponto2 x1 y1) (Ponto2 x2 y2)) = abs ((x2-x1)*(y2-y1))
area (Circulo _ r) = pi * r^2

--d
perimetro':: Figura -> Double
perimetro' (Triangulo p1 p2 p3) = dist' p1 p2 + dist' p2 p3 + dist' p1 p3
perimetro' (Rectangulo (Ponto2 x1 y1) (Ponto2 x2 y2)) = 2 * (abs (x2 - x1) + abs (y2 - y1))
perimetro' (Circulo _ r) = 2 * pi * r

--8

--a
isLower':: Char -> Bool
isLower' c = ord c >= ord 'a' && ord c <= ord 'z'

--b
isDigit:: Char -> Bool
isDigit c = ord c >= ord '0' && ord c <= ord '9'

--c
isAlpha:: Char -> Bool
isAlpha c = (ord c >= ord 'a' && ord c <= ord 'z') || (ord c >= ord 'A' && ord c <= ord 'Z' )

--d
toUpper:: Char -> Char
toUpper c | isLower' c = chr ( ord c - ord 'a' + ord 'A')
          | otherwise = c

--e
intToDigit:: Int -> Char
intToDigit c = chr (ord '0' + c)

--f
digitToInt:: Char -> Int
digitToInt c = ord 'c' - ord '0'

