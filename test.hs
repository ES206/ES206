data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Show, Read)
data Point = Point(Double, Double) deriving(Show, Read)
data Triangle = Triangle Point Point Point deriving (Show, Read)
data Car = Car {company :: String, style :: String, production :: Integer} deriving(Show, Read)

a :: Day
a = Tuesday

b::Point
c::Point
b = Point(2.4, 2.9)
c = Point(3, 4)

t1::Triangle
t1 = Triangle (Point (0,0)) (Point (0,1)) (Point (sqrt 3, (-0.5)))

c1::Car
c1 = Car{company="Toyota", style="SUV", production=3}

data Nat1 = One | Succ Nat1 deriving (Read, Eq, Ord, Show)
toNat1 m
 | m == 1 = One
 | m > 0  = Succ $ toNat1 (m - 1)
 | otherwise = error "Not Natural Number"

fromNat1 One   = 1
fromNat1 (Succ n) = 1 + fromNat1 n

plus m One = Succ m
plus m (Succ n) = Succ (plus m n)
times2 m = plus m m

identity same = [(x,x) | x <- same]

f::Int->Int
f x = x+1
g::Int->Int
g x = x+1
h = g.f

main = do
 let v1 = toNat1 1
 let v1_2 = times2 v1
 let vv1 = fromNat1 v1
 print(v1)
 print(v1_2)
 print(vv1)

 let v8 = toNat1 8
 let v8_2 = times2 v8
 let vv8 = fromNat1 v8
 let vv8_2 = fromNat1 v8_2
 print(v8)
 print(v8_2)
 print(vv8)
 print(vv8_2)
 
 let id = identity [1..4]
 print(id)

 print(h 2)