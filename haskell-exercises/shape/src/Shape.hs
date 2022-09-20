module Shape (Point(..),Circle(..),Rectangle(..),point,origin,rectangle,base,height,shift,circle,surface,Surface(..)) where

data Point = Point { x::Double, y:: Double} deriving (Eq, Show)

data Circle    = Circle    Point Double deriving (Eq, Show)
data Rectangle = Rectangle Point Point deriving (Eq, Show)


-- A point from a tuple Pair
point::(Double, Double) -> Point
point (a, b) = Point{x=a,y=b}

-- The origin
origin::Point
origin = point (0,0)

-- Rectangle from a Tuple where (x0 y0) == origin
rectangle::(Double, Double) -> Rectangle
rectangle (a,b)= Rectangle Point{x=0, y=0} Point{x=a,y=b}

base::Rectangle -> Double
base (Rectangle pointA pointB) = (x pointB) - (x pointA)

height::Rectangle -> Double
height (Rectangle pointA pointB) = (y pointB) - (y pointA)

pointA::Rectangle -> Point
pointA (Rectangle pointA _)= pointA

pointB::Rectangle -> Point
pointB (Rectangle _ pointB)= pointB

-- Circle from radius
circle::Double -> Circle
circle radius = Circle (point (0,0)) radius

-- Clase Shift

class Shift a where
   shift::a -> (Double, Double) -> a
   
instance Shift Point where
   shift aPoint (a,b) = Point{x=((x aPoint) + a),y=((y aPoint)+ b)}
   
instance Shift Rectangle where
   shift rectangle (a,b)= Rectangle (point (a+(x (pointA rectangle)),b+(y (pointA rectangle)))) (point (a+(x (pointB rectangle)),b+(y (pointB rectangle))))
   
instance Shift Circle where
   shift (Circle _ radius) (a,b) = Circle (point (a,b)) radius
   
-- Define the Surface class

class Surface a where
  surface:: a -> Double

instance Surface Rectangle where
  surface rectangle = (base rectangle) * (height rectangle)

instance Surface Circle where
  surface (Circle _ radius) = pi * (radius^2)
