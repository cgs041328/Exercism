module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs, exp)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (a, b) = Complex a b

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex a b) = Complex a (-b)

abs :: Floating a => Complex a -> a
abs (Complex a b) = sqrt $ a ^ 2 + b ^ 2

real :: Num a => Complex a -> a
real (Complex a _) = a

imaginary :: Num a => Complex a -> a
imaginary (Complex _ b) = b

exp :: Floating a => Complex a -> Complex a
exp (Complex a b) = mul (complex (P.exp a, 0)) (complex (cos b, sin b))

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) = complex (a*c - b*d, b*c + a*d)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = complex (a+c, b+d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) = complex (a-c, b-d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) = complex ((a*c + b*d) / (c^2 + d^2),  (b*c - a*d) / (c^2 + d^2))
