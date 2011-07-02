module Prelude where

-- Arithmetic operations and some other primitives are builtin
-- Type signatures are entirely useless here.
-- Guards on the rhs of function equations are not supported, but in case
-- expressions they are.

-- combinators
id x = x

const x _ = x

f $ x = f x
-- infixr 0 $

flip f x y = f y x

(f . g) x = f (g x)
-- infixr 9 .

fix f = let x = f x in x

-- booleans
not True = False
not False = True

True || _ = True
False || b = b
-- infixr 2 ||

False && _ = False
True && b = b
-- infixr 3 &&

-- tuples
fst (x, _) = x
snd (_, x) = x

curry f x y = f (x, y)
uncurry f (x, y) = f x y

-- lists
foldr _ z [] = z
foldr f z (x:xs) = x `f` foldr f z xs

foldl _ acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
-- foldl f z xs = foldr (\x r z -> r (f z x)) id xs z

null [] = True
null _ = False

map f [] = []
map f (x:xs) = f x : map f xs

head (x:_) = x
tail (_:xs) = xs

[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
-- infixr 5 ++

take n xs = if n <= 0 then []
 else case xs of
  [] -> []
  y:ys -> y : take (n - 1) ys

repeat x = let xs = x : xs in xs

drop n xs = if n <= 0 || null xs
 then xs
 else drop (n - 1) (tail xs)

length [] = 0
length (x:xs) = 1 + length xs

scanl f z [] = [z]
scanl f z (x:xs) = z : scanl f (f z x) xs

reverse = foldl (flip (:)) []

zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith (*) (x:xs) (y:ys) = x * y : zipWith (*) xs ys

iterate f x = x : iterate f (f x)
