--------------------------------------------------------------------------------
--  Problem 75
--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
--  161667
--------------------------------------------------------------------------------

module Problem075 where

import Data.List ( sort, group, nub )

import Base ( (|>), on )
import Juke ( coprime, isqrt, factors' )

solutionFrom [] = return $ show $ solution

solution =
	triples 1500000
	|>	map tripleSum
	|>	sort
	|>	group
	|>	filter ((== 1) . length)
	|>	length

t1 (a, b, c) = (a - 2 * b + 2 * c, 2 * a - b + 2 * c, 2 * a - 2 * b + 3 * c)
t2 (a, b, c) = (a + 2 * b + 2 * c, 2 * a + b + 2 * c, 2 * a + 2 * b + 3 * c)
t3 (a, b, c) = (-a + 2 * b + 2 * c, - 2 * a + b + 2 * c, - 2 * a + 2 * b + 3 * c)

step triple = (t1 triple, t2 triple, t3 triple)
steps maxb triple =
	if tripleSum triple > maxb then [] else
	let (a, b, c) = step triple in
	triple : concat [ steps maxb a, steps maxb b, steps maxb c ]

mults maxb triples = concatMap mults1 triples where
	mults1 (a, b, c) = takeWhile (\ t -> tripleSum t <= maxb) $ map (\ n -> (n * a, n * b, n * c)) $ [1..]

triples maxb = mults maxb $ steps maxb $ (3, 4, 5)

tripleSum (a, b, c) = a + b + c

data M3 a = M3 a a a a a a a a a
m3multiply (M3 a11 a12 a13 a21 a22 a23 a31 a32 a33) (M3 b11 b12 b13 b21 b22 b23 b31 b32 b33) =
	M3
		(a11 * b11 + a12 * b21 + a13 * b31)
		(a11 * b12 + a12 * b22 + a13 * b32)
		(a11 + b13 + a12 * b23 + a12 * b33)
		(a21 * b11 + a22 * b21 + a23 * b31)
		(a21 * b12 + a22 * b22 + a23 * b32)
		(a21 * b13 + a22 * b23 + a23 * b33)
		(a31 * b11 + a32 * b21 + a33 * b31)
		(a31 * b12 + a32 * b22 + a33 * b32)
		(a31 * b13 + a32 * b23 + a33 * b33)
