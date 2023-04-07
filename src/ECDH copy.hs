module ECDH where

import Test.HUnit
import Control.Monad
import System.Exit (exitFailure)
import Data.Bits



-- Point data type represents a point on the elliptic curve.
-- The first Integer represents the x-coordinate of the point.
-- The second Integer represents the y-coordinate of the point.
data Point = Point Integer Integer | Infinity
  deriving (Eq, Show)

-- Curve data type represents an elliptic curve.
-- The first Integer represents the 'a' coefficient of the curve equation: y^2 = x^3 + ax + b.
-- The second Integer represents the 'b' coefficient of the curve equation: y^2 = x^3 + ax + b.
-- The third Integer represents the prime field characteristic (modulus) used for arithmetic operations on the curve.
data Curve = Curve Integer Integer Integer
  deriving (Eq, Show)


----------------------------------------------------------------
--                      Curve Functions                       --
----------------------------------------------------------------


add :: Curve -> Point -> Point -> Point
add _ p Infinity = p
add _ Infinity q = q
add (Curve a b p) (Point x1 y1) (Point x2 y2) =
  let s = if not (x1 == x2 && y1 == y2)

          then let top = (y2 - y1) `mod` p
                   bottom = (x2 - x1) `mod` p
               in (top * modInverse bottom p) `mod` p

          else let top = (3 * x1 * x1 + a) `mod` p
                   bottom = (2 * y1) `mod` p
               in (top * modInverse bottom p) `mod` p
      
      x3 = ((s * s) - x1 - x2) `mod` p

      y3 = (s * (x1 - x3) - y1) `mod` p

  in Point x3 y3


double :: Curve -> Point -> Point
double curve point = add curve point point


multiply :: Curve -> Integer -> Point -> Point
multiply _ 0 _ = Infinity
multiply curve n point
  | point == Infinity = Infinity
  | otherwise = go curve point n
  where
    go _ _ 0 = Infinity
    go curve point n
      | n `mod` 2 == 1 = add curve point (go curve (double curve point) (n `div` 2))
      | otherwise = go curve (double curve point) (n `div` 2)





eea :: Integer -> Integer -> (Integer, Integer, Integer)
eea a 0 = (1, 0, a) -- base case
eea a b = let (quotient, remainder) = a `quotRem` b
              (s_prev, t_prev, gcd_prev) = eea b remainder
              s = t_prev -- calculate s
              t = s_prev - quotient * t_prev -- calculate t
              gcd = gcd_prev -- the gcd remains the same
          in (s, t, gcd) -- return the results


modInverse :: Integer -> Integer -> Integer
modInverse a modulus =
  let (inverse, _, g) = eea a modulus
  in if g == 1
     then inverse `mod` modulus
     else error "No modular inverse"







infinityPoint :: Point
infinityPoint = Infinity

{- curve25519 :: Curve
curve25519 = Curve 486662 1 (2^255 - 19)

generator25519 :: Point
generator25519 = Point 9 14781619447589544791020599137925164008

-- Your implementation for key pair generation using Curve25519
generateKeyPair25519 :: Integer -> (Point, Integer)
generateKeyPair25519 = generateKeyPair curve25519

-- Your implementation for shared secret computation using Curve25519
sharedSecret25519 :: Integer -> Point -> Integer
sharedSecret25519 = sharedSecret curve25519 -}




----------------------------------------------------------------
--                           ECDH                             --
----------------------------------------------------------------


generateKeyPair :: Curve -> Integer -> (Point, Integer)
generateKeyPair = undefined

sharedSecret :: Curve -> Integer -> Point -> Integer
sharedSecret = undefined

----------------------------------------------------------------
--                            Tests                           --
----------------------------------------------------------------


main :: IO ()
main = do
    testResults <- runTestTT tests
    if errors testResults + failures testResults == 0
        then putStrLn "All tests passed!"
        else exitFailure

tests :: Test
tests = TestList [additionTests, doublingTests, multiplicationTests]



additionTests :: Test
additionTests = TestLabel "AdditionTests" $ TestCase $ do
  -- a b p
  let curves = [(1,3,31),(3,5,67),(4,4,103),(2,1,199),(1,4,349),
                (5,1,577),(2,2,919),(1,5,1567),(1,3,2129),
                (4,3,2971),(1,3,3907),(5,5,4283),(1,1,4999)]

  -- x1 y1 x2 y2 x3 y3 - x5 per curve
  let vectors = [[(21,27,5,3,15,13),(15,13,6,15,20,26),(3,23,24,26,23,14),(4,28,5,28,22,3),(9,11,26,11,27,20)],
                 [(34,45,40,26,57,39),(28,47,49,22,61,21),(21,4,54,28,18,53),(9,52,49,45,28,20),(34,22,29,19,66,66)],
                 [(37,51,27,15,19,55),(24,15,14,69,20,87),(94,28,62,49,28,51),(1,3,42,10,49,29),(42,10,38,51,83,24)],
                 [(170,35,42,155,169,76),(178,161,30,14,42,44),(104,46,142,180,149,120),(47,144,49,138,112,51),(190,100,197,136,74,127)],
                 [(180,186,319,14,323,119),(209,20,340,231,257,273),(233,162,219,35,6,227),(325,198,202,107,339,274),(34,91,262,317,169,115)],
                 [(69,83,309,139,560,14),(71,536,383,500,228,392),(503,275,302,526,149,480),(359,292,127,211,464,410),(34,355,300,258,57,328)],
                 [(672,732,873,709,698,350),(797,716,453,688,610,464),(902,89,797,203,402,681),(121,315,656,404,393,399),(349,297,884,464,904,569)],
                 [(56,1126,443,194,448,227),(220,114,1114,1122,1240,345),(1036,1105,1118,198,1193,880),(1312,44,589,1418,1108,147),(541,302,452,1432,1428,748)],
                 [(1799,664,122,847,1945,624),(1231,636,961,182,1067,791),(1424,464,532,2123,1403,497),(1957,420,1873,1509,1962,25),(405,449,833,1500,414,1832)],
                 [(2138,1683,2754,2296,2066,1977),(1335,1497,2344,226,1356,1047),(1493,347,1750,363,243,436),(670,2469,1734,2168,864,1456),(2588,1029,1470,2608,636,78)],
                 [(2326,1441,1298,63,3138,3065),(1294,2749,3666,3813,2671,1186),(3211,2690,381,3549,3737,159),(1664,591,1697,444,695,1841),(2186,2957,3528,2127,702,2763)],
                 [(159,4228,449,1196,2087,688),(1547,3337,514,1758,646,3820),(232,369,2211,1554,477,2406),(2480,3224,631,2894,948,1527),(2497,3697,4032,1885,2399,3587)],
                 [(910,2855,2872,2434,667,2711),(4875,3537,224,4060,856,966),(3312,2926,3331,1257,2458,4935),(271,4957,2885,3060,4269,3934),(1873,1580,62,4311,584,4760)]]

  forM_ (zip curves vectors) $ \((a, b, p), vecs) -> do
    let e = Curve a b p
    forM_ vecs $ \(x1, y1, x2, y2, x3, y3) -> do
      let a = Point x1 y1
          b = Point x2 y2
          ab = Point x3 y3
          o = infinityPoint
      assertEqual "Standard addition" ab (add e a b)
      assertEqual "Point at infinity (a + o)" a (add e a o)
      assertEqual "Point at infinity (o + b)" b (add e o b)





doublingTests :: Test
doublingTests = TestLabel "DoublingTests" $ TestCase $ do
  -- a b p
  let curves = [(1,3,31),(3,5,67),(4,4,103),(2,1,199),(1,4,349),
                (5,1,577),(2,2,919),(1,5,1567),(1,3,2129),
                (4,3,2971),(1,3,3907),(5,5,4283),(1,1,4999)]

  -- x1 y1 x3 y3 - x5 per curve
  let vectors = [[(3,8,30,30),(17,29,28,2),(23,17,18,5),(30,1,6,16),(4,28,1,25)],
                 [(18,53,53,10),(11,30,40,41),(2,32,21,63),(20,62,9,15),(41,46,2,35)],
                 [(76,35,83,24),(74,42,31,7),(49,74,43,8),(29,43,74,42),(31,96,71,45)],
                 [(8,176,30,185),(15,146,178,161),(20,9,142,180),(62,38,48,170),(47,144,134,57)],
                 [(214,194,279,1),(24,275,197,27),(326,250,87,322),(221,149,214,194),(222,54,129,160)],
                 [(327,215,324,174),(307,76,464,167),(203,195,563,373),(563,204,93,448),(381,281,188,341)],
                 [(664,515,462,112),(666,597,430,697),(363,51,137,275),(858,289,604,881),(235,407,795,351)],
                 [(177,1530,816,523),(206,1480,1118,198),(495,1155,688,967),(666,444,542,1050),(591,1462,297,1089)],
                 [(592,269,709,1786),(1191,1857,1723,2093),(492,983,1730,1546),(300,1729,1222,162),(501,648,1129,711)],
                 [(2818,2065,2754,675),(466,2393,2102,2853),(65,2318,1005,338),(1855,296,1032,48),(2250,916,424,2210)],
                 [(323,1201,1072,1229),(10,129,524,1876),(2975,3107,2822,1825),(3005,996,3583,842),(1924,2104,1058,2613)],
                 [(4226,716,2692,3677),(487,3308,2379,2666),(1454,3203,3780,925),(2276,686,948,1527),(1238,2509,1245,2201)],
                 [(3569,4115,1443,2392),(1232,731,3146,1450),(4785,1494,1596,3052),(3010,877,4562,3868),(768,2979,1612,2068)]]

  forM_ (zip curves vectors) $ \((a, b, p), vecs) -> do
    let e = Curve a b p
    forM_ vecs $ \(x1, y1, x3, y3) -> do
      let a = Point x1 y1
          a2 = Point x3 y3
          o = infinityPoint
      assertEqual "Point doubling" a2 (double e a)



multiplicationTests :: Test
multiplicationTests = TestLabel "MultiplicationTests" $ TestCase $ do
  -- a b p
  let curves = [(1,3,31),(3,5,67),(4,4,103),(2,1,199),(1,4,349),
                (5,1,577),(2,2,919),(1,5,1567),(1,3,2129),
                (4,3,2971),(1,3,3907),(5,5,4283),(1,1,4999)]

  -- orders
  let orders = [41,53,113,181,349,577,881,1621,2141,3023,3889,4349,5009]

  -- x1 y1 a x3 y3 - x5 per curve
  let vectors = [[(28,2,34,5,28),(14,8,14,9,20),(27,20,14,4,3),(24,26,11,22,3),(20,26,1,20,26)],
                 [(66,66,49,40,41),(16,53,9,20,62),(53,10,15,41,21),(61,46,33,32,21),(64,61,35,34,45)],
                 [(62,54,45,48,75),(41,39,89,20,16),(20,16,110,71,58),(21,64,78,57,98),(74,61,96,69,42)],
                 [(52,47,112,42,155),(152,165,73,184,24),(182,32,72,130,36),(47,144,177,37,88),(30,14,65,150,103)],
                 [(268,3,328,62,203),(178,276,299,142,129),(61,14,26,170,67),(282,330,262,123,210),(254,146,94,192,109)],
                 [(265,347,104,551,392),(412,380,282,211,106),(138,409,468,284,210),(308,221,287,251,317),(324,403,131,8,391)],
                 [(239,751,121,284,730),(769,758,764,518,515),(300,293,107,249,760),(350,26,259,102,370),(450,131,210,654,304)],
                 [(177,37,498,314,1014),(105,1311,881,1249,1260),(864,962,164,1100,1345),(1298,1036,1091,444,962),(844,860,1403,985,419)],
                 [(1944,1437,1857,1643,447),(949,551,1804,1894,512),(826,701,530,466,256),(320,1960,288,85,1811),(1961,377,1048,91,372)],
                 [(607,2965,1053,848,1829),(204,1588,1223,961,139),(948,1940,1894,2935,1352),(1070,2635,1756,1520,1008),(2504,2475,1600,1800,2864)],
                 [(1150,3353,2339,2897,2624),(1778,3573,1872,1585,189),(2001,1148,1540,1778,3573),(694,1012,2820,300,2673),(68,276,3671,3618,233)],
                 [(1168,1922,76,309,4099),(2599,3183,1843,352,3959),(3731,3464,656,1439,3711),(3174,2333,3826,4278,2630),(2207,944,2768,240,3520)],
                 [(2207,1013,3577,1417,4275),(2682,3637,4111,3154,3892),(4793,739,4887,2649,2482),(807,4051,1821,3123,2251),(2345,1235,3260,887,1428)]]

  forM_ (zip3 curves orders vectors) $ \((a, b, p), order, vecs) -> do
    let e = Curve a b p
    forM_ vecs $ \(x1, y1, s, x3, y3) -> do
      let p = Point x1 y1
          sP = Point x3 y3
      assertEqual "Scalar multiplication" sP (multiply e s p)

    -- ord(P) * P = O
    let generator = head vecs
        p = Point (fst generator) (snd generator)
    assertEqual "Multiplying with order" infinityPoint (multiply e order p)
  where
    fst (x, _, _, _, _) = x
    snd (_, y, _, _, _) = y