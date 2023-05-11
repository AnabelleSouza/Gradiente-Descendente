import Text.Printf (printf)

grad :: Double -> Double -> Double -> (Double, Double, Double)
grad w1 w2 b =
  let dedw1 = 2 * ((1 - w1^2 - 4 * w1 - b) * (-2 * w1) + 2 * (2 - (-w2) - 5 * w1 - b) * (2 * w1) + 2 * (3 - 2 * w2 - 7 * w1 - b) * (-4 * w1) + 2 * (4 - (-2 * w2) - 8 * w1 - b) * (-4 * w1) + 2 * (5 - 3 * w2 - 12 * w1 - b) * (-6 * w1) + 2 * (6 - (-3 * w2) - 13 * w1 - b) * (-6 * w1) + 2 * (7 - 4 * w2 - 19 * w1 - b) * (-8 * w1) + 2 * (8 - (-4 * w2) - 20 * w1 - b) * (8 * w1))
  
      dedw2 = 2 * ((1 - w2 - 4 * w1 - b) * (-w1) + 2 * (2 - (-w2)^2 - 5 * w1 - b) * (-2) + 2 * (3 - 2 * w2^2 - 7 * w1 - b) * (-3) + 2 * (4 - (-2 * w2)^2 - 8 * w1 - b) * (-4) + 2 * (5 - 3 * w2^2 - 12 * w1 - b) * (-5) + 2 * (6 - (-3 * w2)^2 - 13 * w1 - b) * (-6) + 2 * (7 - 4 * w2 - 19 * w1 - b) * (-7) + 2 * (8 - (-4 * w2)^2 - 20 * w1 - b) * (-8))
      
      dedb = 2 * ((1 - w1^2 - 4 * w1 - b) * (-1)) + (2 * (2 - (-w2)^2 - 5 * w1 - b) * (-1)) + (2 * (3 - 2 * w2^2 - 7 * w1 - b) * (-1)) + (2 * (4 - (-2 * w2)^2 - 8 * w1 - b) * (-1)) + (2 * (5 - 3 * w2^2 - 12 * w1 - b) * (-1)) + (2 * (6 - (-3 * w2)^2 - 13 * w1 - b) * (-1)) + (2 * (7 - 4 * w2 - 19 * w1 - b) * (-1)) + (2 * (8 - (-4 * w2)^2 - 20 * w1 - b) * (-1))
  in (dedw1, dedw2, dedb)

tol :: Double
tol = 10 ** (-6)

descent :: Double -> Double -> Double -> Double -> Double -> Int -> (Double, Double, Double, Int)
descent lr xt yt zt err i
  | err < tol = (xt, yt, zt, i)
  | otherwise =
      let (dedw1, dedw2, dedb) = grad xt yt zt
          xnovo = xt - lr * dedw1
          ynovo = yt - lr * dedw2
          znovo = zt - lr * dedb
          errnovo = sqrt ((xnovo - xt) ^ 2 + (ynovo - yt) ^ 2 + (znovo - zt) ^ 2)
      in descent lr xnovo ynovo znovo errnovo (i + 1)

main :: IO ()
main = do
  let lr = 0.1
      x0 = 10
      x20 = 10
      b0 = 10
      erro0 = 999
      (x1, x21, b1, i) = descent lr x0 x20 b0 erro0 0
  putStrLn $ "Coeficientes encontrados: (" ++ show lr ++ ", " ++ show x0 ++ ", " ++ show x20 ++ ", " ++ show b0 ++ "," ++ show erro0 ++ ")"



