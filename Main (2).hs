module Main where

grad :: Double -> Double -> Double -> (Double, Double, Double)
grad w1 w2 b =
  let dedw1 = (2 * (4 - w1 * 1 ** 2 - w2 * 1 - b) * (-2 * w1 * 1) + 2*(5 - w1 * (-1) ** 2 - w2 * (-1) - b) * (-2 * w1 * (-1)) + 2*(7 - w1 * 2 ** 2 - w2 * 2 - b) * (-2 * w1 * 2) + 2*(8 - w1 * (-2) ** 2 - w2 * (-2) - b) * (-2 * w1 * (-2)) + 2*(12 - w1 * 3 ** 2 - w2 * 3 - b) * (-2 * w1 * 3) + 2*(13 - w1 * (-3) ** 2 - w2 * (-3) - b) * (-2 * w1 * (-3)) + 2*(19 - w1 * 4 ** 2 - w2 * 4 - b) * (-2 * w1 * 4) + 2*(20 - w1 * (-4) ** 2 - w2 * (-4) - b) * (-2 * w1 * (-4)))
      dedw2 = (2 * (4 - w1 * 1 ** 2 - w2 * 1 - b) * (-1) + 2*(5 - w1 * (-1) ** 2 - w2 * (-1) - b) * (-(-1)) + 2*(7 - w1 * 2 ** 2 - w2 * 2 - b) * (-2) + 2*(8 - w1 * (-2) ** 2 - w2 * (-2) - b) * (-(-2)) + 2*(12 - w1 * 3 ** 2 - w2 * 3 - b) * (-3) + 2*(13 - w1 * (-3) ** 2 - w2 * (-3) - b) * (-(-3)) + 2*(19 - w1 * 4 ** 2 - w2 * 4 - b) * (-4) + 2*(20 - w1 * (-4) ** 2 - w2 * (-4) - b) * (-(-4)))
      dedb = (2 * (4 - w1 * 1 ** 2 - w2 * 1 - b) * (-1) + 2*(5 - w1 * (-1) ** 2 - w2 * (-1) - b) * (-1) + 2*(7 - w1 * 2 ** 2 - w2 * 2 - b) * (-1) + 2*(8 - w1 * (-2) ** 2 - w2 * (-2) - b) * (-1) + 2*(12 - w1 * 3 ** 2 - w2 * 3 - b) * (-1) + 2*(13 - w1 * (-3) ** 2 - w2 * (-3) - b) * (-1) + 2*(19 - w1 * 4 ** 2 - w2 * 4 - b) * (-1) + 2*(20 - w1 * (-4) ** 2 - w2 * (-4) - b) * (-1))
   in (dedw1, dedw2, dedb)

tol :: Double
tol = 10 ** (-4)

descent :: Double -> Double -> Double -> Double -> Double -> Int -> (Double, Double, Double, Int)
descent lr xt yt zt err i
  | err < tol = (xt, yt, zt, i)
  | otherwise =
      let (dedw1, dedw2, dedb) = grad xt yt zt
          xnovo = xt - lr * dedw1
          ynovo = yt - lr * dedw2
          znovo = zt - lr * dedb
          errnovo = sqrt ((xnovo - xt) ** 2 + (ynovo - yt) ** 2 + (znovo - zt) ** 2)
       in descent lr xnovo ynovo znovo errnovo (i + 1)

main = do
  putStrLn "Digite os valores para executar a função descent:"
  input <- getLine
  let [lr, xt, yt, zt, err, i] = map read $ words input
      (x, y, z, iter) = descent lr xt yt zt err (round i)
  putStrLn $ "Ponto mínimo encontrado: (" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"
  putStrLn $ "Número de iterações: " ++ show iter

