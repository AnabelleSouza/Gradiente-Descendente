type R = Double

f :: R -> R -> R -> R
f x y z = (x-2)**2 + (y+3)**2 + (z-1)**2

grad :: R -> R -> R -> (R, R, R)
grad x y z = (2*(x-2), 2*(y+3), 2*(z-1))

descent :: R -> R -> R -> R -> R -> Int -> R -> (R, R, R, Int)
descent lr x y z err i tol
  | err < tol = (x, y, z, i)
  | otherwise = descent lr x' y' z' err' (i+1) tol
  where
    (dx, dy, dz) = grad x y z
    x' = x - lr*dx
    y' = y - lr*dy
    z' = z - lr*dz
    err' = f x' y' z'

main :: IO ()
main = do
  putStr "Digite o valor de x0: "
  x0 <- readLn
  putStr "Digite o valor de y0: "
  y0 <- readLn
  putStr "Digite o valor de z0: "
  z0 <- readLn
  let tol = 1e-16
  putStr "Digite o valor do learning rate: "
  lr <- readLn
  let (xf, yf, zf, n) = descent lr x0 y0 z0 (f x0 y0 z0) 0 tol
  putStrLn $ "Ponto de mínimo: (" ++ show xf ++ ", " ++ show yf ++ ", " ++ show zf ++ ")"
  putStrLn $ "Valor mínimo: " ++ show (f xf yf zf)
  putStrLn $ "Número de iterações: " ++ show n
