main :: IO ()
main = do
  let a = ['a', 'b', 'c']
  let b = ('a', 'b', 'c')
  let c = [(False, '0')]
  let d = ([False, True], ['0', '0']) -- ([Bool], [Char])
  let e = [tail, init, reverse] -- [[a] -> [a]]
  print ()
