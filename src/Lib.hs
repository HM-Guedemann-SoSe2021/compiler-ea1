module Lib
    ( State(..)
    , delta
    , sAccept
    , accept
    ) where


-- Dieses Modul realisiert einen endlichen Automaten, der 'n', 'e', 'w'
-- akzeptiert.
-- Als Zustandsdiagramm:
-- -> s0 -n-> s1 -e-> s2 -w-> |s3|
-- wobei s3 der akzeptierende Zustand ist.

-- `data` definiert einen neuen Typ. Im Gegensatz zu `newtype` können hier
-- mehrere Konstruktoren verwendet werden. Hier wird jeder Zustand auf einen
-- Typkonstruktor abgebildet.
data State = S0
           | S1
           | S2
           | S3
           | Serror
  deriving (Eq, Show)

-- | `sAccept` ist die Liste von akzeptierenden Zuständen, in diesem Fall also
-- genau der Zustand `S3`.
sAccept :: [State]
sAccept = [S3]

-- | `delta` ist die Zustandsübergangfunktion. Diese Funktion bildet einen
-- Zustand und ein Zeichen auf einen Nachfolgezustand ab.
delta :: State -> Char -> State
delta S0 'n' = S1
delta S1 'e' = S2
delta S2 'w' = S3
delta _   _  = Serror

start :: State
start = S0

-- | `accept` testet ob ein gegebener String von dem endlichen Automated
-- akzeptiert wird. Es beginnt im Startzustand `S0` und wendet die
-- Zustandsübergangfunktion auf die Zeichen des Strings an.
accept :: String -> Bool
accept word = scan word start `elem` sAccept
  where scan :: String -> State -> State
        scan [] s = s
        scan (w:rest) s = scan rest (delta s w)
