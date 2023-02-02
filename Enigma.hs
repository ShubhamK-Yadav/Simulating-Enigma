{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int) 
  type Reflector = [(Char, Char)] 
  type Offsets = (Int,Int,Int) 
  type Stecker = [(Char, Char)]
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  -- recursively calls encode and changes offset as a character is taken as input each time and rotates rotor
  encodeMessage :: String -> Enigma -> String
  encodeMessage "" _ = ""
  encodeMessage (l:ls) (SimpleEnigma rotorL rotorM rotorR reflector (x,y,z)) = 
    let (x1,x2,x3) = (changeOffsets rotorL rotorM rotorR (x,y,z)) 
        rotorl = rotate rotorL x1
        rotorm =  rotate rotorM x2
        rotorr =  (rotate rotorR x3) in
          encode (SimpleEnigma rotorl rotorm rotorr reflector (x1,x2,x3)) l : 
            encodeMessage ls (SimpleEnigma rotorL rotorM rotorR reflector (x1,x2,x3))

  encodeMessage (l:ls) (SteckeredEnigma rotorL rotorM rotorR  reflector (x,y,z) stecker) =
    let (x1,x2,x3) = (changeOffsets rotorL rotorM rotorR (x,y,z)) 
        rotorl = rotate rotorL x1
        rotorm =  rotate rotorM x2
        rotorr =  (rotate rotorR x3) in
          encode (SteckeredEnigma rotorL rotorM rotorR  reflector (x,y,z) stecker) l : 
            encodeMessage ls (SteckeredEnigma rotorL rotorM rotorR  reflector (x,y,z) stecker)

  
  -- encodes one character
  encode :: Enigma -> Char -> Char
  encode (SimpleEnigma rotorL rotorM rotorR reflector (x,y,z)) c = goThroughAllInverse rotorL rotorM rotorR . reflect reflector . 
    goThroughAll rotorL rotorM rotorR $ c 
  encode (SteckeredEnigma rotorL rotorM rotorR  reflector (x,y,z) stecker) c = plug stecker . 
    goThroughAllInverse rotorL rotorM rotorR . reflect reflector . goThroughAll rotorL rotorM rotorR . plug stecker $ c

  goThrough :: Rotor -> Char -> Char
  goThrough rotor c = (fst rotor) !! (alphaPos c)

  goThroughInverse :: Rotor -> Char -> Char
  goThroughInverse rotor c = maybe c id $ lookup c invertedrotor
          where invertedrotor = zip (fst rotor) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  goThroughAll :: Rotor -> Rotor -> Rotor -> Char -> Char
  goThroughAll rotorL rotorM rotorR c = goThrough rotorL . goThrough rotorM . goThrough rotorR $ c

  goThroughAllInverse :: Rotor -> Rotor -> Rotor -> Char -> Char
  goThroughAllInverse rotorL rotorM rotorR c = goThroughInverse rotorR . goThroughInverse  rotorM . goThroughInverse rotorL $ c
  
  let2int :: Char -> Int
  let2int c = ord c - (ord 'A')

  int2let :: Int -> Char
  int2let n = chr (ord 'A' + n)

  shift :: Int -> Char -> Char
  shift n c | isUpper c = int2let ((let2int c + n) `mod` 26)
            | otherwise = c

-- cipher the rotor string to match the output correctly
  cipher :: Int -> String -> String
  cipher n input = [shift n str | str <- input]
  cipher' n input = map (shift n) input
  cipher'' n [] = []
  cipher'' n (x:xs) = shift n x : cipher'' n xs

-- Used to match the rotor position with it's char value at each offset
  select_substr :: Int -> String -> String
  select_substr n rotor = reverse (take (length (rotor)- n) (reverse(rotor))) ++ take n rotor

  rotate :: Rotor -> Int -> Rotor
  rotate (str,int) n = (cipher (-n) .select_substr n $ str, int)

  -- Rotates left rotors when on the knockon position, not after
  changeOffsets :: Rotor -> Rotor -> Rotor -> Offsets -> Offsets
  changeOffsets rotorL rotorM rotorR (x,y,z) | z == ((snd rotorR)-1) && y == ((snd rotorM)-1) = ((x+1) `mod` 26, (y+1) `mod` 26, (z+1) `mod` 26) 
    | z == ((snd rotorR)-1) = (x, (y+1) `mod` 26, (z+1) `mod` 26)
    | otherwise = (x, y, (z+1) `mod` 26)

  swap :: (a,b) -> (b,a)
  swap (x,y) = (y,x)

  -- maybe 'c' is the default value, if the lookup doesn't find anything then the input is returned
  plug :: Stecker -> Char -> Char
  plug stecker c = maybe c id $ lookup c plugboard
                        where plugboard = (stecker ++ map swap stecker)
  
  -- maybe 'c' is the default value, if the lookup doesn't find anything then the input is returned
  reflect :: Reflector -> Char -> Char
  reflect reflector c = maybe c id $ lookup c reflected
                      where reflected = (reflector ++ map swap reflector)

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 26)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'

