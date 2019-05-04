import Data.Map

data Card =  V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 | J | Q | K | A | R

canPlay :: Card -> Card -> Bool
canPlay x V2 = x /= V9
canPlay x V10 = x /= V9
canPlay V2 _ = True
canPlay V3 _ = True
canPlay V4 x = x `elem` [V4,V5,V6,V7,V8,V9,V10,J,Q,K,A,R]
canPlay V5 x = x `elem` [V5,V6,V7,V8,V9,V10,J,Q,K,A,R]
canPlay V6 x = x `elem` [V6,V7,V8,V9,V10,J,Q,K,A,R]
canPlay V7 x = x `elem` [V7,V8,V9,V10,J,Q,K,A,R]
canPlay V8 x = x `elem` [V8,V9,V10,J,Q,K,A,R]
canPlay V9 x = x `elem` [V3,V4,V5,V6,V7,V8,V9]
canPlay V10 x = x `elem` [V10,J,Q,K,A,R]
canPlay J x = x `elem` [J,Q,K,A,R]
canPlay Q x = x `elem` [Q,K,A,R]
canPlay K x = x `elem` [K,A,R]
canPlay A x = x `elem` [A,R]
canPlay R x = x `elem` [V3,R]

type Cards = Map Card Int

data State = State {
  hand1 :: Cards,
  hand2 :: Cards,
  pile  :: [(Card,Int)]
}


