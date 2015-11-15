

-- UUAGC 0.9.52.1 (test.ag)

{-# LINE 1 "test.ag" #-}
type Delta = Int
{-# LINE 8 "test.hs" #-}
-- Index -------------------------------------------------------
data Index = Start
           | Next (Index) (Delta)
-- cata
sem_Index :: Index ->
             T_Index
sem_Index (Start) =
    (sem_Index_Start)
sem_Index (Next _prev _today) =
    (sem_Index_Next (sem_Index _prev) _today)
-- semantic domain
type T_Index = ( Int,Int,Int)
sem_Index_Start :: T_Index
sem_Index_Start =
    (let _lhsOrate :: Int
         _lhsOlowest :: Int
         _lhsOhighest :: Int
         (_lhsOrate,_lhsOlowest,_lhsOhighest) =
             ({-# LINE 12 "test.ag" #-}
              (0,0,0)
              {-# LINE 29 "test.hs" #-}
              )
     in  ( _lhsOhighest,_lhsOlowest,_lhsOrate))
sem_Index_Next :: T_Index ->
                  Delta ->
                  T_Index
sem_Index_Next prev_ today_ =
    (let _lhsOhighest :: Int
         _lhsOlowest :: Int
         _lhsOrate :: Int
         _prevIhighest :: Int
         _prevIlowest :: Int
         _prevIrate :: Int
         _rate =
             ({-# LINE 13 "test.ag" #-}
              _prevIrate + today_
              {-# LINE 45 "test.hs" #-}
              )
         _lowest =
             ({-# LINE 14 "test.ag" #-}
              _prevIlowest `min` _rate
              {-# LINE 50 "test.hs" #-}
              )
         _profit =
             ({-# LINE 15 "test.ag" #-}
              _rate     - _lowest
              {-# LINE 55 "test.hs" #-}
              )
         _lhsOhighest =
             ({-# LINE 16 "test.ag" #-}
              _prevIhighest `max` _profit
              {-# LINE 60 "test.hs" #-}
              )
         _lhsOlowest =
             ({-# LINE 8 "test.ag" #-}
              _lowest
              {-# LINE 65 "test.hs" #-}
              )
         _lhsOrate =
             ({-# LINE 7 "test.ag" #-}
              _rate
              {-# LINE 70 "test.hs" #-}
              )
         ( _prevIhighest,_prevIlowest,_prevIrate) =
             prev_
     in  ( _lhsOhighest,_lhsOlowest,_lhsOrate))