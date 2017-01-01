{-# LANGUAGE RecordWildCards #-}

module Prover where

import Formula
import Axioms
import Control.Monad

robinsInput :: [ (String, Bool) ]
robinsInput = [ ("-(p>p)" , False)
              , ("-(p>(q>p))" , False)
              , ("-((p>q)>p)" , True)
              , ("--((pvq)>(-p^-q))", True)
              , ("(p^-p)", False)
              , ("((p>q)^(q>p))", True)
              , ("-((p>(qvr))>((p>q)v(p>r)))", False)
              , ("((p>(qvr))>((p>q)v(p>r)))", True)
              , ("((p>q)^(-q>-p))", True)
              , ("((pvq)^p>-q)", False) -- not a Fmla
              ]
runRobinsTests :: [(Bool, Bool)]
runRobinsTests = map (\(fmla, expected) -> (satisfiable' fmla, expected)) robinsInput

passTests :: Bool
passTests = foldl (&&) True $ [ x == y | (x, y) <- runRobinsTests ]

satisfiable :: Fmla -> Bool
satisfiable fmla = case (buildT >=> closedT) fmla of
  Nothing -> False
  _       -> True

satisfiable' :: String -> Bool
satisfiable' str = case (parseFmla >=> buildT >=> closedT) str of
  Nothing -> False
  _       -> True

data Tableau = Node Fmla Tableau Tableau | Empty

instance Show Tableau where
  show Empty = " *"
  show (Node f left right) =
    show f ++ " [" ++ show left ++ "] [" ++ show right ++ "] "

leaf :: Fmla -> Tableau
leaf f = Node f Empty Empty

node :: Tableau -> Fmla -> Tableau -> Tableau
node left f right = Node f left right

addAlpha :: Tableau -> Fmla -> Tableau
addAlpha Empty f = leaf f
addAlpha (Node root Empty Empty) f = Node root (leaf f) Empty
addAlpha (Node root left  Empty) f = Node root (addAlpha left f) Empty
addAlpha (Node root left right)  f = Node root left' right'
  where
    left' = addAlpha left f
    right'= addAlpha right f

addBeta :: Tableau -> Fmla -> Fmla -> Tableau
addBeta (Node root Empty Empty) f g = Node root (leaf f) (leaf g)
addBeta (Node root left  Empty) f g = Node root (addBeta left f g) Empty
addBeta (Node root left  right) f g = Node root left' right'
  where
    left'  = addBeta left  f g
    right' = addBeta right f g

expandTableau :: Tableau -> Fmla -> Tableau
expandTableau Empty _ = Empty
expandTableau tab (Prop c) = tab
expandTableau tab (Conjunction f g) = addAlpha (addAlpha tab f) g
expandTableau tab (Disjunction f g) = addBeta tab f g
expandTableau tab (Implication f g) = addBeta tab (negFmla f) g
expandTableau tab (Neg (Prop c))  = tab
expandTableau tab (Neg (Neg (Prop c))) = addAlpha tab (Prop c)
expandTableau tab (Neg (Neg f)) = addAlpha tab f
expandTableau tab (Neg (Disjunction f g)) =
  expandTableau tab $ Conjunction (negFmla f) (negFmla g)
expandTableau tab (Neg (Conjunction f g)) =
  expandTableau tab $ Disjunction (negFmla f) (negFmla g)
expandTableau tab (Neg (Implication f g)) =
  expandTableau tab $ Conjunction f (negFmla g)

buildT :: Fmla -> Maybe Tableau
buildT fmla = Just (buildT' start)
  where
    start = leaf fmla
    buildT' :: Tableau -> Tableau
    buildT' Empty = Empty
    buildT' (Node f l r) = do
      let (Node root left right) = expandTableau (Node f l r) f
      Node root (buildT' left) (buildT' right)

closedT :: Tableau -> Maybe String
closedT tab  | closedT' tab []   = Nothing
             | otherwise         = Just "satisfiable"
  where
    closedT' :: Tableau -> [Fmla] -> Bool
    -- basecases, we found a leaf
    closedT' (Node (Neg (Prop c)) Empty Empty) props = (Prop c) `elem` props
    closedT' (Node (Prop c) Empty Empty) props = (Neg (Prop c)) `elem` props
    -- we found a prop/neg prop, but have children
    closedT' (Node (Prop c) left Empty) props
      | (Neg (Prop c)) `elem` props = True
      | otherwise                   = closedT' left $ (Prop c) : props

    closedT' (Node (Neg (Prop c)) left Empty) props
      | (Prop c) `elem` props   = True
      | otherwise               = closedT' left $ (Neg (Prop c)) : props

    -- found a proposition, but have two children
    closedT' (Node (Prop c) left right) props
      | (Neg (Prop c)) `elem` props = True
      | otherwise                   =    (closedT' left  props')
                                      && (closedT' right props')
       where
        props' = (Prop c) : props

    closedT' (Node (Neg (Prop c)) left right) props
      | (Prop c) `elem` props       = True
      | otherwise                   =    (closedT' left  props')
                                      && (closedT' right props')
        where
          props' = (Neg (Prop c)) : props

    closedT' (Node _ left Empty) props =     closedT' left  props
    closedT' (Node _ left right) props =    (closedT' left  props)
                                         && (closedT' right props)

-- QuickCheck in order to check if the prover is correct

prop_NotSatisfiable1 :: Fmla -> Fmla -> Bool
prop_NotSatisfiable1 f g = (satisfiable $ negFmla $ createAxiom1 f g) == False

prop_Satisfiable1 :: Fmla -> Fmla -> Bool
prop_Satisfiable1 f g = satisfiable $ createAxiom1 f g

prop_NotSatisfiable2 :: Fmla -> Fmla -> Fmla -> Bool
prop_NotSatisfiable2 f g h = (satisfiable $ negFmla $ createAxiom2 f g h) == False

prop_Satisfiable2 :: Fmla -> Fmla -> Fmla -> Bool
prop_Satisfiable2 f g h = satisfiable $ createAxiom2 f g h

prop_NotSatisfiable3 :: Fmla -> Fmla -> Bool
prop_NotSatisfiable3 f g = (satisfiable $ negFmla $ createAxiom3 f g) == False

prop_Satisfiable3 :: Fmla -> Fmla -> Bool
prop_Satisfiable3 f g = satisfiable $ createAxiom3 f g

prop_Satisfiable4 ::Fmla -> Bool
prop_Satisfiable4 f = satisfiable $ createAxiom4 f

prop_NotSatisfiable4 :: Fmla ->  Bool
prop_NotSatisfiable4 f = (satisfiable $ negFmla $ createAxiom4 f) == False

prop_robinTests :: Bool
prop_robinTests = passTests