{-# LANGUAGE MultiWayIf #-}
module Text.Regex.Trigram where

import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.List (foldl')
import           Text.Regex.TDFA.Pattern (Pattern(..))
import           Text.Regex.TDFA.ReadRegex (parseRegex)

data Query = QAll
           | QNone
           | QAnd !(HashSet String) ![Query]
           | QOr !(HashSet String) ![Query]
           deriving (Eq, Show)

andQ :: Query -> Query -> Query
andQ QNone _ = QNone
andQ _ QNone = QNone
andQ QAll q = q
andQ q QAll = q
andQ (QAnd ts qs) (QAnd ts' qs') = QAnd (HS.union ts ts') (qs ++ qs')
andQ q1@QOr{} q2@QOr{} = QAnd HS.empty [q1, q2]
andQ (QAnd ts qs) qOr@QOr{} = QAnd ts (qOr : qs)
andQ qOr@QOr{} (QAnd ts qs) = QAnd ts (qOr : qs)

orQ :: Query -> Query -> Query
orQ QNone q = q
orQ q QNone = q
orQ QAll _ = QAll
orQ _ QAll = QAll
orQ (QOr ts qs) (QOr ts' qs') = QOr (HS.union ts ts') (qs ++ qs')
orQ q1@QAnd{} q2@QAnd{} = QOr HS.empty [q1, q2]
orQ (QOr ts qs) qAnd@QAnd{} = QOr ts (qAnd : qs)
orQ qAnd@QAnd{} (QOr ts qs) = QOr ts (qAnd : qs)

data RegexInfo = RegexInfo { emptyValid :: !Bool
                           , exact :: !(HashSet String)
                           , prefix :: !(HashSet String)
                           , suffix :: !(HashSet String)
                           , query :: !Query
                           } deriving (Eq, Show)

emptyRI :: RegexInfo
emptyRI = RegexInfo { emptyValid = True
                    , exact = HS.singleton ""
                    , prefix = HS.empty
                    , suffix = HS.empty
                    , query = QAll
                    }

singletonRI :: Char -> RegexInfo
singletonRI char = RegexInfo { emptyValid = False
                             , exact = HS.singleton [char]
                             , prefix = HS.singleton [char]
                             , suffix = HS.singleton [char]
                             , query = QAll
                             }

analyze :: Pattern -> RegexInfo
analyze PEmpty = emptyRI
analyze PChar{getPatternChar = char} = singletonRI char
analyze PEscape{getPatternChar = char} = singletonRI char
analyze (PQuest p) =
  RegexInfo { emptyValid = True
            , exact = HS.union (exact (analyze p)) (HS.singleton "")
            , prefix = HS.singleton ""
            , suffix = HS.singleton ""
            , query = QAll
            }
analyze PStar{} =
  RegexInfo { emptyValid = True
            , exact = HS.empty
            , prefix = HS.singleton ""
            , suffix = HS.singleton ""
            , query = QAll
            }
analyze (PPlus p) =
  (analyze p) { exact = HS.empty }
analyze (POr ps) =
  RegexInfo { emptyValid = any emptyValid ris
            , exact = HS.unions (map exact ris)
            , prefix = HS.unions (map prefix ris)
            , suffix = HS.unions (map suffix ris)
            , query = foldl' orQ QNone (map query ris)
            }
  where ris = map analyze ps
analyze (PConcat ps) =
  foldl1 conc (map analyze ps)
  where conc ri1 ri2 =
          RegexInfo { emptyValid = emptyValid ri1 && emptyValid ri2
                    , exact = if HS.null (exact ri1) || HS.null (exact ri2)
                              then HS.empty
                              else prod (exact ri1) (exact ri2)
                    , prefix = if | not (HS.null (exact ri1)) ->
                                    prod (exact ri1) (prefix ri2)
                                  | emptyValid ri1 ->
                                    HS.union (prefix ri1) (prefix ri2)
                                  | otherwise ->
                                    prefix ri1
                    , suffix = if | not (HS.null (exact ri2)) ->
                                    prod (suffix ri1) (exact ri2)
                                  | emptyValid ri2 ->
                                    HS.union (suffix ri1) (suffix ri2)
                                  | otherwise ->
                                    suffix ri2
                    , query = andQ (query ri1) (query ri2)
                    }
        prod as bs = HS.fromList [ a ++ b | a <- HS.toList as, b <- HS.toList bs ]
analyze PDot{} =
  RegexInfo { emptyValid = False
            , exact = HS.empty
            , prefix = HS.singleton ""
            , suffix = HS.singleton ""
            , query = QAll
            }
analyze (PGroup _ p) = analyze p
analyze (PNonCapture p) = analyze p
analyze PCarat{} = emptyRI
analyze PDollar{} = emptyRI
analyze (PNonEmpty p) = (analyze p) { emptyValid = False }
analyze PBound{} = error "PBound not supported yet"
analyze PAny{} = error "Character classes not supported yet"
analyze PAnyNot{} = error "Character classes not supported yet"

parseAndAnalyze :: String -> Maybe RegexInfo
parseAndAnalyze s = either (const Nothing) (Just . analyze . fst) (parseRegex s)
