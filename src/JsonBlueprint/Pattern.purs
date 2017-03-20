module JsonBlueprint.Pattern where

import Prelude
import Data.Maybe (Maybe)

data Pattern = Empty
             | BooleanLiteral Boolean
             | BooleanDataType
--             | StringLiteral String
             | StringDataType { minLength :: Maybe Int, maxLength :: Maybe Int }
--             | Choice Pattern Pattern

instance showPattern :: Show Pattern where
  show Empty = "Empty"
  show (BooleanLiteral b)    = show b
  show BooleanDataType       = "Boolean"
--  show (StringLiteral str)   = show str
  show (StringDataType opts) = "String"           -- TODO: append props
--  show (Choice p1 p2)        = (show p1) <> " | " <> (show p2)

-- or :: Pattern -> Pattern -> Pattern
-- or = Choice
