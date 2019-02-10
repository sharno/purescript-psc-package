module Types 
  ( PackageName
  , PackageNameError
  , mkPackageName
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Array (all, elem, filter, head, last, length, notElem, null)
import Data.Char.Unicode (isAscii, isDigit, isLower)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), contains)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

newtype PackageName = PackageName String
derive instance newtypePackageName :: Newtype PackageName _
derive newtype instance showPackageName :: Show PackageName
derive newtype instance eqPackageName :: Eq PackageName
derive newtype instance ordPackageName :: Ord PackageName
derive newtype instance encodeJsonPackageName :: EncodeJson PackageName
derive newtype instance decodeJsonPackageName :: DecodeJson PackageName

data PackageNameError
  = NotEmpty
  | TooLong Int
  | InvalidChars String
  | RepeatedSeparators
  | MustNotBeginSeparator
  | MustNotEndSeparator
derive instance genericPerson :: Generic PackageNameError _
instance showPackageNameError :: Show PackageNameError where show = genericShow
derive instance eqPackageNameError :: Eq PackageNameError
derive instance ordPackageNameError :: Ord PackageNameError


mkPackageName :: String -> Either PackageNameError PackageName
mkPackageName = map PackageName <<< validateAll validators
  where
    dashOrDot = ['-', '.']
    validateAll vs x = traverse (validateWith $ toCharArray x) vs *> Right x
    validateWith x (Tuple p err)
      | p x       = Right x
      | otherwise = Left (err x)
    validChar c = isAscii c && (isLower c || isDigit c || c `elem` dashOrDot)

    validators :: Array (Tuple (Array Char -> Boolean) (Array Char -> PackageNameError))
    validators =
        [ Tuple (not <<< null) (const NotEmpty)
        , Tuple (all validChar) (InvalidChars <<< fromCharArray <<< filter (not <<< validChar))
        , Tuple (firstChar (_ `notElem` dashOrDot)) (const MustNotBeginSeparator)
        , Tuple (lastChar (_ `notElem` dashOrDot)) (const MustNotEndSeparator)
        , Tuple (not <<< contains (Pattern "--") <<< fromCharArray) (const RepeatedSeparators)
        , Tuple (not <<< contains (Pattern "..") <<< fromCharArray) (const RepeatedSeparators)
        , Tuple (length >>> (_ <= 50)) (TooLong <<< length)
        ]
    firstChar p str = maybe false p (head str)
    lastChar p str = maybe false p (last str)
