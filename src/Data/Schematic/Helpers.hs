module Data.Schematic.Helpers where

import Data.Schematic.Schema
import GHC.TypeLits


type UUIDRegex =
  'TRegex "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"

type IsUUID = '[UUIDRegex]

-- [ "1985-04-12T23:20:50.52Z"
-- , "1996-12-19T16:39:57-08:00"
-- , "1990-12-31T23:59:60Z"
-- , "1990-12-31T15:59:60-08:00"
-- , "1937-01-01T12:00:27.87+00:20"
-- ]

-- components
type ISO8601Date = "[1-9][0-9]{3}-(0[1-9]|1[0-2])-[0-3][0-9]"

type ISO8601Time = "(?:[0-2][0-9]:[0-2][0-9]:[0-6][0-9])?"

type ISO8601DateTime = AppendSymbol ISO8601Date (AppendSymbol "T" ISO8601Time)

type ISO8601UTC = "(Z|\\+00:00)"

type ISO8601DateTimeUTC =
  AppendSymbol ISO8601Date (AppendSymbol "T" (AppendSymbol ISO8601Time ISO8601UTC))

type ISO8601TZ = "\\+[0-1][0-9]:[0-6][0-9]"

type ISO8601DateTimeZoned =
  AppendSymbol ISO8601Date (AppendSymbol "T" (AppendSymbol ISO8601Time ISO8601TZ))

-- regexes
type ISO8601DateRegex = 'TRegex ISO8601Date

type ISO8601TimeRegex = 'TRegex ISO8601Time

type ISO8601DateTimeRegex = 'TRegex ISO8601DateTime

type ISO8601DateTimeRegexUTC = 'TRegex ISO8601DateTimeUTC

type ISO8601DateTimeRegexZoned = 'TRegex ISO8601DateTimeZoned

-- constraints
type IsDate = '[ISO8601DateRegex]

type IsTime = '[ISO8601TimeRegex]

type IsDateTime = '[ISO8601DateTimeRegex]

type IsZonedDateTime = '[ISO8601DateTimeRegexZoned]

type IsUTCDateTime = '[ISO8601DateTimeRegexUTC]
