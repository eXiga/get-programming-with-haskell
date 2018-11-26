module Lib where

type Host = String

type Resource = String

type ResourceId = String

type ApiKey = String

type URL = String

getRequestURL :: Host -> Resource -> ResourceId -> ApiKey -> URL
getRequestURL host resource resourceId apiKey =
  host ++ "/" ++ resource ++ "/" ++ resourceId ++ "?token=" ++ apiKey

genHostRequestBuilder :: Host -> Resource -> ResourceId -> ApiKey -> URL
genHostRequestBuilder host =
  (\resource resourceId apiKey -> getRequestURL host resource resourceId apiKey)

genApiRequestBuilder ::
     (Resource -> ResourceId -> ApiKey -> URL)
  -> Resource
  -> ResourceId
  -> ApiKey
  -> URL
genApiRequestBuilder hostBuilder resource apiKey =
  (\resourceId -> hostBuilder resource resourceId apiKey)

exampleUrlBuilder :: Resource -> ResourceId -> ApiKey -> URL
exampleUrlBuilder = genHostRequestBuilder "http://example.com"

bookApiUrlBuilder :: ResourceId -> URL
bookApiUrlBuilder resourceId =
  genApiRequestBuilder exampleUrlBuilder "book" "super_token" resourceId

subtract2 :: Integer -> Integer
subtract2 = flip (-) 2

binaryPartialApplication :: (a -> a -> a) -> a -> a -> a
binaryPartialApplication binaryFunction y = (\x -> binaryFunction x y)
