{- |
File        : ipv6ptr.hs
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Convert an IPv6 address to the format required for a reverse DNS (PTR) record.
-}
import           Data.Bits          (shiftR, (.&.))
import           Data.Char          (intToDigit)
import           Data.IP            (IPv6, fromIPv6b)
import           Data.List          (intercalate)
import           System.Environment (getArgs)
import           System.Exit        (die)
import           Text.Read          (readMaybe)


byteToNibbles :: Int -> [Int]
byteToNibbles x = map (0x0f .&.) [x `shiftR` 4, x]

nibblesToPtr :: [Int] -> String
nibblesToPtr = (++ ".ip6.arpa.") . intercalate "." . map (pure . intToDigit) . reverse

toPtr :: IPv6 -> String
toPtr = nibblesToPtr . concatMap byteToNibbles . fromIPv6b


main :: IO ()
main = do
  args <- getArgs
  case args of
    [str] -> maybe (die ("Not an IPv6 address: " ++ str)) (putStrLn . toPtr) $ readMaybe str
    _     -> die "Usage: ipv6ptr <IPv6 address>"
