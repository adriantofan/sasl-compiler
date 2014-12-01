{-# LANGUAGE TemplateHaskell #-}
import SASL
import Grammar
import Test.QuickCheck
import Test.QuickCheck.All 
import Data.Maybe
prop_name = Just "_a_s_" == parse_result name "_a_s_"
prop_str = Str "\"asd\"" == fromJust ((parse_result str)  "\"\\\"asd\\\"\"") 
prop_ascii_escape_dquote = '"' ==  fromJust ((parse_result escaped_ascii) "\\\"")
prop_ascii_dquote = parse escaped_ascii "\"" == []
return []
runTests = $quickCheckAll
main = runTests