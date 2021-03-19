import Coding

import String2occurrence
import Occurrence2tree
import Tree2assoc
import AssocString2code

test1 = demo1 "Haskell"
test2 = demo1 "Mississippi"
test3 = demo1 "Massachusetts"
test4 = demo1 "Cincinnati"
test5 = demo1 "Donaudampfschiffahrtsgesellschaftskapitan"
test6 = demo1 "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Maecenas libero. Curabitur ligula sapien, pulvinar a vestibulum quis, facilisis vel sapien. Phasellus faucibus molestie nisl. Praesent in mauris eu tortor porttitor accumsan. Morbi imperdiet, mauris ac auctor dictum, nisl ligula egestas nulla, et sollicitudin sem purus in lacus. Et harum quidem rerum facilis est et expedita distinctio. Fusce suscipit libero eget elit. Quisque tincidunt scelerisque libero. Integer vulputate sem a nibh rutrum consequat. Fusce wisi. Nullam sit amet magna in magna gravida vehicula. Praesent in mauris eu tortor porttitor accumsan. Sed elit dui, pellentesque a, faucibus vel, interdum nec, diam. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aenean id metus id velit ullamcorper pulvinar. Pellentesque ipsum. Cras elementum. In dapibus augue non sapien. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. "
test7 = demo1 "It is an empty collection that contains an element."
test8 = demo1 ""
test9 = demo1 "PLLLLLLRRLLLLLLRLLLLLP"

test10 = demo2 "Haskell"
test11 = demo2 "Mississippi"
test12 = demo2 "Massachusetts"
test13 = demo2 "Cincinnati"
test14 = demo2 "Donaudampfschiffahrtsgesellschaftskapitan"
test15 = demo2 "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Maecenas libero. Curabitur ligula sapien, pulvinar a vestibulum quis, facilisis vel sapien. Phasellus faucibus molestie nisl. Praesent in mauris eu tortor porttitor accumsan. Morbi imperdiet, mauris ac auctor dictum, nisl ligula egestas nulla, et sollicitudin sem purus in lacus. Et harum quidem rerum facilis est et expedita distinctio. Fusce suscipit libero eget elit. Quisque tincidunt scelerisque libero. Integer vulputate sem a nibh rutrum consequat. Fusce wisi. Nullam sit amet magna in magna gravida vehicula. Praesent in mauris eu tortor porttitor accumsan. Sed elit dui, pellentesque a, faucibus vel, interdum nec, diam. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aenean id metus id velit ullamcorper pulvinar. Pellentesque ipsum. Cras elementum. In dapibus augue non sapien. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. "
test16 = demo2 "It is an empty collection that contains an element."
test17 = demo2 ""
test18 = demo2 "PLLLLLLRRLLLLLLRLLLLLP"


demo1 :: String -> IO ()
demo1 word = do
    putStrLn $ ">> encode \"" ++ word ++ "\""
    putStrLn $ "<< " ++ encoded
    putStrLn ""
    putStrLn $ ">> decode " ++ encoded
    putStrLn $ "<< " ++ (decode $ encode word)
    where 
        encoded = show $ encode word

demo2 :: String -> IO ()
demo2 word = do
    putStrLn $ ">> string2occurrence \"" ++ word ++ "\""
    putStrLn $ "<< " ++ show occurrence
    putStrLn ""
    putStrLn $ ">> occurrence2tree \"" ++ show occurrence ++ "\""
    putStrLn $ "<< " ++ show tree
    putStrLn ""
    putStrLn $ ">> tree2assoc \"" ++ show occurrence ++ "\""
    putStrLn $ "<< " ++ show tab
    putStrLn ""
    putStrLn $ ">> assoc2code \"" ++ show tab ++ "\""
    putStrLn $ "<< " ++ show code
    putStrLn ""
    putStrLn $ ">> decode (" ++ show code ++ ", " ++ show tab ++ ")"
    putStrLn $ "<< " ++ decode (code,tab)
    where
        occurrence = string2occurrence word
        tree = occurrence2tree occurrence
        tab = tree2assoc tree
        code = assocString2code tab word
