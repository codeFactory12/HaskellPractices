module HelloWorld where

    helloworld :: String
    helloworld = "Hello world"

    main = do
        helloworld

    helloworldntimes :: Int -> String
    helloworldntimes 0 = ""
    helloworldntimes n = "Hello world" ++ " " ++ helloworldntimes (n - 1)
