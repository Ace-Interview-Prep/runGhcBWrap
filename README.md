# Ace RunGHC Server

### Build & Run

``` shell
nix-build && ./result/bin/server
```

### Example request

``` shell
curl --location 'localhost:8080' \
--header 'Content-Type: text/plain' \
--data 'currentTime <- getCurrentTime; putStrLn $ "The current time is: " <> (show currentTime)'
```

