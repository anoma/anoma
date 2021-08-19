# Fungible token encoding and template

The Heliax team implemented an intent encoding, a filter program template, and a
matchmaker program template that can be used to exchange fungible tokens between
any number of participants.

## Intent encoding
The intent encoding allows the expression of a desire to participate in an asset
exchange. The encoding is defined as follows :

```protobuf
message FungibleToken {
  string address = 1;
  string token_sell = 2;
  int64 max_sell = 3;
  int64 rate_min = 4;
  string token_buy = 5;
  int64 min_buy = 6;
  google.protobuf.Timestamp expire = 7;
}
```

## Matchmaker program

The filter program attempts to decode the intent and if successful, checks
that it's not yet expired and that the account address has enough funds for the
intended token to be sold.

The main program can match intents for exchanging assets. It does that by
creating a graph from all intents. When a cycle is found then it removes all
intents from that cycle of the mempool and crafts a transaction based on all the
removed intents.

![matchmaker](matchmaker_graph.svg)
[excalidraw link](https://excalidraw.com/#room=1db86ba6d5f0ccb7447c,2vvRd4X2Y3HDWHihJmy9zw)
