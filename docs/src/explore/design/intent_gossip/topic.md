# Topic

A topic is string and an encoding that describes this sub-network. In a topic
all intents use the exact same encoding. That encoding is known by matchmakers
so it can decode them to find matches. Whenever a node subscribes to a new topic
it informs all connected nodes and each of them propagate it. With this it’s
easy to create new topics in the intent gossip network and inform others.

Other nodes can choose to subscribe to a new topic with the help of a
filter. This filter is defined as a combination of a whitelist, a regex
expression, and a maximum limit.
