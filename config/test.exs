import Config

config :logger,
  level: :error

# rocksdb is disabled for testing because it slows tests down too much
config :anoma_node, :mnesia,
  persist_to_disk: false,
  rocksdb: false
