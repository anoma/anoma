import Config

config :logger,
  level: :warning

config :anoma,
  env: config_env()

import_config "#{config_env()}.exs"
