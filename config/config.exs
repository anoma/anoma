import Config

config :logger,
  level: :warning

import_config "#{config_env()}.exs"
