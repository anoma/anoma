import Config

config :logger,
  level: :warning,
  handle_otp_reports: true,
  handle_sasl_reports: true

import_config "#{config_env()}.exs"
