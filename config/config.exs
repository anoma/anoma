import Config

config :logger,
  level: :warning,
  handle_otp_reports: true,
  handle_sasl_reports: true

app = ~c"anoma_#{Mix.env()}"

data = :filename.basedir(:user_data, app) |> List.to_string()
config = :filename.basedir(:user_config, app) |> List.to_string()
env = config_env()

config :anoma,
  env: env,
  data: data,
  config: config

unless env == :test do
  File.mkdir_p!(data)
  File.mkdir_p!(config)

  config :mnesia,
    dir: ~c"#{data}"
end

import_config "#{config_env()}.exs"
