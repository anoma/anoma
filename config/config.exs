import Config

app = ~c"anoma"

env = config_env()
data = :filename.basedir(:user_data, app) |> List.to_string()

level =
  if env == :test do
    :none
  else
    :error
  end

config :logger,
  level: level,
  handle_otp_reports: true,
  handle_sasl_reports: true

unless env == :test do
  File.mkdir_p!(data)

  config :mnesia,
    dir: ~c"#{data}"
end

config :anoma_client, []
config :anoma_lib, []
config :anoma_node, []
config :anoma_protobuf, []
config :compile_protoc, []
config :event_broker, []
