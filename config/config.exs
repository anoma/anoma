import Config

config :logger,
  level: :error,
  handle_otp_reports: true,
  handle_sasl_reports: true

config :anoma_client, []
config :anoma_lib, []
config :anoma_node, []
config :anoma_protobuf, []
config :compile_protoc, []
config :event_broker, []

############################################################
#                       Mnesia                             #
############################################################

# persist_to_disk: should the mnesia data be written to disk?
#                  default: false
#
# data_dir:        the directory where the data will be written,
#                  if persisted to disk
#                  default: platform dependent
#                           linux: `$XDG_DATA_HOME/anoma` or `~/.config/anoma `
#                           macos: `~/Library/Application Support/Anoma`
#
# rocksdb:         should the rockdb backend be used?
#                  default: true
config :anoma_node, :mnesia,
  persist_to_disk: false,
  rocksdb: false

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
if File.exists?("config/#{config_env()}.exs") do
  import_config "#{config_env()}.exs"
end
