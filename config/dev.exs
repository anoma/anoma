import Config

# refresh the open api spec during development, don't keep it in ETS cache.
config :open_api_spex, :cache_adapter, OpenApiSpex.Plug.NoneCache
