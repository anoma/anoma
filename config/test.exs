import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :anoma_dashboard, AnomaDashboard.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base:
    "c8/XbDzuk5R+nwQ8K48SxX6gVYWMw/RiVPdlLm56096sJis+C2wWz6DsQ9lrL8XP",
  server: false
