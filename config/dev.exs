import Config

# ----------------------------------------------------------------------------
# Logger

# note: to hide the SASL reports in debugging, set `handle_sasl_reports: false`.
#
# Example sasl report:
# 16:13:44.310 [info] Child {:acceptor, #PID<0.3651.0>, 3} of Supervisor #PID<0.3651.0> (:ranch_acceptors_sup) started
# Pid: #PID<0.3654.0>
# Start Call: :ranch_acceptor.start_link(#Port<0.123>, :ranch_tcp, :error_logger, #PID<0.3650.0>)
# Restart: :permanent
# Shutdown: :brutal_kill
# Type: :worker
config :logger,
  level: :debug,
  handle_otp_reports: true,
  handle_sasl_reports: true
