defmodule Glossary do
  use GlossaryBuilder

  define transaction_candidate do
    """
    A transaction candidate is `t:Noun.t/0` that evaluates to a valid or
    invalid `transaction` for a specified
    `t:Anoma.Node.Executor.Worker.backend/0`
    """
  end

  define anoma do
    """
    An intent-centric architecture decentralized counterparty discovery, solving,
    information flow control, and atomic multi‑chain settlement.
    """
  end
end
