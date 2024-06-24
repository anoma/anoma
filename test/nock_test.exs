defmodule AnomaTest.Nock do
  use TestHelper.TestMacro, async: true

  alias Examples.ENock

  doctest(Nock)

  test "examples" do
    ENock.dec()
    ENock.factorial()
    ENock.sign_detatched()
    ENock.verify_detatched()
    ENock.sign()
    ENock.verify()
    ENock.bex()
    ENock.mix()
    ENock.met0()
    ENock.met1()
    ENock.met2()
    ENock.lsh0()
    ENock.lsh1()
    ENock.lsh2()
    ENock.rsh0()
    ENock.rsh1()
    ENock.rsh2()
    ENock.uend0()
    ENock.uend1()
    # Next one calls this... so uneeded but who cares
    ENock.successful_inc()
    ENock.unsuccessful_inc()
    ENock.jamming_and_cueing()
  end
end
