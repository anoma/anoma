defmodule AnomaTest.Nock do
  use TestHelper.TestMacro, async: true

  alias Examples.ENock

  doctest(Nock)
  doctest(Noun)
  doctest(Noun.Format)

  test "examples" do
    ENock.dec()
    ENock.factorial()
    ENock.sign_detatched()
    ENock.verify_detatched()
    ENock.sign()
    ENock.verify()
    ENock.bex()
    ENock.mix()
    ENock.shax()
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
    ENock.one_two()
    ENock.nesting_noun()
    ENock.incorrectly_ending()
    ENock.incorrectly_starting()
    ENock.indexed_noun()
    ENock.replacing_terms()
  end
end
