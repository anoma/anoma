ExUnit.start()

# the ets table for created nodes is used by all test modules that use
# an example node.
# when a module creates this table, it's destroyed when that test module has been executed.
# this causes race conditions when other modules also require this ets table.
# the ets table is destroyed before the other module can use it.
# by creating the ets table here, it is ensured to exist throughout the entire test suite.
#
# the function is still called by the e_node module, to ensure that the table is also created
# when the tests are run as examples.
Anoma.Node.Examples.ENode.initialize_ets()
