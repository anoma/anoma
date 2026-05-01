defmodule Examples.ESparseMerkleTree do
  @moduledoc false

  import ExUnit.Assertions

  @empty_hash <<0::256>>
  @data_hash_a <<202, 151, 129, 18, 202, 27, 189, 202, 250, 194, 49, 179, 154,
                 35, 220, 77, 167, 134, 239, 248, 20, 124, 78, 114, 185, 128,
                 119, 133, 175, 238, 72, 187>>
  @data_hash_b <<62, 35, 232, 22, 0, 57, 89, 74, 51, 137, 79, 101, 100, 225,
                 177, 52, 139, 189, 122, 0, 136, 212, 44, 74, 203, 115, 238,
                 174, 213, 156, 0, 157>>
  @data_hash_c <<46, 125, 44, 3, 169, 80, 122, 226, 101, 236, 245, 181, 53,
                 104, 133, 165, 51, 147, 162, 2, 157, 36, 19, 148, 153, 114,
                 101, 161, 162, 90, 239, 198>>
  @data_hash_h <<170, 169, 64, 38, 100, 241, 164, 31, 64, 235, 188, 82, 201,
                 153, 62, 182, 106, 235, 54, 102, 2, 149, 143, 223, 170, 40,
                 59, 113, 230, 77, 177, 35>>
  @branch_hash_a1 <<209, 175, 3, 72, 43, 158, 186, 64, 26, 85, 38, 143, 186,
                    218, 194, 127, 50, 7, 38, 121, 71, 160, 32, 212, 66, 10,
                    25, 243, 124, 56, 251, 96>>
  @branch_hash_a2 <<247, 57, 2, 118, 31, 93, 148, 3, 66, 133, 27, 113, 124,
                    29, 35, 188, 31, 120, 177, 233, 61, 85, 77, 178, 181, 91,
                    14, 244, 160, 69, 222, 143>>
  @branch_hash_b1 <<142, 235, 26, 197, 47, 107, 234, 186, 93, 242, 205, 33,
                    177, 251, 119, 120, 254, 205, 107, 42, 8, 140, 248, 197,
                    238, 248, 219, 38, 99, 239, 229, 20>>
  @branch_hash_b2 <<27, 54, 1, 142, 135, 140, 102, 107, 75, 108, 251, 69, 40,
                    226, 193, 168, 166, 216, 73, 128, 68, 106, 173, 4, 154,
                    213, 240, 44, 70, 79, 233, 149>>
  @root_hash_a_b <<245, 21, 149, 200, 103, 150, 164, 226, 141, 208, 67, 41,
                   223, 158, 101, 147, 128, 50, 21, 135, 39, 15, 100, 190,
                   215, 204, 50, 8, 246, 114, 8, 97>>

  def hashes() do
    assert @data_hash_a = SparseMerkleTree.hash("a")
    assert @data_hash_b = SparseMerkleTree.hash("b")
    assert @data_hash_c = SparseMerkleTree.hash("c")
    assert @data_hash_h = SparseMerkleTree.hash("h")

    %{
      "a" => @data_hash_a,
      "b" => @data_hash_b,
      "c" => @data_hash_c,
      "h" => @data_hash_h
    }
  end

  def default_empty_tree() do
    SparseMerkleTree.new()
    |> tap(fn tree ->
      assert %SparseMerkleTree{
               root: :empty,
               depth: 256
             } == tree
    end)
  end

  def full_depth_tree() do
    tree = SparseMerkleTree.new()
    first_hash = SparseMerkleTree.hash("first")

    assert {:ok, {:inserted, tree}} = SparseMerkleTree.put(tree, first_hash)
    assert {:ok, {:present, ^tree}} = SparseMerkleTree.put(tree, first_hash)

    assert {:ok, {:inserted, tree}} =
             SparseMerkleTree.put(tree, SparseMerkleTree.hash("second"))

    assert {:ok, {:inserted, tree}} =
             SparseMerkleTree.put(tree, @empty_hash)

    tree = SparseMerkleTree.rehash(tree)

    root_hash =
      <<43, 186, 18, 123, 152, 194, 7, 239, 98, 188, 120, 98, 109, 139, 61,
        131, 142, 85, 140, 229, 158, 0, 12, 179, 122, 66, 98, 138, 146, 13, 7,
        94>>

    assert {:ok, ^root_hash} = SparseMerkleTree.root_hash(tree)

    assert {:ok, {:present, first_proof}} =
             SparseMerkleTree.proof(tree, first_hash)

    assert Enum.count(first_proof) == 256

    assert SparseMerkleTree.valid_proof?(
             first_hash,
             {:present, first_proof},
             root_hash
           )

    refute SparseMerkleTree.valid_proof?(
             first_hash,
             {:absent, first_proof},
             root_hash
           )

    other_hash = SparseMerkleTree.hash("other")

    assert {:ok, {:absent, other_proof}} =
             SparseMerkleTree.proof(tree, other_hash)

    assert Enum.count(other_proof) == 2

    refute SparseMerkleTree.valid_proof?(
             other_hash,
             {:present, other_proof},
             root_hash
           )

    assert SparseMerkleTree.valid_proof?(
             other_hash,
             {:absent, other_proof},
             root_hash
           )

    tree
  end

  defp tree_depth_3() do
    SparseMerkleTree.new(depth: 3)
  end

  defp tree_depth_3_with_a() do
    tree = tree_depth_3()
    assert {:ok, {:inserted, tree}} = SparseMerkleTree.put(tree, @data_hash_a)
    tree
  end

  defp tree_depth_3_with_a_rehash_b() do
    tree =
      tree_depth_3_with_a()
      |> SparseMerkleTree.rehash()

    assert {:ok, {:inserted, tree}} = SparseMerkleTree.put(tree, @data_hash_b)
    tree
  end

  defp tree_depth_3_with_a_b_rehash() do
    tree = tree_depth_3_with_a()
    assert {:ok, {:inserted, tree}} = SparseMerkleTree.put(tree, @data_hash_b)
    SparseMerkleTree.rehash(tree)
  end

  def tree_depth_3_structure() do
    tree = tree_depth_3()

    assert %SparseMerkleTree{
             root: :empty,
             depth: 3
           } == tree

    tree
  end

  def tree_depth_3_rehashing_does_nothing() do
    tree = tree_depth_3()
    assert ^tree = SparseMerkleTree.rehash(tree)
    tree
  end

  def tree_depth_3_proofs() do
    tree = tree_depth_3()
    root_hash = SparseMerkleTree.root_hash!(tree)

    assert {:ok, {:absent, []}} ==
             SparseMerkleTree.proof(tree, @data_hash_a)

    assert {:ok, {:absent, []}} ==
             SparseMerkleTree.proof(tree, @data_hash_b)

    assert {:ok, {:absent, []}} == SparseMerkleTree.proof(tree, @empty_hash)

    assert SparseMerkleTree.valid_proof?(
             @data_hash_a,
             {:absent, []},
             root_hash
           )

    assert SparseMerkleTree.valid_proof?(
             @data_hash_b,
             {:absent, []},
             root_hash
           )

    assert SparseMerkleTree.valid_proof?(
             @empty_hash,
             {:absent, []},
             root_hash
           )

    tree
  end

  def tree_depth_3_proof_for_empty_hash_works_for_presence_or_absence() do
    tree = tree_depth_3()

    assert {:ok, {:inserted, tree}} =
             SparseMerkleTree.put(tree, @empty_hash)

    tree = SparseMerkleTree.rehash(tree)
    root_hash = SparseMerkleTree.root_hash!(tree)

    assert {:ok, {:present, proof}} =
             SparseMerkleTree.proof(tree, @empty_hash)

    assert SparseMerkleTree.valid_proof?(
             @empty_hash,
             {:present, proof},
             root_hash
           )

    assert SparseMerkleTree.valid_proof?(
             @empty_hash,
             {:absent, proof},
             root_hash
           )

    tree
  end

  def tree_depth_3_drop() do
    tree = tree_depth_3()

    assert {:ok, {:absent, tree}} ==
             SparseMerkleTree.drop(tree, @data_hash_a)

    assert {:ok, {:absent, tree}} ==
             SparseMerkleTree.drop(tree, @data_hash_b)

    assert {:ok, {:absent, tree}} ==
             SparseMerkleTree.drop(tree, @empty_hash)

    tree
  end

  def tree_depth_3_with_a_structure() do
    tree = tree_depth_3_with_a()

    assert {
             :no_hash,
             :empty,
             {
               :no_hash,
               :empty,
               {
                 :no_hash,
                 {:leaf, @data_hash_a},
                 :empty
               }
             }
           } ==
             tree.root

    tree
  end

  def tree_depth_3_with_a_hashes() do
    tree =
      tree_depth_3_with_a()
      |> SparseMerkleTree.rehash()

    assert {
             <<187, 120, 24, 192, 94, 25, 186, 99, 13, 178, 20, 83, 184, 38,
               229, 47, 71, 60, 210, 92, 138, 116, 30, 155, 185, 55, 133, 254,
               4, 152, 94, 219>>,
             :empty,
             {
               @branch_hash_a1,
               :empty,
               {
                 @branch_hash_a2,
                 {:leaf, @data_hash_a},
                 :empty
               }
             }
           } ==
             tree.root

    tree
  end

  def tree_depth_3_with_a_drop() do
    tree = tree_depth_3_with_a()

    assert {:ok, {:absent, tree}} ==
             SparseMerkleTree.drop(tree, @data_hash_b)

    assert {:ok, {:absent, tree}} ==
             SparseMerkleTree.drop(tree, @empty_hash)

    assert {:ok, {:dropped, tree}} =
             SparseMerkleTree.drop(tree, @data_hash_a)

    assert :empty == tree.root
    tree
  end

  def tree_depth_3_with_a_rehash_b_structure() do
    tree = tree_depth_3_with_a_rehash_b()

    assert {
             :no_hash,
             {
               :no_hash,
               {
                 :no_hash,
                 :empty,
                 {:leaf, @data_hash_b}
               },
               :empty
             },
             {
               @branch_hash_a1,
               :empty,
               {
                 @branch_hash_a2,
                 {:leaf, @data_hash_a},
                 :empty
               }
             }
           } ==
             tree.root

    tree
  end

  def tree_depth_3_with_a_rehash_b_duplicate_insert() do
    tree = tree_depth_3_with_a_rehash_b()

    assert {:ok, {:present, ^tree}} =
             SparseMerkleTree.put(tree, @data_hash_b)

    tree
  end

  def tree_depth_3_with_a_rehash_b_root_hash() do
    tree = tree_depth_3_with_a_rehash_b()

    assert {:error, %SparseMerkleTree.StaleTreeError{}} ==
             SparseMerkleTree.root_hash(tree)

    tree = SparseMerkleTree.rehash(tree)
    assert {:ok, @root_hash_a_b} == SparseMerkleTree.root_hash(tree)
    tree
  end

  def tree_depth_3_with_a_rehash_b_data_hash_collision() do
    tree = tree_depth_3_with_a_rehash_b()

    # Due to the very shallow depth of the tree, the next value's path
    # collides with the second. This should not happen with default-height
    # trees.
    assert {:error,
            %SparseMerkleTree.CollisionError{
              data_hash: @data_hash_c,
              collision_hash: @data_hash_b
            }} == SparseMerkleTree.put(tree, @data_hash_c)

    tree
  end

  def tree_depth_3_with_a_rehash_b_drop() do
    tree = tree_depth_3_with_a_rehash_b()

    assert {:ok, {:absent, tree}} ==
             SparseMerkleTree.drop(tree, @empty_hash)

    assert {:error,
            %SparseMerkleTree.CollisionError{
              data_hash: @data_hash_c,
              collision_hash: @data_hash_b
            }} == SparseMerkleTree.drop(tree, @data_hash_c)

    assert {:ok, {:dropped, tree}} =
             SparseMerkleTree.drop(tree, @data_hash_a)

    assert {
             :no_hash,
             {
               :no_hash,
               {
                 :no_hash,
                 :empty,
                 {:leaf, @data_hash_b}
               },
               :empty
             },
             :empty
           } ==
             tree.root

    assert {:ok, {:dropped, tree}} =
             SparseMerkleTree.drop(tree, @data_hash_b)

    assert :empty == tree.root
    tree
  end

  def tree_depth_3_with_a_rehash_b_proofs() do
    tree = tree_depth_3_with_a_rehash_b()

    assert {:error, %SparseMerkleTree.StaleTreeError{}} ==
             SparseMerkleTree.proof(tree, @data_hash_a)

    tree
  end

  def tree_depth_3_with_a_b_rehash_structure() do
    tree = tree_depth_3_with_a_b_rehash()

    assert {
             @root_hash_a_b,
             {
               @branch_hash_b1,
               {
                 @branch_hash_b2,
                 :empty,
                 {:leaf, @data_hash_b}
               },
               :empty
             },
             {
               @branch_hash_a1,
               :empty,
               {
                 @branch_hash_a2,
                 {:leaf, @data_hash_a},
                 :empty
               }
             }
           } ==
             tree.root

    tree
  end

  def tree_depth_3_with_a_b_rehash_root_hash() do
    tree = tree_depth_3_with_a_b_rehash()
    assert {:ok, @root_hash_a_b} == SparseMerkleTree.root_hash(tree)
    tree
  end

  def tree_depth_3_with_a_b_rehash_proofs() do
    tree = tree_depth_3_with_a_b_rehash()
    root_hash = SparseMerkleTree.root_hash!(tree)
    valid_proof? = &SparseMerkleTree.valid_proof?(&1, &2, root_hash)

    a_proof = {:present, [@branch_hash_b1, @empty_hash, @empty_hash]}
    b_proof = {:present, [@branch_hash_a1, @empty_hash, @empty_hash]}
    h_proof = {:absent, [@branch_hash_b1, @branch_hash_a2]}
    empty_proof = {:absent, [@branch_hash_a1, @empty_hash, @data_hash_b]}

    assert a_proof == SparseMerkleTree.proof!(tree, @data_hash_a)
    assert b_proof == SparseMerkleTree.proof!(tree, @data_hash_b)
    assert h_proof == SparseMerkleTree.proof!(tree, @data_hash_h)
    assert empty_proof == SparseMerkleTree.proof!(tree, @empty_hash)

    assert valid_proof?.(@data_hash_a, a_proof)
    assert valid_proof?.(@data_hash_b, b_proof)
    assert valid_proof?.(@data_hash_h, h_proof)
    assert valid_proof?.(@empty_hash, empty_proof)

    refute valid_proof?.(@data_hash_a, b_proof)
    refute valid_proof?.(@data_hash_b, a_proof)
    refute valid_proof?.(@empty_hash, a_proof)
    refute valid_proof?.(@data_hash_a, empty_proof)
    refute valid_proof?.(@data_hash_c, b_proof)

    # Bad Input
    refute valid_proof?.(@data_hash_a, {:present, []})
    refute valid_proof?.(@data_hash_a, {:absent, []})
    refute valid_proof?.(@data_hash_a, {:present, ["a"]})

    assert {:error,
            %SparseMerkleTree.CollisionError{
              data_hash: @data_hash_c,
              collision_hash: @data_hash_b
            }} ==
             SparseMerkleTree.proof(tree, @data_hash_c)

    tree
  end
end
