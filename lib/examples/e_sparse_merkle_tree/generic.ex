defmodule Examples.ESparseMerkleTree.Generic do
  defmacro __using__(opts) do
    implementation = Keyword.fetch!(opts, :implementation)

    quote do
      import SparseMerkleTree
      import SparseMerkleTree.Proof

      def empty_tree() do
        tree = unquote(implementation).new()

        <<40, 126, 172, 2, 225, 203, 200, 220, 129, 46, 78, 65, 80, 109,
          175, 176, 220, 208, 172, 231, 241, 135, 64, 235, 248, 191,
          133, 194, 228, 47, 78, 83>> = root(tree)

        tree
      end

      def abc_tree() do
        tree = empty_tree() |> insert("abc")

        <<22, 140, 227, 38, 13, 82, 190, 59, 111, 69, 53, 251, 39, 47,
          187, 80, 33, 4, 53, 28, 121, 103, 191, 79, 160, 55, 201, 178,
          246, 78, 164, 105>> = root(tree)

        tree
      end

      def def_tree() do
        tree = empty_tree() |> insert("def")

        <<67, 49, 10, 77, 155, 234, 33, 71, 100, 44, 159, 4, 218, 38,
          188, 253, 213, 61, 148, 185, 45, 50, 39, 15, 194, 46, 249, 55,
          207, 50, 244, 85>> = root(tree)

        tree
      end

      def abc_def_tree() do
        tree = empty_tree() |> insert("abc") |> insert("def")

        <<255, 6, 238, 194, 117, 166, 105, 95, 156, 157, 91, 95, 240,
          201, 63, 210, 195, 205, 206, 113, 250, 76, 5, 161, 2, 189, 26,
          46, 180, 206, 128, 150>> = root(tree)

        tree
      end

      def def_abc_tree() do
        tree = empty_tree() |> insert("def") |> insert("abc")

        <<255, 6, 238, 194, 117, 166, 105, 95, 156, 157, 91, 95, 240,
          201, 63, 210, 195, 205, 206, 113, 250, 76, 5, 161, 2, 189, 26,
          46, 180, 206, 128, 150>> = root(tree)

        tree
      end

      def abc_def_equals_def_abc() do
        abc_def = abc_def_tree()
        def_abc = def_abc_tree()
        abc_def_root = root(abc_def)
        ^abc_def_root = root(def_abc)
        abc_def
      end

      def safe_double_insert() do
        abc = abc_tree()
        abc_root = root(abc)
        ^abc_root = root(insert(abc, "abc"))
      end

      def prove_abc_present_in_abc() do
        {:ok, _} = prove_present(abc_tree(), "abc")
      end

      def verify_abc_present_in_abc() do
        {:ok, proof} = prove_abc_present_in_abc()
        true = verify_present(proof, root(abc_tree()), "abc")
        proof
      end

      def prove_abc_present_in_abc_def() do
        {:ok, _} = prove_present(abc_def_tree(), "abc")
      end

      def verify_abc_present_in_abc_def() do
        {:ok, proof} = prove_abc_present_in_abc_def()
        true = verify_present(proof, root(abc_def_tree()), "abc")
        proof
      end

      def prove_abc_absent_in_def() do
        {:ok, _} = prove_absent(def_tree(), "abc")
      end

      def verify_abc_absent_in_def() do
        {:ok, proof} = prove_abc_absent_in_def()
        true = verify_absent(proof, root(def_tree()), "abc")
        proof
      end

      def prove_abc_absent_in_empty() do
        {:ok, _} = prove_absent(empty_tree(), "abc")
      end

      def verify_abc_absent_in_empty() do
        {:ok, proof} = prove_abc_absent_in_empty()
        true = verify_absent(proof, root(empty_tree()), "abc")
        proof
      end

      def dont_prove_abc_present_in_empty() do
        :error = prove_present(empty_tree(), "abc")
      end

      def dont_prove_abc_present_in_def() do
        :error = prove_present(def_tree(), "abc")
      end

      def dont_prove_abc_absent_in_abc_def() do
        :error = prove_absent(abc_def_tree(), "abc")
      end

      def dont_prove_abc_absent_in_abc() do
        :error = prove_absent(abc_tree(), "abc")
      end

      def big_tree(exponent \\ 8) do
        for n <- 1..(2 ** exponent), reduce: empty_tree() do
          tree ->
            tree |> insert(Integer.to_string(n))
        end
      end

      # note: don't reduce the big tree below 2^7!
      def prove_123_present_in_big_tree() do
        {:ok, _} = prove_present(big_tree(), "123")
      end

      def verify_123_present_in_big_tree() do
        {:ok, proof} = prove_123_present_in_big_tree()
        true = verify_present(proof, root(big_tree()), "123")
        proof
      end

      # or increase it above 2^191, lol
      def prove_big_number_absent_in_big_tree() do
        {:ok, _} = prove_absent(big_tree(), Integer.to_string(2 ** 192))
      end

      def verify_big_number_absent_in_big_tree() do
        {:ok, proof} = prove_big_number_absent_in_big_tree()

        true =
          verify_absent(
            proof,
            root(big_tree()),
            Integer.to_string(2 ** 192)
          )

        proof
      end
    end
  end
end
