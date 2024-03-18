use array::ArrayTrait;

fn main(index_a: u32, array_a: Array<u32>, index_b: u32, array_b: Array<u32>) -> u32 {
    *array_a.at(index_a) + *array_b.at(index_b)
}
