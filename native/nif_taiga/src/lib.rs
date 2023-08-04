extern crate core;

#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

rustler::init!(
    "Elixir.Taiga.Native",
    [
        add
    ]
);
