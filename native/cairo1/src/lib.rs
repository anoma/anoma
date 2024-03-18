use cairo1_run::cairo_run::{cairo_run_program, Cairo1RunConfig, FuncArg};
use cairo_lang_sierra::ProgramParser;
use cairo_vm::Felt252;

#[rustler::nif(schedule = "DirtyCpu")]
fn cairo1_vm_runner(sierra_program_file: String, inputs: String) -> (Vec<u8>, Vec<u8>) {
    let sierra_code = std::fs::read_to_string(sierra_program_file).unwrap();
    let program = ProgramParser::new().parse(&sierra_code).unwrap();
    let args = process_args(&inputs).unwrap();
    let cairo_run_config = Cairo1RunConfig {
        proof_mode: true,
        relocate_mem: true,
        layout: "all_cairo", // add layouts if needed
        trace_enabled: true,
        args: &args,
        finalize_builtins: false, // TODO: figure out the functionality of the flag
        append_return_values: true,
    };

    let (runner, _vm, _return_values) = cairo_run_program(&program, cairo_run_config).unwrap();

    let trace = {
        let relocated_trace = runner.relocated_trace.unwrap();
        let mut output: Vec<u8> = Vec::with_capacity(3 * 1024 * 1024);
        for entry in relocated_trace.iter() {
            output.extend_from_slice(&(entry.ap as u64).to_le_bytes());
            output.extend_from_slice(&(entry.fp as u64).to_le_bytes());
            output.extend_from_slice(&(entry.pc as u64).to_le_bytes());
        }
        output
    };

    let memory = {
        let mut output: Vec<u8> = Vec::with_capacity(1024 * 1024);
        for (i, entry) in runner.relocated_memory.iter().enumerate() {
            match entry {
                None => continue,
                Some(unwrapped_memory_cell) => {
                    output.extend_from_slice(&(i as u64).to_le_bytes());
                    output.extend_from_slice(&unwrapped_memory_cell.to_bytes_le());
                }
            }
        }
        output
    };

    (trace, memory)
}

// Deserialize the inputs.
// TODO: revise it when we have a standard input format
fn process_args(value: &str) -> Result<Vec<FuncArg>, String> {
    let mut args = Vec::new();
    let mut input = value.split(' ');
    while let Some(value) = input.next() {
        // First argument in an array
        if value.starts_with('[') {
            let mut array_arg =
                vec![Felt252::from_dec_str(value.strip_prefix('[').unwrap()).unwrap()];
            // Process following args in array
            let mut array_end = false;
            while !array_end {
                if let Some(value) = input.next() {
                    // Last arg in array
                    if value.ends_with(']') {
                        array_arg
                            .push(Felt252::from_dec_str(value.strip_suffix(']').unwrap()).unwrap());
                        array_end = true;
                    } else {
                        array_arg.push(Felt252::from_dec_str(value).unwrap())
                    }
                }
            }
            // Finalize array
            args.push(FuncArg::Array(array_arg))
        } else {
            // Single argument
            args.push(FuncArg::Single(Felt252::from_dec_str(value).unwrap()))
        }
    }
    Ok(args)
}

rustler::init!("Elixir.Anoma.Cairo1", [cairo1_vm_runner]);
