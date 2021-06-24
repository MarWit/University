use std::io::{Read, Write};

use ::sequitur::Sequitur;

use anyhow::{Context, Result};

fn main() -> Result<()> {
    let args = std::env::args().collect::<Vec<String>>();
    let args_ref = args.iter().skip(1).map(String::as_str).collect::<Vec<_>>();

    let (file_in_name, file_out_name, compression);

    match &args_ref[..] {
        ["-d", input] => {
            file_in_name = (*input).to_string();
            file_out_name = format!("{}.raw", file_in_name);
            compression = false;
        }
        ["-d", input, output] => {
            file_in_name = (*input).to_string();
            file_out_name = (*output).to_string();
            compression = false;
        }
        [input] => {
            file_in_name = (*input).to_string();
            file_out_name = format!("{}.seq", file_in_name);
            compression = true;
        }
        [input, output] => {
            file_in_name = (*input).to_string();
            file_out_name = (*output).to_string();
            compression = true;
        }
        _ => {
            println!(
                "\
            Usage: sequitur [-d] <input> [<output>]\n\t<input>  - name of the input \
                 file\n\t<output> - name of the output file (def. <input>.seq for compression, \
                 <input>.raw for decompression)\n\t-d       - decompress input file"
            );
            return Ok(());
        }
    }

    if compression {
        let mut file_in = std::fs::File::open(file_in_name)?;
        let mut buffer = vec![];
        file_in.read_to_end(&mut buffer).unwrap();

        let mut sequitur = Sequitur::<u8>::new();
        for c in buffer {
            sequitur.put(c);
        }
        let mut file_out =
            std::fs::File::create(file_out_name).context("Could not create output file")?;
        sequitur
            .compress(&mut file_out)
            .context("Could not compress given input file")?;
    } else {
        let mut file_in =
            std::fs::File::open(file_in_name).context("Could not open give input file")?;

        let sequitur =
            Sequitur::<u8>::decompress(&mut file_in).context("Could not decompress file")?;
        let mut file_out =
            std::fs::File::create(file_out_name).context("Could not create output file")?;

        file_out.write_all(&sequitur.rebuild())?;
    }

    Ok(())
}
