# PSEUDOCODE CIE 9618

An implementation of the CIE 9618 pseudocode spec, designed with [this reference](https://pastpapers.co/cie/A-Level/Computer%20Science%20(for%20first%20examination%20in%202021)%20(9618)/Syllabus%20&%20Specimen/9618_y21_sg.pdf) in mind

## Usage

To run an example

```bash
cargo run --example factorial
```

To use as a standalone binary(precompiled binaries are only available for windows atm)

```powershell
# The file extension does not matter
pseudocode.exe path/to/your/code.pseudo
```



### When compiling from source

To run with the parse tree and abstract syntax tree shown run in dev mode

```bash
cargo run -- "example.pseudo"
```

To silence the output and only show the program output run with release

```bash
cargo run --release - "example.pseudo"
```

## Pseudocode syntax/reference

Please reference the [wiki](../../wiki)
