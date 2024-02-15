
Groth16 prover and verifier
---------------------------

This is a Groth16 SNARK prover and verifier written in Haskell, implemented using 
the "zikkurat" ZK proof library suite. 
Groth16 is a [SNARK](https://en.wikipedia.org/wiki/Non-interactive_zero-knowledge_proof) 
protocol named after a [2016 paper by Jens Groth](https://eprint.iacr.org/2016/260).

This implementation is compatible with the [`circom`](https://docs.circom.io/) + 
[`snarkjs`](https://github.com/iden3/snarkjs) ecosystem.

### TODO

- [ ] make a CLI interface
- [ ] JSON export/import of proofs and public IO
- [ ] multithreading support 
- [ ] load `.r1cs` files 
- [ ] generate fake circuit-specific setup ourselves
- [ ] compare `.r1cs` circuit to the "coeffs" section of `.zkey`
- [ ] add Groth16 PDF notes

### Metadata

copyright: (c) 2024 Faulhorn Labs  
author: Balazs Komuves  
license: MIT or Apache-2.0 (at your choice)  

### Links

See also [`nim-groth16`](https://github.com/codex-storage/nim-groth16) for 
a [Nim](https://nim-lang.org/) language implementation.